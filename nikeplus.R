#! /opt/local/bin/Rscript

# install.packages("XML")
library(XML)
library(shiny)
XML_DIR <- "xml"
# time goal "2009-01-26 19;59;38.xml"
# "2007-07-19 20;14;01.xml" "2007-07-11 20;59;57.xml"
# XML_PATH <- file.path(XML_DIR, "2007-07-19 20;14;01.xml")
# data <- xmlToList(xmlParse(XML_PATH))

setClass("nikePlus", representation(
    durationMS = "numeric",
    goalType = "character",
    goalValue = "numeric",
    goalUnit = "character",
    goalKM = "numeric",
    goalMin = "numeric",
    empedID = "character",
    distanceKM = "numeric",
    intervals = "data.frame",
    formatVersion = "numeric",
    time = "POSIXlt",
    calories = "numeric",
    weightKG = "numeric",
    device = "character",
    snapshot = "data.frame",
    snapshotType = "character",
    dataType = "character",
    intervalType = "character",
    intervalUnit = "character",
    intervalValue = "numeric"
    ))

if (!isGeneric("read.nikePlus")) {
    if (is.function("read.nikePlus"))
        fun <- read.nikePlus
    else fun <- function(.Object, ...) standardGeneric("read.nikePlus")
    setGeneric("read.nikePlus", function(.Object, ...) standardGeneric("read.nikePlus"))
}

setMethod("read.nikePlus", "character", function(.Object) {
    data <- xmlToList(xmlParse(.Object))
    miPerKm <- 1.609344
    np <- new("nikePlus")
    np@durationMS <- as.numeric(data$runSummary$duration)
    np@formatVersion <- as.numeric(data$vers)
    np@time <- strptime(sub(":(\\d\\d)$","\\1", data$startTime), "%FT%T%z")
    np@distanceKM <- as.numeric(data$runSummary$distance$text)
    if (data$runSummary$distance$.attrs[["unit"]] == "mi") 
        np@distanceKM <- np@distanceKM * miPerKm
    np@calories <- as.numeric(data$runSummary$calories)
    goal <- if ("type" %in% names(data$goal)) data$goal else data$goal$.attrs
    np@goalType <- goal[["type"]]
    np@goalValue <- as.numeric(goal[["value"]])
    np@goalUnit <- goal[["unit"]]
    if (np@goalType == "Distance")
        np@goalKM <- ifelse(np@goalUnit == "mi", miPerKm, 1.0) * np@goalValue
    if (np@goalType == "Time")
        np@goalMin <- np@goalValue / ifelse(np@goalUnit == "sec", 60.0, 1.0)
    np@empedID <- data$userInfo$empedID
    np@weightKG <- as.numeric(data$userInfo$weight)
    np@device <- data$userInfo$device
    np@snapshot <- data.frame()
    snapshotLen <- length(data$snapShotList)
    for (i in seq(1, snapshotLen)) {
        row <- data$snapShotList[[i]]
        if ("distance" %in% names(row)) {
            np@snapshot <- rbind(np@snapshot, data.frame(
                durationMS = as.numeric(row$duration),
                distance = as.numeric(row$distance),
                pace = as.numeric(row$pace)
            ))
        }
        else if ("snapShotType" %in% names(row)) {
            np@snapshotType <- row[["snapShotType"]]
        }
    }
    np@dataType <- data$extendedDataList$extendedData$.attrs[["dataType"]]
    np@intervalType <- data$extendedDataList$extendedData$.attrs[["intervalType"]]
    np@intervalUnit <- data$extendedDataList$extendedData$.attrs[["intervalUnit"]]
    np@intervalValue <- as.numeric(data$extendedDataList$extendedData$.attrs[["intervalValue"]])
    intervalToMin <- ifelse(np@intervalUnit == "m", 1.0, 60.0)
    distances <- sapply(strsplit(data$extendedDataList$extendedData$text, ",\\s*"), as.numeric)
    count <- length(distances)
    times <- seq(0, count - 1) * np@intervalValue
    np@intervals <- data.frame(
        empedID     = np@empedID,
        startTime   = np@time,
        distanceKM  = distances,
        durationMin = times / intervalToMin,
        diffKM      = distances - c(NA, distances)[1:count]
    )
    np@intervals$speedKPH <- np@intervals$diffKM * intervalToMin * 60.0 / np@intervalValue
    np@intervals$paceMPK <- 60.0 / np@intervals$speedKPH
    np@intervals$time <- np@time + (np@intervals$durationMin * 60.0)
    np@intervals$goalKM <- ifelse(is.null(np@goalKM), NA, np@goalKM)
    np@intervals$goalMin <- ifelse(is.null(np@goalMin), NA, np@goalMin)
    np@intervals$goalPercent <- if (np@goalType == "Distance")
        np@intervals$goalPercent <- distances / np@goalKM
    else if (np@goalType == "Time")
        np@intervals$goalPercent <- np@intervals$durationMin / np@goalMin
    else NA
    np
})

readAllXml <- function() {
    all <- data.frame()
    files <- dir(XML_DIR, "*.xml")
    for (f in files) {
        print(f)
        plus <- read.nikePlus(file.path(XML_DIR, f))
        if (nrow(plus@intervals) > 3) all <- rbind(all, plus@intervals)
    }
    
    write.table(all, "all-data.txt", sep="\t")
}

#plus <- read.nikePlus(XML_PATH)
#tail(plus@intervals)

plotNike <- function(data, windowSize=7, confLevel=0.95, minimumPoints=2, title=NULL, method="lm") {
    int <- subset(data, speedKPH > 0, c("speedKPH","time"))
    speedTimeCols <- c("speedKPH","segmentNum","outlier","time")
    int$outlier <- rep(FALSE, nrow(int))
    int$segmentNum <- rep(NA, nrow(int))
    segmentNum <- 1
    int[1:windowSize, "segmentNum"] <- segmentNum
    all <- seq(from=windowSize + 1, to=nrow(int) - windowSize)
    for (i in all) {
        timeI <- int[i, "time"]
        speedI <- int[i, "speedKPH"]
        prevSegment <- int[i - 1, "segmentNum"]
        beforeI <- subset(int[seq(from=i - windowSize, to=i - 1),], segmentNum==prevSegment & outlier==FALSE)
        # print(data.frame(i=i,prevSegment=prevSegment,nrow=nrow(beforeI)))
        if (nrow(beforeI) >= minimumPoints) {
            afterI <- int[seq(from=i + 1, to=windowSize + i),]
            lmBoth <- lm(speedKPH ~ time, data=rbind(beforeI,afterI))
            ciBoth <- suppressWarnings(as.vector(confint(lmBoth, "(Intercept)", confLevel)))
            if (speedI >= ciBoth[1] & speedI <= ciBoth[2]) {
                lmBefore <- lm(speedKPH ~ time, data=beforeI)
                lmAfter <- lm(speedKPH ~ time, data=afterI)
                ciBefore <- suppressWarnings(as.vector(confint(lmBefore, "(Intercept)", confLevel)))
                ciAfter <- suppressWarnings(as.vector(confint(lmAfter, "(Intercept)", confLevel)))
                if (!((ciBefore[2] >= ciAfter[1] & ciBefore[2] <= ciAfter[2]) | (ciAfter[1] >= ciBefore[1] & ciAfter[1] <= ciBefore[2]))) {
                    segmentNum <- segmentNum + 1
                }
            }
            else {
                int[i, "outlier"] <- TRUE
            }
        }
        int[i, "segmentNum"] <- segmentNum
    }
    int[seq(nrow(int)-windowSize, nrow(int)), "segmentNum"] <- segmentNum
    plot(int$speedKPH ~ int$time, col="steelblue", pch=".", xlab="Wall Clock Time", ylab="Speed (km/h)")
    segments <- aggregate(cbind(end=time) ~ segmentNum, data=int, FUN=max, na.rm=TRUE)
    segments$end < as.POSIXct(segments$end, origin="1970-01-01")
    segments$start <- as.POSIXct(aggregate(time ~ segmentNum, data=int, FUN=min, na.rm=TRUE)$time, origin="1970-01-01")
    for (i in segments$segmentNum) {
        segment <- subset(int, segmentNum == i)
        model <- suppressWarnings(lm(speedKPH ~ time, data=segment, method=method))
        startTime <- segments$start[i]
        endTime <- segments$end[i]
        kph <- suppressWarnings(predict(model, data.frame(time=c(startTime,endTime))))
        lines(c(startTime,endTime), kph, col="red", lwd=3)
    }
    if (!is.null(title)) {
        title(title)
    }
}

#files <- c("2007-07-09 21;39;48.xml", "2007-07-11 20;59;57.xml", "2007-07-13 20;38;01.xml", "2007-07-19 20;14;01.xml")
#files <- c("2007-07-09 21;39;48.xml")
#par(mfrow=c(2,2))
#par(mfrow=c(1,1))
#for (file in files) {
#    plus <- read.nikePlus(file.path(XML_DIR, file))
#    plotNike(plus@intervals, confLevel=0.95, minimumPoints=6, windowSize=9, title=plus@time)
#}

#plus <- read.nikePlus(file.path(XML_DIR, file))
#plotNike(plus@intervals, confLevel=0.95, minimumPoints=6, windowSize=9, title=plus@time, method="treebag")

