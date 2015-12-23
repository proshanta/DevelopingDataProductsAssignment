source('nikeplus.R')

files <- c("2007-07-09 21;39;48.xml", "2007-07-11 20;59;57.xml", "2007-07-13 20;38;01.xml", "2007-07-19 20;14;01.xml")
sets <- list()
for (file in files) {
    sets[[file]] <- read.nikePlus(file.path(XML_DIR, file))@intervals
}

shinyServer(function(input, output) {
    output$chart <- renderPlot({
        file <- grep(input$set_num, files, value=TRUE)
        if (!length(file)) file <- files[1]
        plotNike(sets[[file]],
                 confLevel=input$conf_level * 0.01,
                 minimumPoints=input$min_points,
                 windowSize=input$window_size,
                 title=file
        )
    })
})