library(shiny)

shinyUI(pageWithSidebar(
    headerPanel("Segment Detection in Nike+ Pedometer Run Data"),
    sidebarPanel(
        h3('Plot Options'),
        selectInput('set_num', 'Set', c("2007-07-09", "2007-07-11", "2007-07-13", "2007-07-19")),
        sliderInput('conf_level', 'Confidence %', min=80, max=99, step=1, value=95),
        sliderInput('min_points', 'Minimum Segment Length', min=1, max=7, step=1, value=2),
        sliderInput('window_size', 'Window Size', min=5, max=18, step=1, value=12),
        hr(),
        h3('Author: Proshanta Sarkar'),
        p('Proshanta is an Application Developer by day, a runner by night, and an avid learner on weekends.  This app is the final project for the Data Science specialization offered by Coursera and the Johns Hopkins Bloomberg School of Public Health.'),
        p('The datasets here are from runs Rick did in Nov 2007.')
    ),
    mainPanel(
        h3('Run Segments'),
        plotOutput('chart'),
        h3('Explanation'),
        p('Nike+ run data is stored as a list of total distance at 10-second intervals for the duration of the run.  This can be massaged to find run speed and time, represented by the blue dots above.  As you can see, this data can be quite noisy, and clearly has bias toward specific increments.'),
        p('When running long distances, many runners, myself included, get to a stride (pace) and stay relatively constant for a while.  When training, runners may include bursts of acceleration or deceleration.'),
        p('Many running apps will present this data in a smoothed spline.  While pretty, this is possibly the least accurate way to represent a runner\'s pace, which is seldom so smooth and continuous.'),
        p('This app uses a naive, brute force feature detection algorithm by comparing a window of time before and after each point.  Outliers are rejected, discontinuities are detected, and segments are discovered.  You can see how adjusting the confidence intervals, segment length, and window size can affect the segment detection.')
    )
))