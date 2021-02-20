library(shiny)
library(shinyWidgets)
library(plotly)
shinyUI(fluidPage(
    
    title = "Netflix Analysis",
    setBackgroundColor("black"),
    tags$head(tags$style('body {color:white;}')),
    fluidRow(
        
        column(12,
            titlePanel("A Summary of Your Netflix Viewing Behavior - A coursera.org Course Project"),
            p("Hi and welcome to this small shiny application. It was created as part of the 
            'Developing Data Products'-Class on coursera.org and somewhat inspired by this website:"),
            a("Explore your activity on Netflix with R: How to analyze and visualize your viewing history", 
              href="https://towardsdatascience.com/explore-your-activity-on-netflix-with-r-how-to-analyze-and-visualize-your-viewing-history-e85792410706"),
            br(),
            p("You can upload your Netflix viewing history below and the application will create a short summary for you.
              If you don't know how to download your viewing history from netflix you can find an explanation under the follwing link."),
            a("How to See and Download Your Netflix Viewing History", href = "https://www.makeuseof.com/tag/how-to-download-netflix-viewing-history/"),
            br(),
            p("Once you have downloaded the viewing history you can upload the .csv-file below. Please make sure that it
              is the unaltered version of the .csv since everything else might result in various issues. You might also
              encounter some issues depending on special characters used in your native language."),
            br(),
            p("This application does not save your data beyond it's execution time. If you are not willing or able to
              upload your own viewing history you can simply click the 'use default data instead' checkbox below to use
              a dataset provided by me."),
            fileInput("data", 
                      "Choose CSV File", 
                      multiple = FALSE, 
                      accept = c(
                          "text/csv",
                          "text/comma-separated-values,text/plain",
                          ".csv")
                      ),
            checkboxInput(inputId = "default", label = "Use default data instead", value = FALSE),
            tags$hr()
        ),
        
        
        column(12,
            plotlyOutput("topten"),
            tags$br()
        ),
        column(6,
               plotlyOutput("weeklyDistribution")
               
        ),
        column(6,
               plotlyOutput("monthlyDistribution"),
               tags$br()
        ),
        column(12,
               plotlyOutput("dailyDistribution")
               
        )
    )
))