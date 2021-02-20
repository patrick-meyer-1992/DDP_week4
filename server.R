library(shiny)
library(stringr)
library(aweek)
library(plotly)
library(tidyr)
library(dplyr)
library(lubridate)

server <- function(input, output) {
   netflix = reactive({
        
        # input$file1 will be NULL initially. After the user selects
        # and uploads a file, it will be a data frame with 'name',
        # 'size', 'type', and 'datapath' columns. The 'datapath'
        # column will contain the local filenames where the data can
        # be found.
    
        inFile <- input$data
        
        if (is.null(inFile) && !input$default)
            return(NULL)
        
        if(input$default){
            netflix = read.csv("NetflixViewingHistory_Patrick.csv", header = TRUE) 
        }else{
            netflix = read.csv(inFile$datapath)
        }
        
        
        netflix$Title <- sapply(netflix$Title, gsub, pattern="Ã¼", replacement="ü")
        netflix$Title <- sapply(netflix$Title, gsub, pattern="Ã¶", replacement="ö")
        netflix$Title <- sapply(netflix$Title, gsub, pattern="Ã¤", replacement="ä")
        netflix$Title <- sapply(netflix$Title, gsub, pattern="â€“", replacement="-")
        netflix$Title <- sapply(netflix$Title, gsub, pattern="Ã©", replacement="é")
        netflix$Title <- sapply(netflix$Title, gsub, pattern="â€™", replacement="'")
        netflix$Title <- sapply(netflix$Title, gsub, pattern="ÃŸ", replacement="ß")
        netflix$Title <- sapply(netflix$Title, gsub, pattern="â€¦", replacement="...")
        netflix$Title <- sapply(netflix$Title, gsub, pattern="â€“", replacement="-")
        
        #Break the title into 3 seperate columns with "Title of show", "Season", "Title of episode"
        netflix = separate(netflix, col = Title, into = c("Title", "Season", "Episode"), sep = ': ')
        
        #Exclude rows which are not in the proper format and are therefore likely not series
        netflix = netflix[-which(is.na(netflix$Episode)),]
        
        #Break up the Date-Column in separate columns
        netflix$days = substr(netflix$Date, start = 1, stop = 2)
        netflix$months = substr(netflix$Date, start = 4, stop = 5)
        netflix$years = paste("20", substr(netflix$Date, start = 7, stop = 8), sep = "")
        netflix$Date = as.Date(paste(netflix$years, "-", netflix$months, "-", netflix$days, sep = ""))
        netflix$weekday = wday(netflix$Date, label = FALSE, week_start = 1)
        #Order by Date
        netflix = netflix[order(netflix$Date),]

        netflix
    })
   

    output$topten <- renderPlotly({
        if (is.null(netflix()))
            return(NULL)
        
        netflix = netflix()

        startDate = netflix$Date[1]
        endDate = netflix$Date[nrow(netflix)]
        
        netflixTitleCounts = count(netflix, Title)
        netflixTitleCounts = netflixTitleCounts[order(netflixTitleCounts$n, decreasing = TRUE),]

        #Turn the title into a factor with defined order, so plot_ly can recognize this order instead of sorting alphabetically
        netflixTitleCounts$Title = factor(netflixTitleCounts$Title, levels = netflixTitleCounts$Title[order(netflixTitleCounts$n, decreasing = FALSE)])

        #Plot the ten most watched shows
        plot_ly(data = netflixTitleCounts[1:10,], x = ~n, y = ~Title, type = "bar",
                         orientation = "h", marker = list(color = "rgba(229,9,20,1)")) %>%
            layout(yaxis = list(title = list(standoff = 1), color = "white"),
                   xaxis = list(color = "white", title = "Total Number of Views"),
                   title = list(text = paste("Your 10 Most Watched Shows on Netflix From", 
                                             startDate, "To", endDate, sep = " "),
                                font = list(color = "white", size = 20)),
                   margin = list(t = 50),
                   plot_bgcolor = "rgba(0,0,0,1)",
                   paper_bgcolor = "rgba(0,0,0,1)")
    })
    
    output$weeklyDistribution = renderPlotly({
        if (is.null(netflix()))
             return(NULL)
        
        netflix = netflix()
        
        weeklyDistribution = count(netflix, weekday)
        weeklyDistribution$weekday = factor(x = weeklyDistribution$weekday,
                                               labels = c("Mon", "Tue", "Wed", "Thu", "Fri", "Sat", "Sun"))
        weeklyDistribution$ratio = round((weeklyDistribution$n/sum(weeklyDistribution$n) * 100), 2)
        
        plot_ly(data = weeklyDistribution, labels = ~weekday, values = ~n, type = "pie", 
                sort = FALSE, 
                direction = "clockwise",
                textinfo = "label+percent", 
                insidetextfont = list(color = "#FFFFFF"), 
                marker = list(colors = rep("#E50914", 7), 
                              line = list(color = "#FFFFFF", width = 1)), 
                showlegend = FALSE) %>%
            layout(title = list(text ="Your Views Per Weekday on Netflix", 
                                font = list(color = "white")),
                   margin = list(t = 50),
                   plot_bgcolor = "rgba(0,0,0,1)", 
                   paper_bgcolor = "rgba(0,0,0,1)")

    })
    
    output$monthlyDistribution = renderPlotly({
        if (is.null(netflix()))
            return(NULL)
        
        netflix = netflix()
        
        monthlyDistribution = count(netflix, months)
        monthlyDistribution$months = factor(x = monthlyDistribution$months,
                                            labels = c("Jan", "Feb", "Mar", "Apr", "May", "June", 
                                                       "July","Aug", "Sept", "Oct", "Nov", "Dec"))
        monthlyDistribution$ratio = round((monthlyDistribution$n/sum(monthlyDistribution$n) * 100), 2)
        
        plot_ly(data = monthlyDistribution, labels = ~months, values = ~n, type = "pie", 
                sort = FALSE, 
                direction = "clockwise",
                textinfo = "label+percent", 
                insidetextfont = list(color = "#FFFFFF"), 
                outsidetextfont = list(color = "#FFFFFF"),
                marker = list(colors = rep("#E50914", 12), 
                              line = list(color = "#FFFFFF", width = 1)), 
                showlegend = FALSE) %>%
            layout(title = list(text ="Your Views Per Month on Netflix", 
                                font = list(color = "white")),
                   margin = list(t = 50),
                   plot_bgcolor = "rgba(0,0,0,1)", 
                   paper_bgcolor = "rgba(0,0,0,1)")
    })
    
    output$dailyDistribution = renderPlotly({
        
        if (is.null(netflix()))
            return(NULL)
        
        netflix = netflix()
        startDate = netflix$Date[1]
        endDate = netflix$Date[nrow(netflix)]
        
        dailyDistribution = count(netflix, Date)
        
        plot_ly(data = dailyDistribution, x = ~Date, y = ~n, type = "bar", 
                              marker = list(color = "rgba(229,9,20,1)"))  %>%
            layout(yaxis = list(title = "Views per Day", color = "white"), 
                   xaxis = list(title = "Date", color = "white"),
                   title = list(text = paste("Your Views Per Day on Netflix From", startDate, "To", endDate, sep = " "), 
                                font = list(color = "white", size = 20)),
                   margin = list(t = 50),
                   plot_bgcolor = "rgba(0,0,0,1)", 
                   paper_bgcolor = "rgba(0,0,0,1)")
    })
}