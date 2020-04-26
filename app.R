#Project 3
#Aashish Agrawal - aagraw10
#Ivan Madrid - imadri2
#Richard Miramontes - rmiram2
#CS 424

#load libraries
library(shiny)
library(shinydashboard)
library(ggplot2)
library(leaflet)

#read in data from csv 
#(use rawdata and save into a new dataframe for your stuff ex: variableName <- rawdata)
rawdata <- read.csv(file = "movies_data.csv")

#overallData is used for our overall plots
#overallData only has unique titles (so one row for each movie)
overallData <- rawdata
#order by year
overallData <- rawdata[order(rawdata$Year),]
#remove duplicates
overallData <- overallData[!duplicated(overallData["Title"]),]
#make years as factors (for our plots)
overallData$Year <- factor(overallData$Year)

#convert release date to character (so we can pull month from it)
overallData$Release.Date <- as.character(overallData$Release.Date)
#pull month from release date
overallData$Month <- sapply(strsplit(overallData$Release.Date, " "), function(x) {
  if (length(x) == 2) {
    x[1]
  }
  else {
    x[2]
  }
})
#month vector
months = c("January", "February", "March", "April", "May", "June", "July", "August", "September", "October", "November", "December")
#use month vector to factor Month column in right order
overallData$Month <- factor(overallData$Month, levels = months)

#counter column, so we can get averages
overallData["Counter"] = 1

#get the sums of each year unique movie title occurences
sums <- aggregate(overallData[, "Counter"], list(overallData$Year),sum)
#get average number of films released per year
avgPerYear <- sum(sums["x"]) / nrow(sums)

#get the sums of each year + month unique movie title occurences
sums <- aggregate(overallData[, "Counter"], list(overallData$Year, overallData$Month),sum)
#get average number of films released per month
avgPerMonth <- sum(sums["x"]) / nrow(sums)

#get average movie runtime
avgByRuntime <- mean(overallData$Running.Time)

#dataframe for keywords
keywords <- as.data.frame(table(rawdata$Keyword))
names(keywords)[names(keywords) == "Var1"] <- "Keyword"
keywords <- keywords[rev(order(keywords$Freq)),]
row.names(keywords) <- NULL


# begin ui
ui <- dashboardPage(
  dashboardHeader(title = "CS 424 Project 3"),
  dashboardSidebar(
    width = 300,
    sidebarMenu(
      menuItem("Dashboard", tabName ="dashboard", icon = icon("dashboard")),
      menuItem("About",tabName = "til", startExpanded = F, icon = icon("question"),
        h4("Coded By:"), p("Ivan M., Richard M., Aashish A."), 
        h4("Libraries:"), p("shiny,shinydashboard,ggplot2"),
        h4("Data Source:"), p("IMDB"), tags$p(tags$a(href = "ftp://ftp.fu-berlin.de/pub/misc/movies/database/frozendata/", "fu-berlin.de"))
      ),
      menuItem("Average Films Released Per Year", tabName = "blanks"),
      menuItem(infoBoxOutput("avgYear")),
      menuItem("", tabName = "blanks"),
      menuItem("", tabName = "blanks"),
      menuItem("", tabName = "blanks"),
      menuItem("", tabName = "blanks"),
      menuItem("Average Films Released Per Month", tabName = "blanks"),
      menuItem(infoBoxOutput("avgMonth")),
      menuItem("", tabName = "blanks"),
      menuItem("", tabName = "blanks"),
      menuItem("", tabName = "blanks"),
      menuItem("", tabName = "blanks"),
      menuItem("Average Runtime of Films", tabName = "blanks"),
      menuItem(infoBoxOutput("avgRuntime"))
    )
  ),
  dashboardBody(
    fluidRow(
      box(width = 12,
        mainPanel(width = 12, 
          tabsetPanel(
            tabPanel("Movies per Year",      
              ##chart
              box( width = 12, status = "primary", solidHeader = TRUE, plotOutput("MoviesPerYearChart", height = 360)),
              ##table
              box( width = 12, status = "primary", solidHeader = TRUE, DT::dataTableOutput("MoviesPerYearTable", height = 360))
            ),
            tabPanel("Movies per Month",      
              ##chart
              box( width = 12, status = "primary", solidHeader = TRUE, plotOutput("MoviesPerMonthChart", height = 360)),
              ##table
              box( width = 12, status = "primary", solidHeader = TRUE, DT::dataTableOutput("MoviesPerMonthTable", height = 360))
            ),
            tabPanel("Movies per Runtime",
              ##chart
              box( width = 12, status = "primary", solidHeader = TRUE, plotOutput("MoviesPerRuntimeChart", height = 360)),
              ##table
              box( width = 12, status = "primary", solidHeader = TRUE, DT::dataTableOutput("MoviesPerRunTimeTable", height = 360))
            ),
            tabPanel("Movies per Genre",
              ##chart
              box( width = 12, status = "primary", solidHeader = TRUE, plotOutput("MoviesPerGenreChart", height = 360)),
              ##table
              box( width = 12, status = "primary", solidHeader = TRUE, DT::dataTableOutput("MoviesPerGenreTable", height = 360))
            ),
            tabPanel("Movies per Certificate",
              ##chart
              box( width = 12, status = "primary", solidHeader = TRUE, plotOutput("MoviesPerCertificateChart", height = 360)),
              ##table
              box( width = 12, status = "primary", solidHeader = TRUE, DT::dataTableOutput("MoviesPerCertificateTable", height = 360))
            ),
            tabPanel("Movie Top N Keywords",
              ##slider picker for top n
              box(
              sliderInput("pickFilter", "Pick N", 1, 50, 10)),
              ##chart
              box( width = 12, status = "primary", solidHeader = TRUE, plotOutput("MovieKeywordsChart", height = 420)),
              ##table
              box( width = 12, status = "primary", solidHeader = TRUE, DT::dataTableOutput("MovieKeywordsTable", height = 360))
            ),
            tabPanel("Runtime of each Genre",      
              ##chart
              box( width = 12, status = "primary", solidHeader = TRUE, plotOutput("RuntimeGenrePlot", height = 360)),
              ##table
              box( width = 12, status = "primary", solidHeader = TRUE, DT::dataTableOutput("RuntimeGenreTable", height = 360))
            )
          ) # end tabsetpanel
        ) # end mainpanel
      ) # end box
    )#end fluidrow
  )##end dashboardBody
)##end dashboardPage

#begin server
server <- function(input, output) {
  
  output$avgYear <- renderInfoBox({
    infoBox(
      "Average Films Released Per Year",
      paste0(sprintf("%0.2f", avgPerYear), " Films"),
      icon = icon("calendar"),
    )
  })
  
  output$avgMonth <- renderInfoBox({
    infoBox(
      "Average Films Released Per Month",
      paste0(sprintf("%0.2f", avgPerMonth), " Films"),
      icon = icon("calendar-alt")
    )
  })
  
  output$avgRuntime <- renderInfoBox({
    infoBox(
      "Average Runtime of Films",
      paste0(sprintf("%0.2f", avgByRuntime), " Minutes"),
      icon = icon("clock")
    )
  })
  
  filteredKeywords <- reactive({
    #new dataframe consisting of top n results from dataframe
    filteredKeywords <- keywords[1:input$pickFilter,]
    rownames(filteredKeywords) <- NULL
    filteredKeywords
  })
  
  output$MoviesPerYearChart <- renderPlot({
    #amount of movies per year
    ggplot(overallData) +
      aes(x = overallData$Year) +
      geom_bar( fill="tomato3") +
      labs(title="Number of Movies Released per Year",caption="source: Year") +
      labs(x = "Year", y = "Count") +
      theme(axis.text.x = element_text(angle=65, vjust=0.6))
  })
  
  output$MoviesPerYearTable = DT::renderDataTable({
    #table for movies per year,full data unique titles set(no duplicates of same movie)
    movieYearTable <- as.data.frame(table(overallData$Year))
    names(movieYearTable)[names(movieYearTable) == "Var1"] <- "Year"
    movieYearTable
  })
  
  output$MoviesPerMonthChart <- renderPlot({
    #amount of movies per year
    ggplot(overallData) +
      aes(x = overallData$Month) +
      geom_bar( fill="tomato3") +
      labs(title="Number of Movies Released per Month",caption="source: Month") +
      labs(x = "Month", y = "Count") +
      theme(axis.text.x = element_text(angle=65, vjust=0.6))
  })
  
  output$MoviesPerMonthTable = DT::renderDataTable({
    #table for movies per month,full data unique titles set(no duplicates of same movie)
    movieMonthTable <- as.data.frame(table(overallData$Month))
    names(movieMonthTable)[names(movieMonthTable) == "Var1"] <- "Month"
    movieMonthTable
  })
  
  output$MoviesPerRuntimeChart <- renderPlot({
    #amount of movies per runtime
    ggplot(overallData) +
      aes(x = factor(overallData$Running.Time)) +
      geom_bar( fill="tomato3") +
      labs(title="Distribution of Movie Runtimes",caption="source: Running Time") +
      labs(x = "Runtime (in minutes)", y = "Count") +
      theme(axis.text.x = element_text(angle=90, vjust=0.6))
  })
  
  output$MoviesPerRunTimeTable = DT::renderDataTable({
    #table for movies per runtime
    runtimeTable <- as.data.frame(table(overallData$Running.Time))
    names(runtimeTable)[names(runtimeTable) == "Var1"] <- "Runtime (in minutes)"
    runtimeTable <- runtimeTable[rev(order(runtimeTable$Freq)),]
    row.names(runtimeTable) <- NULL
    runtimeTable
  })
  
  output$MoviesPerGenreChart <- renderPlot({
    #amount of movies per Genre
    ggplot(overallData, aes(x=factor(Genres))) +
      geom_bar( width=.5, fill="tomato3") +
      labs(title="Number of Movies per Genre", caption="source: Genres") +
      labs(x = "Genre", y = "Count") +
      theme(axis.text.x = element_text(angle=65, vjust=0.6))
  })
  
  output$MoviesPerGenreTable = DT::renderDataTable({
    #table for genre
    genreTable <- as.data.frame(table(overallData$Genres))
    names(genreTable)[names(genreTable) == "Var1"] <- "Genres"
    genreTable
  })
  
  output$MoviesPerCertificateChart <- renderPlot({
    #amount of movies per Certificate
    ggplot(overallData, aes(x=factor(Certificate))) +
      geom_bar( width=.5, fill="tomato3") +
      labs(title="Number of Movies per Certificate", caption="source: Certificate") +
      labs(x = "Certificate", y = "Count") +
      theme(axis.text.x = element_text(angle=65, vjust=0.6))
  })
  
  output$MoviesPerCertificateTable = DT::renderDataTable({
    #table for movies per Certificate
    certificateTable <- as.data.frame(table(overallData$Certificate))
    names(certificateTable)[names(certificateTable) == "Var1"] <- "Certificate"
    certificateTable
  })
  
  output$MovieKeywordsChart <- renderPlot({
    #plot for the keyword
    ggplot(filteredKeywords(), aes(x= Keyword, y = factor(Freq))) +
      geom_bar(position="dodge", stat="identity", width=.5, fill="tomato3") +
      labs(title="Number of Movies per Keyword", caption="source: Keyword") +
      labs(x = "Keyword", y = "Count") +
      theme(axis.text.x = element_text(angle=90, vjust=0.6)) + 
      coord_flip()
  })
  
  output$MovieKeywordsTable = DT::renderDataTable({
    #table for movies per keywords
    filteredKeywords()
  })
  
  output$RuntimeGenrePlot <- renderPlot({
    ##shows the min and max Runtime of each Genre
    ggplot(overallData, aes(x=Genres, y=Running.Time, group = Running.Time)) +
      geom_point(col="tomato2", size=3) +   # Draw points
      geom_segment(
        aes(x=Genres, xend=Genres, y=min(Running.Time), yend=max(Running.Time)), 
        linetype="dashed", size=0.1
      ) + 
      labs(title="Genre & Run Time", caption="Genre & Run Time") +  
      labs(x = "Genre", y = "Runtime (in minutes)") +
      coord_flip()
  })
  
  output$RuntimeGenreTable = DT::renderDataTable({
    #table for runtime/genre
    runtimeGenresTable <- as.data.frame(table(overallData$Genres,overallData$Running.Time))
    names(runtimeGenresTable)[names(runtimeGenresTable) == "Var1"] <- "Genres"
    names(runtimeGenresTable)[names(runtimeGenresTable) == "Var2"] <- "RunTime"
    runtimeGenresTable <- runtimeGenresTable[rev(order(runtimeGenresTable$Freq)),]
    rownames(runtimeGenresTable) <- NULL
    runtimeGenresTable
  })
  
  
}#end ofserver

shinyApp(ui = ui, server = server)