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

#####vv--dont worry about this stuff--vv########
##temp only has unique titles (so one row for each movie)
temp <- rawdata[order(rawdata$Year),]
temp <- temp[!duplicated(temp["Title"]),]
#for ploting purposes
temp$Year <- factor(temp$Year)

##temp2 dataframe to use for full data (has all data)
temp2 <- rawdata

#dataframe 'a' for keywords
a <- as.data.frame(table(temp2$Keyword))
names(a)[names(a) == "Var1"] <- "Keyword"
a <- a[rev(order(a$Freq)),]
#new dataframe from 'a' consisting of top50 results from dataframe 'a'
b <- a[1:50,]
rownames(b) <- 1:nrow(b)
#####^^--dont worry about this stuff--^^#######



ui <- dashboardPage(
  dashboardHeader(title = "CS 424 Project 3"),
  dashboardSidebar(
    width = 300,
    sidebarMenu(
      menuItem("Dashboard", tabName ="dashboard", icon = icon("dashboard")),
      menuItem("Page Info",tabName = "til", startExpanded = F, textOutput("il"), textOutput("il2"),textOutput("il3"), textOutput("il4")),
      menuItem("Source: ",tabName = "lit",startExpanded = F, textOutput("li"))
      
      
    )
  ),
  dashboardBody(
    fluidRow(
      box(width = 12,
          mainPanel(width = 12, 
                    tabsetPanel(
                      tabPanel("Movies per Year",      
                               ##chart
                               box( width = 12, status = "primary", solidHeader = TRUE, plotOutput("MoviesPerYearChart", height = 360)   
                               )
                      ),
                      tabPanel("Movies per Runtime",
                               ##chart
                               box( width = 12, status = "primary", solidHeader = TRUE, plotOutput("MoviesPerRuntimeChart", height = 360)   
                               )
                      ),
                      tabPanel("Movies per Genre",
                               ##chart
                               box( width = 12, status = "primary", solidHeader = TRUE, plotOutput("MoviesPerGenreChart", height = 360)   
                               )
                      ),
                      tabPanel("Movies per Certificate",
                               ##chart
                               box( width = 12, status = "primary", solidHeader = TRUE, plotOutput("MoviesPerCertificateChart", height = 360)   
                               )
                      ),
                      tabPanel("Movie KeyWords",
                               ##chart
                               box( width = 12, status = "primary", solidHeader = TRUE, plotOutput("MovieKeyWordsChart", height = 420)   
                               )
                      ),tabPanel("RunTime of each Genre",      
                                 ##chart
                                 box( width = 12, status = "primary", solidHeader = TRUE, plotOutput("RunTimeGenrePlot", height = 360)   
                                 )
                      )
                    )
            )
          )
      
    ),#end fluidrow
    fluidRow(
      box(width = 12,
          mainPanel(width = 12, 
                    tabsetPanel(
                      tabPanel("Movies/Year Table",      
                               ##table
                               box( width = 12, status = "primary", solidHeader = TRUE, DT::dataTableOutput("MoviesPerYearTable", height = 360)   
                               )
                      ),
                      tabPanel("Movies/RunTime Table",      
                               ##table
                               box( width = 12, status = "primary", solidHeader = TRUE, DT::dataTableOutput("MoviesPerRunTimeTable", height = 360)   
                               )
                      ),
                      tabPanel("RunTime/Genre Table",      
                               ##table
                               box( width = 12, status = "primary", solidHeader = TRUE, DT::dataTableOutput("RunTimeGenreTable", height = 360)   
                               )
                      ),
                      tabPanel("Movies/Genre Table",      
                               ##table
                               box( width = 12, status = "primary", solidHeader = TRUE, DT::dataTableOutput("MovieGenreTable", height = 360)   
                               )
                      ),
                      tabPanel("Movies/Certificate Table",      
                               ##table
                               box( width = 12, status = "primary", solidHeader = TRUE, DT::dataTableOutput("MovieCertificateTable", height = 360)   
                               )
                      ),
                      tabPanel("Movies/Keywords Table",      
                               ##table
                               box( width = 12, status = "primary", solidHeader = TRUE, DT::dataTableOutput("MovieKeywordsTable", height = 360)   
                               )
                      )
                    )
                  )
      )##end box
    ) ##end fluidrow
    
    
    
)##end dashboardBody
)##end dashboardPage

server <- function(input, output) {
  
  output$li <- renderText({
    #
    text2 <- as.character("ftp://ftp.fu-berlin.de/pub/misc/movies/database/frozendata/")
    text2
  })
  output$il <- renderText({
    ##about this project
    text1 <- as.character("Coded By: Ivan M., Richard M., Aashish A.")
    text1
  })
  output$il2 <- renderText({
    ##about this project
    text3 <- as.character("Libraries: shiny,shinydashboard,ggplot2")
    text3
  })
  output$il3 <- renderText({
    ##about this project
    text3 <- as.character("Data_Source:")
    text3
  })
  output$il4 <- renderText({
    ##about this project
    text3 <- as.character("IMDB")
    text3
  })
  
  output$MoviesPerYearChart <- renderPlot({
    #amount of movies per year
    p <- ggplot(temp) + aes(x = temp$Year)+ 
         geom_bar( fill="tomato3") + 
         labs(title="Bar Chart",subtitle="Amount of Movies per Year",caption="source: Year") + 
         theme(axis.text.x = element_text(angle=65, vjust=0.6)) 
    p
  })
  
  output$MoviesPerRuntimeChart <- renderPlot({
    #amount of movies per runtime
    z <- ggplot(temp) + aes(x = factor(temp$Running.Time))+
         geom_bar( fill="tomato3") + 
         labs(title="Bar Chart",subtitle="Amount of Movies per Run Time",caption="source: Running.Time") +  
         theme(axis.text.x = element_text(angle=90, vjust=0.6)) 
    z
  })
  
  output$RunTimeGenrePlot <- renderPlot({
    ##shows the min and max RunTime of each Genre
    d <- ggplot(temp, aes(x=Genres, y=Running.Time, group = Running.Time)) + 
         geom_point(col="tomato2", size=3) +   # Draw points
         geom_segment(aes(x=Genres, 
                       xend=Genres, 
                       y=min(Running.Time), 
                       yend=max(Running.Time)), 
                       linetype="dashed", 
                       size=0.1) + 
         labs(title="Dot Plot",subtitle="Genre & Run Time", caption="Genre & Run Time") +  
         coord_flip()
    d
  })
  
  output$MoviesPerGenreChart <- renderPlot({
    #amount of movies per Genre
    t <- ggplot(temp, aes(x=factor(Genres))) + 
         geom_bar( width=.5, fill="tomato3") + 
         labs(title="Bar Chart", 
              subtitle="Amount per Genre", 
              caption="source: Genres") + 
         theme(axis.text.x = element_text(angle=65, vjust=0.6))
    t
  })
  
  output$MoviesPerCertificateChart <- renderPlot({
    #amount of movies per Certificate
    c <- ggplot(temp, aes(x=factor(Certificate))) + 
         geom_bar( width=.5, fill="tomato3") + 
         labs(title="Bar Chart", 
              subtitle="Amount per Certificate", 
              caption="source: Certificate") 
         # + theme(axis.text.x = element_text(angle=65, vjust=0.6))
    c
  })
  
  output$MovieKeyWordsChart <- renderPlot({
    #plot for the keyword
    k <- ggplot(b, aes(x= Keyword, y = factor(Freq))) + 
         geom_bar(position="dodge", stat="identity", width=.5, fill="tomato3") + 
         labs(title="Bar Chart", 
              subtitle="Amount per Keyword", 
              caption="source: Keyword")+ 
         theme(axis.text.x = element_text(angle=90, vjust=0.6)) + coord_flip()
    k
  })
  
  output$MoviesPerYearTable = DT::renderDataTable({
    #table for movies per year,full data unique titles set(no duplicates of same movie)
    movieYearTable <- as.data.frame(table(temp$Year))
    names(movieYearTable)[names(movieYearTable) == "Var1"] <- "Year"
    movieYearTable <- movieYearTable[rev(order(movieYearTable$Freq)),]
    movieYearTable
  })
  
  output$MoviesPerRunTimeTable = DT::renderDataTable({
    #table for movies per runtime
    runtimeTable <- as.data.frame(table(temp$Running.Time))
    names(runtimeTable)[names(runtimeTable) == "Var1"] <- "RunTime"
    runtimeTable <- runtimeTable[rev(order(runtimeTable$Freq)),]
    runtimeTable
  })
  
  output$RunTimeGenreTable = DT::renderDataTable({
    #table for runtime/genre
    runtimeGenresTable <- as.data.frame(table(temp$Genres,temp$Running.Time))
    names(runtimeGenresTable)[names(runtimeGenresTable) == "Var1"] <- "Genres"
    names(runtimeGenresTable)[names(runtimeGenresTable) == "Var2"] <- "RunTime"
    runtimeGenresTable <- runtimeGenresTable[rev(order(runtimeGenresTable$Freq)),]
    runtimeGenresTable
  })
  
  output$MovieGenreTable = DT::renderDataTable({
    #table for genre
    genreTable <- as.data.frame(table(temp$Genres))
    names(genreTable)[names(genreTable) == "Var1"] <- "Genres"
    genreTable <- genreTable[rev(order(genreTable$Freq)),]
    genreTable
  })
  
  output$MovieCertificateTable = DT::renderDataTable({
    #table for movies per Certificate
    certificateTable <- as.data.frame(table(temp$Certificate))
    names(certificateTable)[names(certificateTable) == "Var1"] <- "Certificate"
    certificateTable <- certificateTable[rev(order(certificateTable$Freq)),]
    certificateTable
  })
  
  output$MovieKeywordsTable = DT::renderDataTable({
    #table for movies per keywords
    a
  })
  
  
}#end ofserver

shinyApp(ui = ui, server = server)