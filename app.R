################################################################################
#' @Libraries
library(shiny)
library(shinythemes)
library(shinyWidgets)
library(plotly)
library(tidyverse)
library(fmsb)
library(tm)
library(LDAvis)
library(servr)
library(dplyr)
library(stringi)
library(topicmodels)
library(slickR)
################################################################################
#' @UI
ui <- navbarPage(
    theme = shinytheme("united"),
    "Interactive Visualizations for Data Science Project",
#' *Tab*    
    tabPanel("Welcome",
             h3("Welcome to my Shiny-App"),
             
             p(strong("Thank you for stopping by!")),
             p("This is the second app to our research paper about patterns in Oscar-nominated movies."),
             
             p("In our paper we tried to answer the question: ", br(),
               tags$b("What are best picture nominees about and how has it changed over the years?")),
             
             p("We did this by doing sentiment and topic modeling on orcar nominated movie scripts.", br(),
             "However, for my individual video, I want to explore something else. The data that I will focus on now,", br(),
             "is", tags$b("viewer generated descriptions of the movies"), ". The goal is to see, whether there are differences in", br(),
             "the way that a movie is written (script) and the way a movie is descripbed by an actual viewer of the movie."),
             
             p(strong("Feeel free to explore this App :) Have fun!")),
             
             p("Cheers,",em("Anton")),
             
             
             tags$img(src='Anton.jpeg', width="350"),
             HTML("<br/>"),
             HTML("<br/>"),
             p("P.s.:", br(),
               "I am running this App on a free server, so you might notice that the performance is not that great!", br(),
               "If you notice that the plots do not show or take a while to show up, just give it a little bit of time ;)")
    )
#' *Data Preparations*
    , tabPanel("1. Data Preparations",
         h3("Data Preparations"),
         
         p("In order to obatin viewer generated summaries of the movies, I went on", tags$b("imdb.com"), "and crawled", br(),
           "for the links of all oscar nominated movies and their", tags$b("synopsis"),".", br(),
         "A synopsis is an approach to create a short video summary of a long video, which in this case is not created", br(),
           "by the director or author of the movie, but by the viewers/users of imdb."),
         br(),
         h4("The Prozess of obtaining the Data"),
         p("This time the all the Data was obtained only from imdb. The process for turning the synopsis into a format", br(),
           "suitable for analysis was the following:"),
         tags$img(src='Data Collection.jpeg', height="70%", width="70%"),
         br(),
         h4("The Availability of Synopsis on IMDB"),
         p("Not surprisingly, it can be seen that for older movies less synopsis are available.", br(),
           "Nevertheless, about 370 video summaries could be found."),
         tags$img(src='Availability synopsis.jpeg', height="70%", width="70%")
    )
#' *Sentiments*
    , tabPanel("2. Sentiment Analysis",
         h3("Sentiment Analysis with NRC and Bing"),
         
         p("When plotting the results of the analysis of the synopsis with the bing and the nrc", br(),
           "(ex. positive and negative) lexicon we get similar results to the analysis done on the scripts."),
         p("We see that the bing score is more or less the same over the years and that the radarchart", br(),
           "containing the results from the analysis based on nrc in bins of 5 years."),
         br(),
         mainPanel(width = 11
            , fluidRow(
                sliderInput("Decade", "Timeframe:"
                            , min = 1935, max = 2015
                            , value = 1925, step = 5
                            , animate = animationOptions(interval = 500, loop = TRUE)
                            , width = "40%"),
                splitLayout(cellWidths = c("40%", "60%")
                            , h4("NRC dictionary")
                            , h4("Bing dictionary")),
                splitLayout(cellWidths = c("40%", "60%")
                            , plotOutput("radarplot")
                            , tags$img(src='BING over the years.jpeg', height="100%", width="100%")))
         )
    )
)
################################################################################
#' @Server
server <- function(input, output) {
#' *NRC RADARCHART*
    sum <- read.csv("Data/sum_nrc.csv")
    nrc <- read.csv("Data/nrc.csv")
    radarplot <- reactive({
        Y = input$Decade
        n <- data.frame(matrix(c(round(nrc$n[nrc$decade == Y]/sum$sum[sum$decade == Y]*100,1)), ncol = 8)) 
        min <- data.frame(t(rep(0,8)))
        max <- data.frame(t(rep(24,8)))
        radar <- rbind(max, min, n)
        colnames(radar) <- c(nrc$sentiment[nrc$decade == Y])
        rownames(radar) <- c("max", "min", "year")
        radarchart(radar
                   , pcol = "green"
                   , pfcol = scales::alpha("lightgreen", 0.5), plwd = 2, plty = 1
                   , cglcol = "grey", cglty = 1, cglwd = 0.8
                   , title = paste0("Timeframe ", Y, " - ", Y+5)
                   , caxislabels=seq(0,max[1,1],(max[1,1]/4))
                   , axislabcol="grey"
                   , axistype=4
        )
    })
    output$radarplot <- renderPlot({
        radarplot()
    })
    
}
################################################################################
shinyApp(ui, server)