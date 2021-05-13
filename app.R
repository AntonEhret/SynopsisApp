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
library(rlist)
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
             h4("Main Differences between Script and Synopsis"),
                  HTML("<style>
                           table {
                               font-family: arial, sans-serif;
                               border-collapse: collapse;
                               width: 50%;
                           }
                       
                       td, th {
                           border: 1px solid #dddddd;
                           text-align: left;
                           padding: 6px;
                       }
                       
                       tr:nth-child(even) {
                           background-color: #dddddd;
                       }
                       
                       h5 {
                           text-align: center;
                           font-size: 20px;
                       }
                       
                       </style>
                       <table>
                      <tr>
                      	<th></th>
                        <th>Script</th>
                        <th>Synopsis</th>
                      </tr>
                     <tr>
                        <td>Source</td>
                        <td>IMSDB, scriptsslug, scripts, others</td>
                        <td>IMDB</td>
                      </tr>
                      <tr>
                        <td>Amount obtained</td>
                        <td>516</td>
                        <td>369</td>
                      </tr>
                      <tr>
                        <td>Nr of Words</td>
                        <td>30.000 - 50.000</td>
                        <td>300 - 800</td>
                      </tr>
                      <tr>
                        <td>Author</td>
                        <td>screenplay writer</td>
                        <td>IMDB user</td>
                      </tr>
                      <tr>
                        <td>Content</td>
                        <td>spoken words of the movie</td>
                        <td>summary of the movie</td>
                      </tr>
                      <td>Expected differences</td>
                      <td>Harder to see patterns due to<br>
                      	length and missing music and pictures</td>
                      <td>Easier to see patterns due to <br>
                      	condensed format and judgement of viewer</td>
                    </table>"),
             br(),
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
         h3("Sentiment Analysis with NRC and AFINN"),
         br(),
         p("Below you can compare the scores for AFINN and NRC for the synopsis and scripts"),
         br(),
         mainPanel(width = 10
            , fluidRow(
                splitLayout(cellWidths = c("50%", "50%"), h5("Synopsis"),  h5("Scripts")),
                HTML("<h4>AFINN</h4>"),
                splitLayout(cellWidths = c("50%", "50%")
                            , tags$img(src='AFINN over the years.jpeg', height="100%", width="100%")
                            , tags$img(src='AFINN over the years Scripts.jpeg', height="100%", width="100%")
                            ),
                HTML("<h4>NRC</h4>"),
                sliderInput("Decade", "Timeframe:"
                        , min = 1935, max = 2015
                        , value = 1935, step = 5
                        , animate = animationOptions(interval = 500, loop = TRUE)
                        , width = "40%"),
                splitLayout(cellWidths = c("50%", "50%")
                            , plotOutput("radarplot")
                            , plotOutput("radarplot2")
                )
                )
         )
    )
#' *Heatmap*
    , tabPanel("3. Topic Modeling",
               h3("Heatmap based on LDA"),
               br(),
               sliderInput("Topic","Pick the topic"
                           , min = 1, max = 50, value = 1, step = 1),
               mainPanel(
                   fluidRow(
                       splitLayout(
                           cellWidths = c("35%","60%", "60%")
                           , fluidRow(
                               htmlOutput("mov"))
                           , fluidRow(
                               h5("Synopsis")
                               , tags$img(src='Heatmap Synopsis1.jpeg', height="100%", width="100%"))
                           , fluidRow(
                               h5("Scripts")
                               , tags$img(src='Heatmap Scripts1.jpeg', height="100%", width="100%"))
                           )
                       ),
                   br(),
                   br(),
               p("When inspecting the 50 topics and what documents are having their highest gamma value in the topic,", br(),
                 "the topics tends to have one of these 3 specific characters:"),
               p("1.  A topic is descriptive containing 3 or more movies, representative words make sense and are aligned", br(),
                    "with most of the movies that are grouped in one topic.", br(),
                 "2.  A topic that has 1 or 2 movies. This kind of topic is extremely specific and describes only 1 or 2 movies.", br(),
                    "An example of this is topic 1 containing a movie Parasite from 2019. Parasite became the first non-English film", br(),
                    "to win the Academy Award for Best Picture therefore it makes sense this movie belongs to a separate category.", br(),
                 "3.  A topic containing movie combinations that are strange. Meaning it seems these movies do not have much in common", br(),
                    "but are categorized in one group anyway, e.g., topic 21 which combines The Wizard of Oz and Taxi Driver,", br(), 
                    "while it may seem random, the explanation lies in the words that the script contains.")
               )
    )

#' *Intertopic distance map*
    , tabPanel("4. Intertopic Distance Map",
         h3("Intertopic Distance Map for Visualization of Topics"),
         p("From the left hand side of the plot you can see the inter relationships between the topics."),
         p("When you press a topic, you can see all the top words for this specific topic on the right hand side.", br(),
           "Each word is depict with a red bar, which is the frequency of this word in this specific topic compared to the whole corpus, which is shown as the blue bar."),
         
         mainPanel(
             fluidRow(visOutput("DistMap")
                 ),
             br(),
             sliderInput("Topic3","Pick the topic"
                         , min = 1, max = 50, value = 1, step = 1),
             htmlOutput("mov3")
             )
         )
#' *Tsne*
, tabPanel("5. Movie Clusters based on TSNE",
           h3("Movie Clusters based on T-distributed Stochastic Neighbor Embedding"),
           p("Below you can see the synopsis and the scripts put on a 2D graph, created with a dimensional reduction based on LDA using t-SNE."),
           br(),
           mainPanel(
               width = 11,
               splitLayout(cellWidths = c("50%", "50%"),
                           h5("Clusters from Synopsis"),
                           h5("Clusters from Scripts")
               ),
               splitLayout(cellWidths = c("50%", "50%"),
                           plotlyOutput("tsne_syn", height = "500px"),
                           plotlyOutput("tsne_script", height = "500px"))
               # , p("When fitting the documents into topics based on the highest gamma value within that topic, it means that some information is lost.", br(),
               #   "For instance, if the highest gamma value is 0.40, the document will be clustered to that specific topic, even though the topic only", br(),
               #   "consists of 40% of that topic, hence we are loosing some information. t-SNE, a non linear technique used for dimensionality reduction,", br(),
               #   "is very well suited for visualizations of this types of datasets. With t-SNE, we can reduce this dimensionality, and get a 2D plot, ", br(),
               #   "where we can see some more relevant clusters of movies, that are based on all of the dimensions. This technique also comes with a cost", br(),
               #   "since the dimensionality reduction means loosing some information, but in this case these benefits are estimated to clearly outweigh the costs.")
           )
)
#' *Conclusions*
    , tabPanel("Conclusions",
               h3("Conclusions"),
               br(),
               p("So what are the Conclusions we can make?"),
               mainPanel(
                   h4("Analysis of Sentiments"),
                   HTML("
                     <style>
                         .box {
                                width: 80%;
                                align: center;
                                border: 4px solid #008B8B;
                                padding: 20px;
                                margin: auto;
                           }
                       </style>
                     <div class='box'>
                     <strong>We can observe:<strong><br>
                     <ul>
                      <li>There are no significant patterns in nrc or afinn for scripts or synopsis</li>
                      <li>NRC: In both cases trust and fear are the strongest emotions, the over all patterns are similar.</li>
                      <li>AFINN: The synopsis seems to be written in an overall more negative tone</li>
                    </ul>
                    <strong>One can conclude:<strong><br>
                    <ul>
                      <li>The emotions portrayed in the scripts and in the synopsis did not change significantly over the years</li>
                      <li>The academy award members are looking for a specific feeling that the movies portray when nominating them</li>
                    </ul>
                     </div>"),
                   h4("Clusters based on TSNE"),
                   HTML("
                     <style>
                         .box {
                                width: 80%;
                                align: center;
                                border: 4px solid #008B8B;
                                padding: 20px;
                                margin: auto;
                           }
                       </style>
                     <div class='box'>
                     <strong>We can observe:<strong><br>
                     <ul>
                      <li>In both cases some clusters are seem to be more correct than others</li>
                      <li>The clusters based on the synopsis make less sense than based on the scripts</li>
                    </ul>
                    <strong>One can conclude:<strong><br>
                    <ul>
                      <li>If one was to make use of the clusters for e.g. recommendations of related movies the scripts should be rather analyzed than the synopsis</li>
                    </ul>
                     </div>")
                   )
               )
)
################################################################################
#' @Server
server <- function(input, output) {
#' *NRC RADARCHART Synopsis*
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
    
#' *NRC RADARCHART Script*
    nrc2 <- read.csv("Data/nrcScripts.csv")
    sum2 <- nrc2 %>% group_by(decade) %>% summarise(sum2 = sum(n)) 
    radarplot2 <- reactive({
        Y = input$Decade
        n2 <- data.frame(matrix(c(round(nrc2$n[nrc2$decade == Y]/sum2$sum2[sum2$decade == Y]*100,1)), ncol = 8)) 
        min2 <- data.frame(t(rep(0,8)))
        max2 <- data.frame(t(rep(24,8)))
        radar2 <- rbind(max2, min2, n2)
        colnames(radar2) <- c(nrc2$sentiment[nrc2$decade == Y])
        rownames(radar2) <- c("max", "min", "year")
        radarchart(radar2
                   , pcol = "deeppink3"
                   , pfcol = scales::alpha("pink", 0.5), plwd = 2, plty = 1
                   , cglcol = "grey", cglty = 1, cglwd = 0.8
                   , title = paste0("Timeframe ", Y, " - ", Y+5)
                   , caxislabels=seq(0,max2[1,1],(max2[1,1]/4))
                   , axislabcol="grey"
                   , axistype=4
        )
    })
    output$radarplot2 <- renderPlot({
        radarplot2()
    })
    
#' *Topics*
    script_lda_tidy <- read.csv("Data/synopsis_LDA_tidy, T = 50, A = 0.1, D = 0.1.csv")
    topics <- 50
    lower <- 1
    upper <- topics
    max_topic <- NA
    for (i in 1:(nrow(script_lda_tidy)/topics)) {
        max_topic[i] <- which.max(script_lda_tidy$gamma[lower:upper])
        lower <- lower + topics
        upper <- upper + topics
    }
    
    mov <- list()
    for (i in 1:topics){
        mov[[i]] <- script_lda_tidy$document[which(max_topic == i)*topics]
    }
    output$mov <- renderUI({
        x <- paste0("<strong>&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;",length(mov[[input$Topic]])," movies in this topic: ","</strong>","<br>","<br>","<ul>", paste(paste0("<li>",mov[[input$Topic]],"</li>"), collapse = " "), "</ul>")
        HTML(x)
    })
    
    
#' *TSNE Synopsis*
    #Tsne is done in different doc
    tsne_out_DF <- list.load("Data/tsne_out.Rdata")
    #tsne_out_DF <- data.frame(x=tsne_out$y[,1],y=tsne_out$y[,2])
    clusters_gammas <- read.csv("Data/clusters_gamma.csv")
    df_names <- read.csv("Data/df_names.csv")
    
    output$tsne_syn <- renderPlotly({
        plot_ly(tsne_out_DF, mode= "markers", type= "scatter", x=tsne_out_DF$x, y=tsne_out_DF$y, color= ~factor(clusters_gammas$...52), text=df_names$document) %>% 
            layout(hovermode="closest")
    })
#' *TSNE Script*
    scripts_lda2 <- readRDS("Data/tsne_scripts/scripts_lda, T = 50, A = 0.1, D = 0.1.rds")
    #Tsne is done in different doc
    tsne_out2 <- list.load("Data/tsne_scripts/tsne_out.RData")
    tsne_out_DF2 <- data.frame(x=tsne_out2$Y[,1],y=tsne_out2$Y[,2])
    clusters_gammas2 <- read.csv("Data/tsne_scripts/clusters_gammas.csv")
    df_names2 <- read.csv("Data/tsne_scripts/df_names.csv")
    
    output$tsne_script <- renderPlotly({
        plot_ly(tsne_out_DF2, mode= "markers", type= "scatter", x=tsne_out_DF2$x, y=tsne_out_DF2$y, color= ~factor(clusters_gammas2$...52), text=df_names2$document) %>% 
            layout(hovermode="closest")
    })
    
#' *DistanceMap Synopsis*
    ldaOut <- readRDS("Data/synopsis_lda, T = 50, A = 0.1, D = 0.1.rds")
    topics <- 50
    ldaOut.topics <- as.matrix(topics(ldaOut))
    k=50
    ldaOut.terms <- as.matrix(terms(ldaOut,15))
    topicmodels2LDAvis <- function(x, ...){
        post <- topicmodels::posterior(x)
        if (ncol(post[["topics"]]) < 3) stop("The model must contain > 2 topics")
        mat <- x@wordassignments
        LDAvis::createJSON(
            phi = post[["terms"]], 
            theta = post[["topics"]],
            vocab = colnames(post[["terms"]]),
            doc.length = slam::row_sums(mat, na.rm = TRUE),
            term.frequency = slam::col_sums(mat, na.rm = TRUE)
        )
    }
    output$DistMap <- renderVis(
        topicmodels2LDAvis(ldaOut)
    )
    
    mov3 <- mov[order(sapply(mov, length), decreasing=T)]
    output$mov3 <- renderUI({
        x <- mov3[[input$Topic3]]
        HTML(x)
    })
}
################################################################################
shinyApp(ui, server)