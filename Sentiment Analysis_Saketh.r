install.packages('shinythemes')
install.packages('SentimentAnalysis')
install.packages('qdap')
install.packages('tidytext')
install.packages('topicmodels')
install.packages('ngram')
install.packages('lsa')
install.packages('syuzhet')
install.packages('eeptools')
install.packages('sentimentr')
library(shiny)
library(topicmodels)
library(shinythemes)
library(tm)
library(wordcloud)
library(reshape2)
library(qdap)
library(tidytext)
library(ggplot2)
library(dplyr)
library(tidyr)
library(janeaustenr)
library(stringr)
library(ngram)
library(lsa)
library(syuzhet)
library(eeptools)
library(RColorBrewer)
library(sentimentr)
library(SentimentAnalysis)

#ui.r

ui = tagList(
  navbarPage(
    theme = shinythemes::shinytheme("cerulean"),
    "YELP.COM",
    tabPanel("Sentiment Analysis",
             sidebarPanel(
               fileInput("selection", "Choose a text:"),
               
               hr(),
               
               sliderInput("max",
                           "Maximum Number of Words:",
                           min = 1,  max = 300,  value = 20),
               hr(),
               
               textInput("text", label = h4("Comment")),
               
               downloadButton('save', 'Save')
             ),
             mainPanel(
               tabsetPanel(
                 tabPanel("WordCloud",
                          plotOutput("plot")
                          
                          
                 ),
                 tabPanel("Frequency",
                          dataTableOutput('table')
                          
                 ),
                 tabPanel("Sentiment",
                          plotOutput("plot2")
                 ),
                 tabPanel("Comparison", 
                          plotOutput("plot1")),
                 
                 tabPanel("Negative/Positive", 
                          plotOutput("plot3")),
                 tabPanel("Emotions", 
                          
                          fluidRow(
                            splitLayout(cellWidths = c("50%", "50%"), plotOutput("plot4"), plotOutput("plot5"))
                          )
                          
                          
                          
                          
                 )
               )
               
             )
    )
  )
)




#Server.r
server <- function(input, output, session) {
  # Define a reactive expression for the document term matrix
  terms <- reactive({
    # Change when the "update" button is pressed...
    
    getTermMatrix(input$selection)
    
  })
  
  # Make the wordcloud drawing predictable during a session
  wordcloud_rep <- repeatable(wordcloud)
  
  output$plot <- renderPlot({
    v <- terms()
    wordcloud_rep(names(v), v, scale=c(5,1),
                  max.words=input$max,
                  colors=brewer.pal(6, "Dark2"))
  })
  
  
  ## Draw a pictore of negative and positive words 
  
  output$plot1 <- renderPlot({
    h <- terms()
    d <- data.frame(word=names(h),freq=h)
    rownames(d) <- NULL
    
    d$word <- as.character(d$word)
    colnames(d) <- c("word", "n")
    
    bing_word_count <- d %>%
      inner_join(get_sentiments("bing")) %>%
      ungroup()
    
    bing_word_count %>%
      acast(word ~ sentiment, value.var = "n", fill = 0) %>%
      comparison.cloud(colors = c("#F8766D", "#00BFC4"))
    
  })
  
  
  ## Sentiment Analysis Based on words
  term <- reactive({
    # Change when the "update" button is pressed...
    
    Sentimentwords(input$selection)
    
  })
  
  output$plot2 <- renderPlot({
    
    k <- term()
    sentiment <- analyzeSentiment(k)
    pnn <- convertToDirection(sentiment$SentimentQDAP)
    barplot(table(pnn), main="Sentiment Analysis", 
            col=cm.colors(3))
  })
  
  output$plot3 <- renderPlot({
    h <- terms()
    d <- data.frame(word=names(h),freq=h)
    rownames(d) <- NULL
    
    d$word <- as.character(d$word)
    colnames(d) <- c("word", "n")
    
    bing_word_count <- d %>%
      inner_join(get_sentiments("bing")) %>%
      ungroup()
    
    bing_word_count %>%
      group_by(sentiment) %>%
      top_n(10,n) %>%
      ungroup() %>%
      mutate(word = reorder(word, n)) %>%
      ggplot(aes(word, n, fill = sentiment)) +
      geom_col(show.legend = FALSE) +
      facet_wrap(~sentiment, scales = "free_y") +
      labs(y = "Contribution to sentiment",
           x = NULL) +
      coord_flip()
  })
  
  output$table <- renderDataTable({
    h <- terms()
    d <- data.frame(word=names(h),freq=h)
    rownames(d) <- NULL
    d <- data.frame(d)
    d})
  
  
  output$plot4 <- renderPlot({
    #negative feelings
    m <- term()
    corp <- Corpus(VectorSource(m))
    aa<- get_nrc_sentiment(as.character(corp))
    td <-data.frame(t(aa))
    names(td)[1] <- "count"
    td <- cbind("sentiment"=rownames(td),td)
    rownames(td) <- NULL
    
    
    td_new2 <- td[td$sentiment %in% c("trust","anticipation","joy","surprise","anger","sadness","disgust","fear"), ]
    td_new2 <- td_new2[order(td_new2$count,decreasing = TRUE),] 
    
    td_new3 <- td_new2[td_new2$sentiment %in% c("anger","sadness","disgust","fear"), ]
    td_new3$count <- as.integer(td_new3$count)
    td_new3 <- td_new3[order(td_new3$count,decreasing = TRUE),] 
    td_new3$count <- as.integer(td_new3$count)
    td_new3$count2 <- prop.table(td_new3$count)
    ggplot(td_new3,aes(x=reorder(sentiment, order(count2,decreasing = T)),y=count2,
                       fill=sentiment))+geom_bar(stat='identity')+ggtitle("Customer Service Negative Words Analysis")+labs(x="Sentiment",y="Percentage")
  })
  
  output$plot5 <- renderPlot({
    #positive feelings
    m <- term()
    corp <- Corpus(VectorSource(m))
    aa<- get_nrc_sentiment(as.character(corp))
    td <-data.frame(t(aa))
    names(td)[1] <- "count"
    td <- cbind("sentiment"=rownames(td),td)
    rownames(td) <- NULL
    
    td_new2 <- td[td$sentiment %in% c("trust","anticipation","joy","surprise","anger","sadness","disgust","fear"), ]
    td_new2 <- td_new2[order(td_new2$count,decreasing = TRUE),] 
    
    td_new3 <- td_new2[td_new2$sentiment %in% c("trust","anticipation","joy","surprise"), ]
    td_new3$count <- as.integer(td_new3$count)
    td_new3 <- td_new3[order(td_new3$count,decreasing = TRUE),] 
    td_new3$count <- as.integer(td_new3$count)
    td_new3$count2 <- prop.table(td_new3$count)
    ggplot(td_new3,aes(x=reorder(sentiment, order(count2,decreasing = T)),y=count2,
                       fill=sentiment))+geom_bar(stat='identity')+ggtitle("Customer Service Positive Words Analysis")+labs(x="Sentiment",y="Percentage")
  })
  
  
  #Text Input
  textinput <- reactive({
    textin <- input$text
    
  })
  
  output$save <- downloadHandler(
    
    filename =function() {
      paste(input$selection,'comment.txt')
    },
    content = function(file) {
      
      write.csv(textinput(), file)
    }
  )
  
}





#global.r
#For the Word Cloud
getTermMatrix <- function(file) {
  
  text <- readLines(file$datapath)
  
  toString <- content_transformer(function(x,from,to) gsub(from,to,x))
  
  myCorpus = Corpus(VectorSource(text))
  myCorpus = tm_map(myCorpus, content_transformer(tolower))
  myCorpus = tm_map(myCorpus, stripWhitespace)
  myCorpus = tm_map(myCorpus, removeNumbers)
  myCorpus = tm_map(myCorpus, removePunctuation)
  myCorpus = tm_map(myCorpus, removeWords,stopwords("english"))
  myCorpus = tm_map(myCorpus, removeWords,c('gmt',"one","two","three","four","seven","five","twenty",'fifty','forty','nine','six','zero','eight','ten',"just","hundred","thousand",'seventeen','sixty','thirty'))
  
  
  myDTM = TermDocumentMatrix(myCorpus,
                             control = list(minWordLength = 1))
  
  m = as.matrix(myDTM)
  
  sort(rowSums(m), decreasing = TRUE)
}

#For the Sentiment Analysis
Sentimentwords <- function(file) {
  
  text <- readLines(file$datapath)
}




#run the app
shinyApp(ui, server)
