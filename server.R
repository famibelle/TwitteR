#inspiration came from http://davetang.org/muse/2013/04/06/using-the-r_twitter-package/
#install.packages("ROAuth")
#install.packages("twitteR")
#install.packages("wordcloud")
#install.packages("tm")
#install.packages("devtools")
#library(devtools)
# install.packages("SnowballC")
#install_github("timjurka/sentiment")
#install_url("http://cran.r-project.org/src/contrib/Archive/sentiment/sentiment_0.2.tar.gz")
# devtools::install_github('christophergandrud/d3Network')

# require(sentiment)
require(graphics)

library(twitteR)
library(shiny)
library(ROAuth)
library(RColorBrewer)
library(tm)
library(wordcloud)
library(googleVis)
library(ggplot2)
library(gridExtra)
library(plyr)
library(igraph)
library(stringr)
library(SnowballC)
library(ape) # for the phylogenetic dendrogram
library(ggdendro)
library(networkD3)

# load functions 
source("sentiment/R/classify_emotion.R")
source("sentiment/R/classify_polarity.R")
source("sentiment/R/create_matrix.R")
source("who-retweet.R") 
source("clean.txt.R")
source("Stop.words.R")
source("purify.R")
source("hclust_to_d3js.R")

# get the world main cities and and countries WOIED (Where On Earth IDentifier)
CountryWoeid <- fromJSON(txt = "WOEID.json", flatten =TRUE)
TownList <-    CountryWoeid[CountryWoeid$placeType.name == "Town"    ,  ]
CountryList <- CountryWoeid[CountryWoeid$placeType.name == "Country" ,  ]

# load all the needed twitter authentication 
load("twitter.authentication")
registerTwitterOAuth(twitCred)

# Shiny main program
shinyServer(
    function(input, output, session) {
        r_stats <- reactive({
#             validate(
#                 need(input$TwitterQuery != "", "En attente du mot clé")
#             )
            
            QueryResult <- searchTwitteR(input$TwitterQuery, 
                                            n = input$n_Tweets, 
                                            since = as.character(input$daterange[1]),
                                            until = as.character(input$daterange[2]),
                                            lang = input$lang,
                                            cainfo = "cacert.pem")                
            
            #Transform the list into a neat dataframe
            do.call("rbind", lapply(QueryResult, as.data.frame))
        })
        
        output$TwitterQuery <- renderDataTable({
                    r_stats()[,c("screenName", "text", "created") ]
        })

        output$on_Tweets    <- renderPrint({input$n_Tweets})
        output$oid2         <- renderPrint({input$id2})
        output$odate        <- renderPrint({input$daterange})
        
        
        output$sentiment <- renderGvis({
            withProgress(message = 'Calculation in progress',
                         detail = 'This may take a while...', value = 0, {
                            TweetSentiments <- r_stats()
        
                            emotion  <- classify_emotion( TweetSentiments[,c("text") ],algorithm="bayes", prior=1.0)
                            polarity <- classify_polarity(TweetSentiments[,c("text") ],algorithm="bayes", prior=1.0)
                            emotion  <- as.data.frame(emotion)
                            polarity <- as.data.frame(polarity)
                         }
            )
            
            TweetSentiments[,17] <- emotion[,7]
            TweetSentiments[,18] <- polarity[,4]
            
            names(TweetSentiments) <- c(
                "text", "favorited", "favoriteCount", "replyToSN", "created", "truncated", 
                "replyToSID", "id", "replyToUID", "statusSource", "screenName", "retweetCount", 
                "isRetweet", "retweeted", "longitude", "latitude", 
                "emotion", "polarity"
            )
            Polarity <- count(TweetSentiments$polarity)
            Emotion  <- count(TweetSentiments$emotion)
            PiePolarity <- gvisPieChart(Polarity,
                                        options=list(
                                            title='Tweets Polarity'
                                            )
                                        )
            PieEmotion  <- gvisPieChart(Emotion,
                                        options=list(
                                            title='Tweets Emotion'
                                        )
            )
            PolarityPercentage <- Polarity
            PolarityPercentage$freq <- round(PolarityPercentage$freq/input$n_Tweets*100,2)
            GaugePolarityNegative <- gvisGauge(
                PolarityPercentage[1,], labelvar = "x",
                options=list(
                    #                     animation = "{startup: TRUE, duration : 400 }",
                    #                     NumberFormat = "{pattern: \"#'%'\"}",
                    min=0, 
                    max=100, 
                    redFrom=75, 
                    redTo=100
                )
            )
            
            Gauges <- gvisMerge(gvisMerge(PiePolarity, PieEmotion, horizontal=FALSE),
                                GaugePolarityNegative, horizontal=TRUE, tableOptions="cellspacing=5")
            return(Gauges)
        })

        output$TrendingTopics <- renderDataTable({
            TT <- getTrends(TownList[TownList$name == input$town, "woeid"], cainfo = "cacert.pem")
            TrendingTopics <- as.data.frame(TT$name)
            names(TrendingTopics) <- paste("Trending in ", input$town, sep="")
            return(TrendingTopics)            
        },
            options=list(
                     lengthChange = FALSE    # show/hide records per page dropdown
            )
        )
        
        output$WhoRT <- renderPlot({
            withProgress(message = 'Collecting tweets in progress',
                         detail = 'This may take a while...', value = 0, {
                             Who_RT_the_Tweet(Tweet = input$TwitterQuery, no_of_tweets = input$n_Tweets, lang = input$lang)
                         })
            })

        
        output$dendrogram <- renderPrint({
            withProgress(message = 'Collecting tweets in progress',
                         detail = 'This may take a while...', value = 0, {
                             VectorTweet <- as.vector(r_stats()[,"text"])
                         })
            
            withProgress(message = 'Processing Corpus and Dendrogram',
                         detail = 'This may take a while...', value = 0, {
                            TweetCorpus <- purify(VectorTweet, lang = input$lang)
                            tdm <- TermDocumentMatrix(TweetCorpus, control = list(wordLengths = c(1, Inf)))
                            # remove sparse terms
                            tdm2 <- removeSparseTerms(tdm, sparse = 0.95)
                            m2 <- as.matrix(tdm2)
                            # cluster terms
                            distMatrix <- dist(scale(m2))
                            hc <- hclust(distMatrix, method = "ward.D")
                            d3js.html <- treeNetwork(as.treeNetwork(hc))
#                             JSON <- HCtoJSON(hc)
#                             Flare <- rjson::fromJSON(JSON)
#                             d3js.html <- d3ClusterDendro(
#                                 Flare,
#                                 fontsize = 12,
#                                 zoom = TRUE, 
#                                 widthCollapse = 0.8
#                             )
#                             d3js.html <- d3Tree(List = Flare, fontsize = 12, diameter = 800)
                            
                            return(d3js.html)
                         })
        })
        
        output$plot <- renderPlot({
            withProgress(message = 'Collecting tweets in progress',
                         detail = 'This may take a while...', value = 0, {
                             VectorTweet <- as.vector(r_stats()[,"text"])
                         })

            withProgress(message = 'Processing word cloud',
                         detail = 'This may take a while...', value = 0, {
                Tweet_palette <-brewer.pal(9,"Set1")
                TweetCorpus <- purify(VectorTweet, lang = input$lang)
                wordcloud(TweetCorpus,
                          min.freq=5,max.words=500, 
                          random.order=FALSE,
                          scale = c(4,1),
                          colors=Tweet_palette)
            })
        })
    }
)