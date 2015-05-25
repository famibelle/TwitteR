library(rjson)
library(jsonlite)
library(networkD3)
library(xts)
library(dygraphs)

# get the world main cities and and countries WOIED (Where On Earth IDentifier)
CountryWoeid <- fromJSON(txt = "WOEID.json", flatten =TRUE)
TownList <-    CountryWoeid[CountryWoeid$placeType.name == "Town"    ,  ]
CountryList <- CountryWoeid[CountryWoeid$placeType.name == "Country" ,  ]

# Load all the Twitter supported languages see twitter doc for more details
languages <- fromJSON(txt = "language.json", flatten =TRUE)

#Add Google Analytics to the Shiny app
tags$head(includeScript("google-analytics.js"))

shinyUI(fluidPage(
    # include google analytics source http://shiny.rstudio.com/articles/google-analytics.html
    tags$head(includeScript("google-analytics.js")),    

    # Load D3.js 
    tags$head(
        tags$script(src = 'http://d3js.org/d3.v3.min.js')
    ),
    
    # Application title
    titlePanel("A Minimum Viable Twitter Analysis Tool"),
    sidebarPanel(
        textInput('TwitterQuery', "Text to be searched (#, @ included): ", "orange france"), #Terms can contain spaces, and multiple terms should be separated with "+"
        numericInput('n_Tweets', 'Number of tweets to retrieve: ', 101, min = 1, max = 1500, step = 1), #labeled n_Tweets
        selectInput("lang","Select the language",
                    choices = languages$name
        ),
        
        dateRangeInput("daterange", "Select a Date range:",
                       start = Sys.Date()-1,
                       end   = Sys.Date())
    ),
    
    mainPanel(
        tabsetPanel(
            tabPanel("Words Cloud", plotOutput("plot")),
#             tabPanel("Sentiment Analysis", htmlOutput("sentiment")),
            tabPanel("Tweets", dataTableOutput("TwitterQuery")),
            tabPanel("Network view of the conversations", simpleNetworkOutput("tweetnetwork")),
            tabPanel("Tweets timeline", dygraphOutput("TweetsTimeLine")),
            tabPanel("Cluster Dendrogram", treeNetworkOutput("dendrogram")),
            tabPanel("Tweets sources", htmlOutput("TweetSource")),
            tabPanel("Who has RT the tweets", simpleNetworkOutput("WhoRT"))         
        )
    )
))