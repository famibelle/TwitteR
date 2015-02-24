library(rjson)
library(jsonlite)

# get the world main cities and and countries WOIED (Where On Earth IDentifier)
CountryWoeid <- fromJSON(txt = "WOEID.json", flatten =TRUE)
TownList <-    CountryWoeid[CountryWoeid$placeType.name == "Town"    ,  ]
CountryList <- CountryWoeid[CountryWoeid$placeType.name == "Country" ,  ]

# Load all the Twitter supported languages
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
        textInput('TwitterQuery', "Text to be searched (#, @ included): ", "#JeSuisCharlie"), #labeled TwitterQuery
        numericInput('n_Tweets', 'Number of tweets to retrieve: ', 101, min = 1, max = 1500, step = 1), #labeled n_Tweets
        selectInput("lang","Select the language",
                    choices = languages$code
        ),
        
        dateRangeInput("daterange", "Select a Date range:",
                       start = Sys.Date()-10,
                       end   = Sys.Date())
    ),
    
    mainPanel(
        tabsetPanel(
            tabPanel("Words Cloud", plotOutput("plot")),
            tabPanel("Sentiment Analysis", htmlOutput("sentiment")),
            tabPanel("Tweets", dataTableOutput("TwitterQuery")),
            tabPanel("Who has RT the tweets", plotOutput("WhoRT")),
            tabPanel("Cluster Dendrogram", renderTreeNetwork("dendrogram"))
        )
    )
))