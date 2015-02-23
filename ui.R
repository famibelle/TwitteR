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
    # Application title
    titlePanel("A Minimum Viable Twitter Analysis Tool"),
    sidebarPanel(
        textInput('TwitterQuery', "Text to be searched (#, @ included): ", "#JeSuisCharlie"), #labeled TwitterQuery
        #numericInput("woeid", "Where On Earth Identifiers: ", 615702, min = 1),
        numericInput('n_Tweets', 'Number of tweets to retrieve: ', 101, min = 1, max = 1500, step = 1), #labeled n_Tweets
        selectInput("lang","Select the language",
                    choices = languages$code
        ),
        
#         checkboxInput("checkbox", label = "Take into account GPS coordinate ?", value = FALSE),
#         sliderInput("latitude", "Latitude",   -25,  min = -90,  max = 90,  step = .5),
#         sliderInput("longitude", "Longitude", 135, min = -180, max = 180, step = .5),
        
#         selectInput("town", "Choose a Town (only for Trend Topics tab)", 
#                     choices = TownList$name,
#                     selectize =TRUE
#         ),
        
        
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
            tabPanel("Cluster Dendrogram", htmlOutput("dendrogram"))
#             tabPanel("Trending Topics by major cities", dataTableOutput("TrendingTopics"))
            #tabPanel("Trending Map", htmlOutput("mapPlot")) #soon             
        )
    )
))