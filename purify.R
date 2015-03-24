removeUrl <- function(x) {
    gsub("http[[:alnum:]]*", "\\2", x, perl = TRUE)
}

removalPunctuation <- function(x) {
    gsub("(.*?)($|'|-|_|[^[:punct:]]+?)(.*?)", " ", x, perl = TRUE)
}

removeFancyCharaters <- function(x) {
    gsub("[^\x20-\x7E]", "", x)
}

# http://gastonsanchez.com/blog/how-to/2012/05/29/Catching-errors-when-using-tolower.html
tryTolower = function(x)
{
    # create missing value
    # this is where the returned value will be
    y = NA
    # tryCatch error
    try_error = tryCatch(tolower(x), error = function(e) e)
    # if not an error
    if (!inherits(try_error, "error"))
           y = tolower(x)
    return(y)
}

purify <- function(text, lang) {
    text <- enc2utf8(text)
    text <- removeWords(text, Stop.words(lang))
#     text <- stri_trans_tolower(text) # alternative function to tolower() to handle smileys a so on like in https://twitter.com/_Marine_T/status/571439694954831873
    myCorpus <- Corpus(VectorSource(text)) # create corpus object    
#     myCorpus <- tm_map(myCorpus, function(x) iconv(x, to='latin1', sub='byte'))
#     myCorpus <- tm_map(myCorpus, function(x) enc2utf8(x))
    myCorpus <- tm_map(myCorpus, removeWords, "RT", lazy=TRUE)
    myCorpus <- tm_map(myCorpus, removeWords, "via", lazy=TRUE)
#     myCorpus <- tm_map(myCorpus, removeWords, "&amp")
#     myCorpus <- tm_map(myCorpus, tolower)
    myCorpus <- tm_map(myCorpus, removalPunctuation, lazy=TRUE)
    myCorpus <- tm_map(myCorpus, tryTolower)
#     myCorpus <- tm_map(myCorpus, removeNumbers)
    myCorpus <- tm_map(myCorpus, removeUrl, lazy=TRUE)
    myCorpus <- tm_map(myCorpus, removeFancyCharaters, lazy=TRUE)
#     myCorpus <- tm_map(myCorpus, removeWords, Stop.words(lang), lazy=TRUE)

#     myCorpus <- tm_map(myCorpus, removeWords, "a", lazy=TRUE) #remove stop word "a" commun to many languages, including french and english    myCorpus <- tm_map(myCorpus, stripWhitespace)
    # keep a copy of corpus to use later as a dictionary for stem completion
    myCorpus <- tm_map(myCorpus, PlainTextDocument)
# Stem Completion takes more than 1/4h to complete
#     myCorpusCopy <- myCorpus
#     # stem words
#     myCorpus <- tm_map(myCorpus, stemDocument, language = lang)
#     # stem completion
#     myCorpus <- tm_map(myCorpus, stemCompletion, dictionary = myCorpusCopy)
    return(myCorpus)
}