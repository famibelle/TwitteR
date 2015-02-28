removeUrl <- function(x) {
    gsub("http[[:alnum:]]*", "", x)
}

removalPunctuation <- function(x) {
    gsub("(.*?)($|'|-|_|[^[:punct:]]+?)(.*?)", "\\2", x)
}

purify <- function(text, lang) {
    myCorpus <- Corpus(VectorSource(text)) # create corpus object    
    myCorpus <- tm_map(myCorpus, function(x) iconv(x, to='latin1', sub='byte'))
    myCorpus <- tm_map(myCorpus, removeWords, "RTi")
    myCorpus <- tm_map(myCorpus, removeWords, "&amp")
    myCorpus <- tm_map(myCorpus, tolower)
    myCorpus <- tm_map(myCorpus, removalPunctuation)
#     myCorpus <- tm_map(myCorpus, removeNumbers)
    myCorpus <- tm_map(myCorpus, removeUrl)
    myCorpus <- tm_map(myCorpus, removeWords, Stop.words(lang))
    myCorpus <- tm_map(myCorpus, removeWords, c("a")) #remove stop word "a" commun to many languages, including french and english    myCorpus <- tm_map(myCorpus, stripWhitespace)
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