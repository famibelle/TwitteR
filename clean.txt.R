# source https://sites.google.com/site/miningtwitter/questions/talking-about/wordclouds/comparison-cloud
# The purpose of this function is to clean the text by lowering all the caracters and removing punctatio, url, RT, numbers etc.
clean.text = function(x)
{
    # tolower
    x = tolower(x)
    # remove rt
    x = gsub("rt ", "", x)
#     # remove at
#     x = gsub("@\\w+", " ", x)
    # remove punctuation
#     x = gsub("[[:punct:]]", "", x)
    x = gsub("(.*?)($|'|-|_|[^[:punct:]]+?)(.*?)", "\\2", x)
    # remove numbers
    x = gsub("[[:digit:]]", " ", x)
    # remove links http
    x = gsub("http\\w+", " ", x)
    # remove tabs
    x = gsub("[ |\t]{2,}", " ", x)
    # remove blank spaces at the beginning
    x = gsub("^ ", "", x)
    # remove blank spaces at the end
    x = gsub(" $", "", x)
    return(x)
}