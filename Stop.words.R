# Catch error for stopwords function, return "" if language is not supported
Stop.words <- function(x)
{
    y = ""
    try_error = tryCatch(stopwords(x), error=function(e) e)
    if (!inherits(try_error, "error"))
        y = stopwords(x)
    return(y)
}