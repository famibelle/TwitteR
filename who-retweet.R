# source https://sites.google.com/site/miningtwitter/questions/user-tweets/who-retweet
# Who retweets whom?
# 
# There are a couple of basic patterns for a retweet:
#     RT @user Mary had a little lamb
# Mary had a little lamb (via @user)
# 
# In either case, the meaning is simple:
#     someone is giving @user credit for a particular bit of information
# 
# The matching pattern of a retweet entity is:
#     "(RT|via)((?:\b\W*@\w+)+)"


# Step 1: Load required packages
# load packages
# Step 2: Harvest tweets about "bioinformatics"
# tweets in english containing "bioinformatics"

Who_RT_the_Tweet <- function(Tweet= "#JeSuisCharlie",no_of_tweets=101, lang="en") {
    dm_tweets = searchTwitter(Tweet,
                              n = no_of_tweets, 
                              lang = lang)
#                               cainfo = "cacert.pem")
    
    # get text
    dm_txt = sapply(dm_tweets, function(x) x$getText())
    
    
    # Step 3: Identify retweets
    # regular expressions to find retweets
    grep("(RT|via)((?:\\b\\W*@\\w+)+)", dm_tweets, 
         ignore.case=TRUE, value=TRUE)
    
    # which tweets are retweets
    rt_patterns = grep("(RT|via)((?:\\b\\W*@\\w+)+)", 
                       dm_txt, ignore.case=TRUE)
    
    # show retweets (these are the ones we want to focus on)
    dm_txt[rt_patterns] 
    
    
    # Step 4: Collect who retweeted and who posted
    # We'll use these results to form an edge list in order to create the graph
    # create list to store user names
    who_retweet = as.list(1:length(rt_patterns))
    who_post = as.list(1:length(rt_patterns))
    
    # for loop
    for (i in 1:length(rt_patterns))
    { 
    # get tweet with retweet entity
    twit = dm_tweets[[rt_patterns[i]]]
    # get retweet source 
    poster = str_extract_all(twit$getText(),
    "(RT|via)((?:\\b\\W*@\\w+)+)") 
    #remove ':'
    poster = gsub(":", "", unlist(poster)) 
    # name of retweeted user
    who_post[[i]] = gsub("(RT @|via @)", "", poster, ignore.case=TRUE) 
    # name of retweeting user 
    who_retweet[[i]] = rep(twit$getScreenName(), length(poster)) 
    }
    
    # unlist
    who_post = unlist(who_post)
    who_retweet = unlist(who_retweet)
    
    
    # Step 5: Create graph from an edglist
    # two column matrix of edges
    retweeter_poster = cbind(who_retweet, who_post)
    return(as.data.frame(retweeter_poster))
#     # generate graph
#     rt_graph = graph.edgelist(retweeter_poster)
#     
#     # get vertex names
#     ver_labs = get.vertex.attribute(rt_graph, "name", index=V(rt_graph))
#     
#     # choose some layout
#     glay = layout.fruchterman.reingold(rt_graph)
# 
#     # Step 7: Let's try to give it a more bioinformatician look
#     # another plot
#     par(bg="gray15", mar=c(1,1,1,1))
#     plot(rt_graph, layout=glay,
#        vertex.color=hsv(h=.35, s=1, v=.7, alpha=0.1),
#        vertex.frame.color=hsv(h=.35, s=1, v=.7, alpha=0.1),
#        vertex.size=5,
#        vertex.label=ver_labs,
#        vertex.label.family="mono",
#        vertex.label.color=hsv(h=0, s=0, v=.95, alpha=0.5),
#        vertex.label.cex=0.85,
#        edge.arrow.size=0.8,
#        edge.arrow.width=0.5,
#        edge.width=3,
#        edge.color=hsv(h=.35, s=1, v=.7, alpha=0.4))
#     # add title
#     title(paste("\n", no_of_tweets, "Tweets with ", Tweet, " Who retweets whom"),
#        cex.main=1, col.main="gray95", family="mono")
}