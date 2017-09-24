### I am using a code modification from a lecture of Yanchang Zhao

require("twitteR")
require("httr")

setup_twitter_oauth('xxxxxxxxxxxxxxxxxx', 'xxxxxxxxxxxxxxxxxxxxx',
                    'xxxxxxxxxxxxxxxxxxxx', 'xxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxx')
                    
tweets <- searchTwitter("Kyrgyzstan",n=1000,lang="en",since="2010-01-01")
# convert tweets to a data frame
tweets.df <- twListToDF(tweets)
# tweet #190
tweets.df[190, c("id", "created", "screenName", "replyToSN",
                 "favoriteCount", "retweetCount", "longitude", "latitude", "text")]
tweets.df$text <- sapply(tweets.df$text,function(row) iconv(row, "latin1", "ASCII", sub=""))

write.csv(tweets.df, file = "tweetsdf.csv")

# print tweet #190 and make text fit for slide width
writeLines(strwrap(tweets.df$text[190], 60))

require("tm")
# build a corpus, and specify the source to be character vectors
myCorpus <- Corpus(VectorSource(tweets.df$text))
# convert to lower case
myCorpus <- tm_map(myCorpus, content_transformer(tolower))
# remove URLs
removeURL <- function(x) gsub("http[^[:space:]]*", "", x)
myCorpus <- tm_map(myCorpus, content_transformer(removeURL))
# remove anything other than English letters or space
removeNumPunct <- function(x) gsub("[^[:alpha:][:space:]]*", "", x)
myCorpus <- tm_map(myCorpus, content_transformer(removeNumPunct))
# remove stopwords
myStopwords <- c(setdiff(stopwords('english'), c("r", "big")),
                 "use", "see", "used", "via", "amp")
myCorpus <- tm_map(myCorpus, removeWords, myStopwords)
# remove extra whitespace
myCorpus <- tm_map(myCorpus, stripWhitespace)
# keep a copy for stem completion later
myCorpusCopy <- myCorpus


# inspect frequent words ## modified myCorpus = tdm
myCorpus <- tm_map(myCorpus, content_transformer(tolower))
myCorpus <- tm_map(myCorpus, removePunctuation)
myCorpus <- tm_map(myCorpus, removeNumbers)
myCorpus <- tm_map(myCorpus, removeWords,
                   c(stopwords("SMART"), "to", "and", "but"))
                     
myDTM <- TermDocumentMatrix(myCorpus,
                     control = list(minWordLength = 5))

m <- as.matrix(myDTM)

sort(rowSums(m), decreasing = TRUE)

require("wordcloud")
wordcloud(myCorpus, scale=c(3,.2), min.freq=1, max.words=200, random.order=F,
          rot.per=0.35, use.r.layout=FALSE, colors=brewer.pal(8, "Dark2"))

####


(freq.terms <- findFreqTerms(myDTM, lowfreq = 100))

term.freq <- rowSums(as.matrix(myDTM))
term.freq <- subset(term.freq, term.freq >= 100)
df <- data.frame(term = names(term.freq), freq = term.freq)

require("ggplot2")
ggplot(df, aes(x=term, y=freq)) + geom_bar(stat="identity") +
    xlab("Terms") + ylab("Count") + coord_flip() +
    theme(axis.text=element_text(size=7))

m <- as.matrix(myDTM)
# calculate the frequency of words and sort it by frequency
word.freq <- sort(rowSums(m), decreasing = T)
# colors
require("RColorBrewer")
pal <- brewer.pal(9, "BuGn")[-(1:4)]

# plot word cloud
require("wordcloud")
wordcloud(words = names(word.freq), freq = word.freq, min.freq = 3,
          random.order = F, colors = pal)

# which words are associated with 'r'?
findAssocs(myDTM, "women", 0.2)

# which words are associated with 'data'? remember all are low case
findAssocs(myDTM, "kyrgyzstan", 0.2)

#Network of Terms
#source("http://bioconductor.org/biocLite.R")
        #biocLite("Rgraphviz")

require("graph")
require("Rgraphviz")
plot(myDTM, term = freq.terms, corThreshold = 0.99, weighting = F)


#Topic Modelling
dtm <- as.DocumentTermMatrix(myDTM)
require("topicmodels")
lda <- LDA(dtm, k = 8) # find 8 topics
term <- terms(lda, 7) # first 7 terms of every topic
(term <- apply(term, MARGIN = 2, paste, collapse = ", "))


topics <- topics(lda) # 1st topic identified for every document (tweet)
topics <- data.frame(date=as.IDate(tweets.df$created), topic=topics)
ggplot(topics, aes(date, fill = term[topic])) +
    geom_density(position = "stack")


## Sentiment Analysis

# install package sentiment140
require("devtools")
install_github("sentiment140", "okugami79")
# sentiment analysis
require("sentiment")
sentiments <- sentiment(tweets.df$text)
table(sentiments$polarity)
##
## neutral positive
## 428 20
# sentiment plot
sentiments$score <- 0
sentiments$score[sentiments$polarity == "positive"] <- 1
sentiments$score[sentiments$polarity == "negative"] <- -1
sentiments$date <- as.IDate(tweets.df$created)
result <- aggregate(score ~ date, data = sentiments, sum)
plot(result, type = "l")

#Retrieve User Info and Followers
user <- getUser("unwomenctrlasia")
user$toDataFrame()
friends <- user$getFriends() # who this user follows
followers <- user$getFollowers() # this user's followers
followers2 <- followers[[1]]$getFollowers() # a follower's followers

# select top retweeted tweets
table(tweets.df$retweetCount)
selected <- which(tweets.df$retweetCount >= 9)

# plot them
dates <- strptime(tweets.df$created, format="%Y-%m-%d")
plot(x=dates, y=tweets.df$retweetCount, type="l", col="grey",
     xlab="Date", ylab="Times retweeted")
colors <- rainbow(10)[1:length(selected)]
points(dates[selected], tweets.df$retweetCount[selected],
       pch=19, col=colors)
text(dates[selected], tweets.df$retweetCount[selected],
     tweets.df$text[selected], col=colors, cex=.9)

tweets[[1]]
retweeters(tweets[[1]]$id)
retweets(tweets[[1]]$id)


#map the followers
twitterMap("unwomenctrlasia", plotType = "followers")
