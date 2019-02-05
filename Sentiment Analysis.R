rm(list=ls())
#// Twitter sentiment Analysis using R

# Required packages 
library("SnowballC")          # These three packages used for text mining & analysis 
library("tm")
library("twitteR")
library("syuzhet")            # syuzhet package is used for sentimant analysis
library("ROAuth")             # 

# Authonitical keys for APIs (Twitter App)
consumer_key <- 'TgfLPu5tUA6BwAfZyYDE0CGh3'
consumer_secret <- 'VL3RoimnCicd17D9Q4oBe7H01WoyXRqNWpSqK0ohTFbW72h3Sn'
access_token <- '953953527545479168-2e5KABkImBdfeJMkSfkm56PC24DcTuz'
access_secret <- 'cpTrvqoxChtQCLvAIg4aTcy2VKg26jHJQei8Ta7HyHZVz'

setup_twitter_oauth(consumer_key, consumer_secret, access_token, access_secret)
tweets <- userTimeline("sachin_rt", n=500)         # I extrxted 500 tweets made by Sachin Tendulkar
n.tweet <- length(tweets)

# Cleaning the tweets for further analysis
tweets.df <- twListToDF(tweets) 
head(tweets.df)
text<-tweets.df$text
head(tweets.df$text)

text1 <- gsub("https.*","",text)
text1 <- gsub("@\\w+", "", text1)
text1<-gsub("[[:punct:]]", "", text1)
text1 <- gsub("#*","",text1)
head(text1)

# Assinging sentimant emotion to each tweet
text2 <- as.vector(text1)
score <- get_nrc_sentiment(text2)         # 'Syuzhet' breaks the tweets into 10 different emotions - 
tweet.data <- cbind(text1, score)         # anger, anticipation, disgust, fear, joy, sadness, surprise, trust, negative and positive.
head(tweet.data)

# Extract sentiment score for each of the tweets
score <- get_sentiment(text2)
max.positive=text2[which(score == max(score))]
max.negative=text2[which(score == min(score))]
max.positive
max.negative

# Sperating positive & Negative tweets
positive.tweets=text2[which(score>0)]
negative.tweets=text2[which(score<0)]
neutral.tweets=text2[which(score==0)]
head(positive.tweets)
head(negative.tweets)
head(neutral.tweets)

category<-ifelse(score > 0, "positive", ifelse(score < 0,"negative","neutral"))

result=cbind(text2,category,score)

table(category)       # Finally I got the sentiment of twitter handler Sachin Tendulkar.
barplot(table(category))
  