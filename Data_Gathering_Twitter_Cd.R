
library(rtweet)
library(dplyr)
library(tidyr)
library(tidytext)
library(twitteR)
library(tidyverse)
app_name <- 'rstatsbrandsamalysis'
api_key <- 'bzIvz68ghK5JBYH3HBvizXmbv'
api_secret <- 'fdtlIW69ssD3zkJSj64VbKME6h48gqE6fuYkMAMjZKxRc1KnQg'
access_token <- 'AAAAAAAAAAAAAAAAAAAAAERGSgEAAAAAzy4XvecowqFhuO1PZbaWJ35lLlc%3DE2GPTQBxYCSna3PMjbEnS8HlqShhzBSFG6Z4kNoepnZllCZHAB'
topic_1 <- search_tweets('@ExideCare', n = 3200, include_rts = FALSE)
tweets.1 <- topic_1 %>% select(screen_name, text)
tweets.1

tweets.1$stripped_text1 <- gsub('https\\S+',"" ,tweets.1$text)
head(tweets.1$stripped_text1)
data <- data.frame(tweets.1$screen_name, tweets.1$stripped_text1)
library(tm)
library(NLP)
library(SnowballC)
library(magrittr)
library(tidytext)
install.packages("writexl")
library('writexl')
write_xlsx(data, "/Users/vishalsharma/Downloads/Machine Learning Books//Exide24_Aug.xlsx")
//stop()
setwd("/Users/vishalsharma/Downloads/Personal/R Folder")
df <- read.csv("ExideTweets.csv",stringsAsFactors = F)
my_corpus <-VCorpus(DataframeSource(data))
my_corpus
writeLines(head(strwrap(my_corpus), 15))
addspace <- content_transformer(function(x, pattern) {
  return(gsub(pattern, " ", x))
})
my_corpus <- tm_map(my_corpus, addspace, "-")
writeLines(head(strwrap(my_corpus[[2]]), 15))
my_corpus <- tm_map(my_corpus, removePunctuation)
my_corpus <- tm_map(my_corpus, removeWords, stopwords("english"))
writeLines(head(strwrap(my_corpus[[14]]), 15))
my_corpus <- tm_map(my_corpus,content_transformer(tolower))
my_corpus <- tm_map(my_corpus, stripWhitespace)
dtm <-TermDocumentMatrix(my_corpus)
m <-as.matrix(dtm)
v <-sort(rowSums(m), decreasing = TRUE)
d <-data.frame(word = names(v), freq = v)
head(d, 50)
install.packages("wordcloud")
install.packages("RColorBrewer")
library(wordcloud)
library(RColorBrewer)
set.seed(1234)
wordcloud(words = d$word, freq = d$freq, min.freq = 1,
          max.words=200, random.order=FALSE, rot.per=0.35, 
          colors=brewer.pal(8, "Dark2"))
text <- tokens %>%
  inner_join(get_sentiments("bing")) %>% # pull out only sentiment words
  count(sentiment) %>% # count the # of positive & negative words
  spread(sentiment, n, fill = 0)
install.packages("syuzhet")
library(syuzhet)
syuzhet_vector <- get_sentiment(my_corpus, method="syuzhet")
head(syuzhet_vector, 40)
summary(syuzhet_vector)
head(my_corpus, 30)
get_nrc_sentiment(my_corpus)
as.character(my_corpus[1,])
str(my_corpus)
my_corpus
str_split(my_corpus,pattern = "\\s+")
jj <- str_split(my_corpus,pattern = "\\s+")
get_nrc_sentiment(jj)
changed_jj <- as.character(jj)
get_nrc_sentiment(changed_jj)
syuzhet_vector <- get_sentiment(changed_jj, method = "syuzhet")
head(syuzhet_vector)
summary(syuzhet_vector)