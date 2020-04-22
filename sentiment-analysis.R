#install.packages("rtweet")
#install.packages("dplyr")
#install.packages("tidytext")
#install.packages("tidyr")
#install.packages("ggplot2")
#install.packages("wordcloud")
#install.packages("wordcloud2")
#install.packages("syuzhet")
#install.packages("maptools")
#install.packages("timelineR")

library(rtweet)
library(dplyr)
library(tidytext)
library(tidyr)
library(ggplot2)
library(tm)
library(wordcloud)
library(wordcloud2)
library(syuzhet)
library(maptools)
require(timelineR)


library(wordcloud) #to plot wordcloud
library(RColorBrewer) #for colors in wordcloud
library(tm) #required for some functions
library(stringr) #required for some functions

# Twitter Login
# =============
## TWITTER TOKEN CREATION
#api_key <- "<<your API key>>"
#api_secret_key <- "your API secret"
#access_token <- "your access token"
#access_token_secret <- "your token secret"

## authenticate via web browser
token <- create_token(
  app = "<<Your Twitter App Name>>",
  consumer_key = api_key,
  consumer_secret = api_secret_key,
  access_token = access_token,
  access_secret = access_token_secret)

get_token()

# Scrape Data and Save to CSV File for analysis
# =============================================
bookstoread_tweet <- search_tweets("#bookstoread", n = 2500, include_rts = FALSE, lang = "en")
books_lovers_tweet <- search_tweets("#booklovers", n = 2500, include_rts = FALSE, lang = "en")
book_blogger_tweet <- search_tweets("#bookbloggers", n = 2500, include_rts = FALSE, lang = "en")

books_to_read_tweet = rbind(bookstoread_tweet, books_lovers_tweet, book_blogger_tweet)

books_to_read_df = apply(books_to_read_tweet,2,as.character)

write.csv(books_to_read_df,"bookstoread.csv")

books_to_read_corpus = iconv(books_to_read_tweet$text, to = "utf-8")
books_to_read_corpus_vector = Corpus(VectorSource(books_to_read_corpus))

# Cleaning Data
books_to_read_corpus = tm_map(books_to_read_corpus_vector, tolower)
books_to_read_corpus <-tm_map(books_to_read_corpus, content_transformer(function(x) gsub(x, pattern = "book", replacement = "books")))

books_to_read_corpus = tm_map(books_to_read_corpus, removePunctuation)
#books_to_read_corpus = tm_map(books_to_read_corpus,     removeNumbers)
books_to_read_corpus = tm_map(books_to_read_corpus, removeWords, stopwords("english"))

books_to_read_corpus = tm_map(books_to_read_corpus, removeWords, c("bookstoread", "bookbloggers", "booklovers", "bookblogger", 
                                                                   "bookboost", "booklover", "bookslovers"))

removeURL_func = function(x) gsub("http[[:alnum:]]*", "", x)
books_to_read_corpus = tm_map(books_to_read_corpus, content_transformer(removeURL_func))
books_to_read_cleaned = tm_map(books_to_read_corpus, stripWhitespace)

# HASHTAG WORDCLOUD
# ================

hashtag_dataframe <- data.frame(table(unlist(str_extract_all(books_to_read_tweet$text,"#[a-zA-Z0-9]{2,}"))))
hashtag_dataframe <- hashtag_dataframe[rev(order(hashtag_dataframe$Freq)),]
hashtag_dataframe = na.omit(hashtag_dataframe)
hashtag_dataframe=hashtag_dataframe[9:50,]
wordcloud(hashtag_dataframe$Var1, hashtag_dataframe$Freq, random.order=FALSE,colors=brewer.pal(8, "Dark2"), scale = c(4,.2), rot.per = 0.2)

# WORDCLOUD
# =========
books_to_read_matrix = TermDocumentMatrix(books_to_read_cleaned)
books_to_read_matrix = as.matrix(books_to_read_matrix)

word_count = rowSums(books_to_read_matrix)
word_count = sort(rowSums(books_to_read_matrix), decreasing = TRUE)
set.seed(125)
wordcloud(words = names(word_count), freq = word_count, max.words = 50, random.order = F, min.freq = 40,
          colors = brewer.pal(8, "Dark2"), scale = c(4,.2), rot.per = 0.2)


# SENTIMENT ANALYSIS AND BARPLOT
# ==============================
# Barplot on word Count
word_repeat_count = subset(word_count, word_count > 110)
# Barplot
par(mar=c(15, 3, 3, 1)) 
barplot(word_repeat_count, las = 2, col = rainbow(50))
# Use NRC Dictionary
books_sentiment = get_nrc_sentiment(books_to_read_tweet$text)
colSums(books_sentiment)
# barplot
barplot(colSums(books_sentiment), las = 2, col = rainbow(10), ylab = "Count", main = "Sentiment Scores for Books")

# Top Tweets Screen Name
screenname_corpus = iconv(books_to_read_tweet$screen_name, to = "utf-8")
screenname_corpus = Corpus(VectorSource(screenname_corpus))
screenname_cleaned = tm_map(screenname_corpus, stripWhitespace)
screenname_matrix = TermDocumentMatrix(screenname_cleaned)
screenname_matrix = as.matrix(screenname_matrix)
screename_count = rowSums(screenname_matrix)
screename_count = subset(screename_count, screename_count > 1)
set.seed(222)
wordcloud(words = names(screename_count), freq = screename_count, max.words = 40, random.order = T, min.freq = 10,
          colors = brewer.pal(8, "Dark2"), scale = c(3,1), rot.per = 0.3)

# Tweets over time graph
qplot(books_to_read_tweet$created_at, bins = 100, main = "tweets over time", xlab = "Date", ylab = "Tweet Count", color = "brown")

