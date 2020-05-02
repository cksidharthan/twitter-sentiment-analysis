setwd("~/Documents/Programming/R Programming/twitter-analytics-project")

library(rtweet)
library(tmaptools)
library(leaflet)
library(tidyverse)

library(tidytext)
library(textdata)
library(plotly)
library(ggwordcloud)
library(tidyquant)

# Twitter API Authentication
api_key <- "ZoFcbnA7JGqYWKRo5rc27v2JW"
api_secret_key <- "COi030ksGl6glIlMongUBL3GvP4XtynTP3CfrfcN9BUhNymGY0"
access_token <- "1225901758620278784-azx3OOiFce0JRER4NlSBLft6UUckHw"
access_token_secret <- "rGA4wi0KqwY548DQQdHQG5BNUgR"

token <- create_token(
  app = "HashtagAnalyticsProject",
  consumer_key = api_key,
  consumer_secret = api_secret_key,
  access_token = access_token,
  access_secret = access_token_secret)
get_token()

# Get User Inputs
hashtag <- readline(prompt = "Enter the search Query: ")
tweet_count <- as.numeric(readline(prompt = "Enter the tweet count: "))

# Search Tweets
tweets_tbl <- rtweet::search_tweets(
  q = hashtag,
  n = tweet_count,
  include_rts = FALSE,
  lang = "en"
)

write_rds(tweets_tbl, "tweets.rds")

tweets_tbl = read_rds("tweets.rds")

# Tidy the data
tweets_tokenized_tbl <- tweets_tbl %>%
  select(text) %>%
  rowid_to_column() %>% # adds rowid as a column
  unnest_tokens(word, text)

# Count the words in tweet
tweets_tokenized_tbl %>% count(word, sort = TRUE)

# sentiment analysis on tokenized words
sentiment_bing_tbl <- tweets_tokenized_tbl %>%
  inner_join(get_sentiments("bing"))

# count the total number of positive and negative sentiment.
sentiment_bing_tbl %>% count(sentiment)

# map the sentiment with the tweets as per the row id
sentiment_by_row_id_tbl <- sentiment_bing_tbl %>%
  select(-word) %>%
  count(rowid, sentiment) %>%
  pivot_wider(names_from = sentiment, values_from = n, values_fill = list(n = 0)) %>%
  mutate(sentiment = positive-negative) %>%
  left_join(tweets_tbl %>% select(screen_name, text) %>% rowid_to_column())

# Visualization
label_wrap <- label_wrap_gen(width = 60)

data_formatted <- sentiment_by_row_id_tbl %>%
  mutate(text_formatted = str_glue("Row ID: {rowid}
                                   Screen Name: {screen_name}
                                   Text: {label_wrap(text)}"))

plot <- data_formatted %>%
  ggplot(mapping = aes(x = rowid, y = sentiment)) +
  geom_line(color = "blue", alpha = 0.5) +
  geom_point(mapping = aes(text = text_formatted), color = "black") +
  geom_smooth(method = "loess", se = FALSE, span = 0.25, color = "red") + 
  geom_hline(aes(yintercept = mean(sentiment)), color = "cornflowerblue") +
  geom_hline(aes(yintercept = median(sentiment) + 0.67*IQR(sentiment)), color ="red") +
  geom_hline(aes(yintercept = median(sentiment) - 0.67*IQR(sentiment)), color ="red") +
  theme_tq() + 
  labs(
    title = "Bing Sentiment Graph",
    subtitle = "Bing sentiment Graph of hashtag",
    x = "twitter user",
    y = "sentiment"
  )

# plot
ggplotly(plot, tooltip = "text") %>%
  # Adding slider
  layout(
    xaxis = list(
      rangeslider = list(type = "date")
    )
  )

sentiment_by_word_tbl <- sentiment_bing_tbl %>%
  count(word, sentiment, sort = TRUE)

sentiment_by_word_tbl %>%
  slice(1:100) %>%
  mutate(sentiment = factor(sentiment, levels = c("positive", "negative"))) %>%
  ggplot(aes(label = word, color = sentiment, size = n)) +
  geom_text_wordcloud_area() +
  facet_wrap(~ sentiment, nrow = 2) + 
  theme_tq_dark() +
  scale_color_tq() + scale_size_area(max_size = 16) +
  labs(title = "sentiment word frequency")
