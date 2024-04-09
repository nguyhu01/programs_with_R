#--- Install and load required packages
install.packages(c("rtweet", "textdata", "syuzhet", "ggplot2", "dplyr"))
library(rtweet)
library(textdata)
library(syuzhet)
library(ggplot2)
library(dplyr)

#--- Set up Twitter API authentication (change the values in the strings)
api_key <- "API_KEY"
api_secret <- "API_SECRET"
access_token <- "ACCESS_TOKEN"
access_secret <- "ACCESS_SECRET"

#--- Authenticate with Twitter API
token <- create_token(
  app = "Twitter_sentiment_analysis",
  consumer_key = api_key,
  consumer_secret = api_secret,
  access_token = access_token,
  access_secret = access_secret
)

#--- Retrieve a number of tweets (e.g., 10,000) containing a specific keyword (change the keyword as needed)
keyword <- "waffle house"
tweets <- search_tweets(keyword, n = 10000, lang = "en", include_rts = FALSE)

#--- Perform sentiment analysis on the tweets
sentiment_scores <- get_nrc_sentiment(tweets$text)

#--- Calculate the average sentiment scores for each emotion
avg_sentiment <- colMeans(sentiment_scores)

#--- Create a data frame with the average sentiment scores
sentiment_df <- data.frame(
  Emotion = names(avg_sentiment),
  Score = avg_sentiment
)

#--- Create a bar plot of the average sentiment scores
ggplot(sentiment_df, aes(x = reorder(Emotion, Score), y = Score, fill = Emotion)) +
  geom_bar(stat = "identity") +
  labs(x = "Emotion", y = "Average Score", title = "Sentiment Analysis of Tweets") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

#--- Retrieve top 10 most retweeted tweets
top_retweets <- tweets %>%
  arrange(desc(retweet_count)) %>%
  head(10)

#--- Print top retweeted tweets
print(top_retweets$text)