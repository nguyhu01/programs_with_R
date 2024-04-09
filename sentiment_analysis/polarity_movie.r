#--- Install and load required packages
install.packages("textdata")
install.packages("tidytext")
install.packages("dplyr")
install.packages("ggplot2")

library(textdata)
library(tidytext)
library(dplyr)
library(ggplot2)

#--- Load the movie review dataset
data(polarity)

#--- Explore the dataset
head(polarity)
table(polarity$sentiment)

#--- Tokenize text data
movie_tokens <- polarity %>%
  unnest_tokens(word, text)

#--- Remove stop words
movie_tokens <- movie_tokens %>%
  anti_join(stop_words)

#--- Calculate word frequencies
word_freq <- movie_tokens %>%
  count(sentiment, word, sort = TRUE) %>%
  ungroup()

#--- Visualize top words for each sentiment
top_words <- word_freq %>%
  group_by(sentiment) %>%
  slice_max(n, n = 10) %>%
  ungroup() %>%
  mutate(word = reorder(word, n))

ggplot(top_words, aes(n, word, fill = sentiment)) +
  geom_col(show.legend = FALSE) +
  facet_wrap(~sentiment, scales = "free_y") +
  labs(x = "Frequency", y = NULL)

#--- Perform sentiment analysis using the "bing" lexicon
bing_word_counts <- movie_tokens %>%
  inner_join(get_sentiments("bing")) %>%
  count(sentiment, index = id, sentiment_bing = sentiment) %>%
  pivot_wider(names_from = sentiment_bing, values_from = n, values_fill = 0) %>%
  mutate(sentiment = ifelse(positive > negative, "Positive", "Negative"))

#--- Compare and evaluate
comparison <- polarity %>%
  select(id, sentiment) %>%
  inner_join(bing_word_counts, by = c("id" = "index"))

accuracy <- sum(comparison$sentiment.x == comparison$sentiment.y) / nrow(comparison)
cat("Accuracy:", accuracy)

#--- Visualize distribution
ggplot(comparison, aes(sentiment.x, fill = sentiment.y)) +
  geom_bar(position = "dodge") +
  labs(x = "Original Sentiment", y = "Count", fill = "Predicted Sentiment")