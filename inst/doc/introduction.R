## ----include = FALSE----------------------------------------------------------
knitr::opts_chunk$set(
  collapse = TRUE,
  message = FALSE,
  warning = FALSE,
  fig.width = 8,
  fig.height = 5,
  comment = "#>"
)

## ----setup--------------------------------------------------------------------
library(tidyEmoji)
library(dplyr)
library(ggplot2)

## -----------------------------------------------------------------------------
ata_tweets <- readr::read_csv("ata_tweets.rda", show_col_types = FALSE)
ata_tweets

## -----------------------------------------------------------------------------
summary_tbl <- ata_tweets %>%
  emoji_summary(full_text)

summary_tbl

## -----------------------------------------------------------------------------
ata_tweets %>%
  emoji_filter(full_text)

## -----------------------------------------------------------------------------
ata_tweets %>%
  emoji_extract_nest(full_text) %>%
  select(full_text, .emoji_unicode)

## -----------------------------------------------------------------------------
emoji_per_tweet <- ata_tweets %>%
  emoji_extract_unnest(full_text)

emoji_per_tweet

## ----fig.alt = "Bar chart of the number of emoji per emoji-bearing tweet. The vast majority of tweets contain a single emoji, with a long, thin tail of more emoji-heavy tweets."----
emoji_per_tweet %>%
  group_by(row_number) %>%
  summarise(n_emoji = sum(.emoji_count)) %>%
  ggplot(aes(n_emoji)) +
  geom_bar() +
  scale_x_continuous(breaks = seq(1, 15)) +
  labs(x = "Number of emoji in the tweet",
       y = "Number of tweets",
       title = "Most emoji tweets contain a single emoji")

## -----------------------------------------------------------------------------
ata_tweets %>%
  emoji_tokens(full_text)

## -----------------------------------------------------------------------------
demo <- data.frame(
  text = c("our family \U0001F468‍\U0001F469‍\U0001F467‍\U0001F466",
           "great work \U0001F44D\U0001F3FD")
)

demo %>%
  emoji_extract_unnest(text)

## -----------------------------------------------------------------------------
ata_tweets %>%
  emoji_frequency(full_text)

## -----------------------------------------------------------------------------
top_20_emojis <- ata_tweets %>%
  top_n_emojis(full_text)

top_20_emojis

## ----fig.alt = "Horizontal bar chart of the 20 most frequent emoji in the corpus, coloured by Unicode category."----
top_20_emojis %>%
  mutate(emoji_name = stringr::str_replace_all(emoji_name, "_", " "),
         emoji_name = forcats::fct_reorder(emoji_name, n)) %>%
  ggplot(aes(n, emoji_name, fill = emoji_category)) +
  geom_col() +
  labs(x = "Count",
       y = NULL,
       fill = "Category",
       title = "The 20 most frequent emoji")

## ----fig.alt = "Horizontal bar chart of the 10 most frequent emoji in the corpus, coloured by Unicode category."----
ata_tweets %>%
  top_n_emojis(full_text, n = 10) %>%
  mutate(emoji_name = stringr::str_replace_all(emoji_name, "_", " "),
         emoji_name = forcats::fct_reorder(emoji_name, n)) %>%
  ggplot(aes(n, emoji_name, fill = emoji_category)) +
  geom_col() +
  labs(x = "Count", y = NULL, fill = "Category",
       title = "The 10 most frequent emoji")

## -----------------------------------------------------------------------------
ata_emoji_category <- ata_tweets %>%
  emoji_categorize(full_text) %>%
  select(.emoji_category)

ata_emoji_category

## ----fig.alt = "Horizontal bar chart of the most common emoji category combinations that appear in more than 20 tweets."----
ata_emoji_category %>%
  count(.emoji_category, sort = TRUE) %>%
  filter(n > 20) %>%
  mutate(.emoji_category = forcats::fct_reorder(.emoji_category, n)) %>%
  ggplot(aes(n, .emoji_category)) +
  geom_col() +
  labs(x = "Number of tweets", y = NULL,
       title = "Most common emoji category combinations")

## ----fig.alt = "Horizontal bar chart of how often each individual Unicode emoji category is used, dominated by Smileys & Emotion followed by People & Body."----
ata_emoji_category %>%
  tidyr::separate_rows(.emoji_category, sep = "\\|") %>%
  count(.emoji_category, sort = TRUE) %>%
  mutate(.emoji_category = forcats::fct_reorder(.emoji_category, n)) %>%
  ggplot(aes(n, .emoji_category)) +
  geom_col() +
  labs(x = "Number of tweets", y = NULL,
       title = "Emoji category usage")

## -----------------------------------------------------------------------------
ata_sentiment <- ata_tweets %>%
  emoji_sentiment(full_text)

ata_sentiment %>%
  select(.emoji_n, .emoji_sentiment)

## ----fig.alt = "Histogram of the mean emoji sentiment per tweet, which is concentrated on the positive side of the scale."----
ata_sentiment %>%
  filter(!is.na(.emoji_sentiment)) %>%
  ggplot(aes(.emoji_sentiment)) +
  geom_histogram(binwidth = 0.1) +
  labs(x = "Mean emoji sentiment",
       y = "Number of tweets",
       title = "Emoji sentiment skews positive")

## ----fig.alt = "Horizontal bar chart of the average emoji sentiment within each Unicode category."----
ata_tweets %>%
  emoji_tokens(full_text) %>%
  group_by(.emoji_category) %>%
  summarise(mean_sentiment = mean(.emoji_sentiment, na.rm = TRUE),
            n_scored = sum(!is.na(.emoji_sentiment))) %>%
  filter(n_scored > 0) %>%
  mutate(.emoji_category = forcats::fct_reorder(.emoji_category, mean_sentiment)) %>%
  ggplot(aes(mean_sentiment, .emoji_category)) +
  geom_col() +
  labs(x = "Mean sentiment", y = NULL,
       title = "Average emoji sentiment by category")

## -----------------------------------------------------------------------------
emoji_sentiment_lexicon %>%
  filter(occurrences >= 500) %>%
  slice_max(sentiment_score, n = 8) %>%
  select(emoji, unicode_name, occurrences, sentiment_score)

emoji_sentiment_lexicon %>%
  filter(occurrences >= 500) %>%
  slice_min(sentiment_score, n = 8) %>%
  select(emoji, unicode_name, occurrences, sentiment_score)

