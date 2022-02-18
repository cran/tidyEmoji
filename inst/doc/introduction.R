## ---- include = FALSE---------------------------------------------------------
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
library(ggplot2)
library(dplyr)

## -----------------------------------------------------------------------------
ata_tweets <- readr::read_csv("ata_tweets.csv")

## -----------------------------------------------------------------------------
ata_tweets %>%
  emoji_summary(full_text)

## -----------------------------------------------------------------------------
ata_tweets %>%
  emoji_extract_nest(full_text) %>%
  select(.emoji_unicode) 

## -----------------------------------------------------------------------------
emoji_count_per_tweet <- ata_tweets %>%
  emoji_extract_unnest(full_text) 

emoji_count_per_tweet

## -----------------------------------------------------------------------------
emoji_count_per_tweet %>%
  group_by(.emoji_count) %>%
  summarize(n = n()) %>%
  ggplot(aes(.emoji_count, n)) +
  geom_col() +
  scale_x_continuous(breaks = seq(1,15)) +
  ggtitle("How many Emoji does each Emoji Tweet have?")

## -----------------------------------------------------------------------------
top_20_emojis <- ata_tweets %>%
  top_n_emojis(full_text)

top_20_emojis

## -----------------------------------------------------------------------------
top_20_emojis %>%
  ggplot(aes(n, emoji_name, fill = emoji_category)) +
  geom_col()

## -----------------------------------------------------------------------------
top_20_emojis %>%
  mutate(emoji_name = stringr::str_replace_all(emoji_name, "_", " "),
         emoji_name = forcats::fct_reorder(emoji_name, n)) %>%
  ggplot(aes(n, emoji_name, fill = emoji_category)) +
  geom_col() +
  labs(x = "# of Emoji",
       y = "Emoji name",
       fill = "Emoji category",
       title = "The 20 most popular Emojis")

## -----------------------------------------------------------------------------
top_20_emojis %>%
  mutate(emoji_name = stringr::str_replace_all(emoji_name, "_", " "),
         emoji_name = forcats::fct_reorder(emoji_name, n)) %>%
  ggplot(aes(n, emoji_name, fill = emoji_category)) +
  geom_col() +
  geom_text(aes(label = unicode), hjust = 0.1) +
  labs(x = "# of Emoji",
       y = "Emoji name",
       fill = "Emoji category",
       title = "The 20 most popular Emojis")

## -----------------------------------------------------------------------------
ata_tweets %>%
  top_n_emojis(full_text, n = 10) %>%
  ggplot(aes(n, emoji_name, fill = emoji_category)) +
  geom_col()

## -----------------------------------------------------------------------------
ata_emoji_category <- ata_tweets %>%
  emoji_categorize(full_text) %>%
  select(.emoji_category)

ata_emoji_category

## -----------------------------------------------------------------------------
ata_emoji_category %>%
  count(.emoji_category) %>%
  filter(n > 20) %>%
  mutate(.emoji_category = forcats::fct_reorder(.emoji_category, n)) %>%
  ggplot(aes(n, .emoji_category)) +
  geom_col()

## -----------------------------------------------------------------------------
ata_emoji_category %>%
  tidyr::separate_rows(.emoji_category, sep = "\\|") %>%
  count(.emoji_category) %>%
  mutate(.emoji_category = forcats::fct_reorder(.emoji_category, n)) %>%
  ggplot(aes(n, .emoji_category)) +
  geom_col()

