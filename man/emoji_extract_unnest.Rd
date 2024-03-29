% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/emoji-extraction.R
\name{emoji_extract_unnest}
\alias{emoji_extract_unnest}
\title{Emoji extraction unnested summary}
\usage{
emoji_extract_unnest(tweet_tbl, tweet_text)
}
\arguments{
\item{tweet_tbl}{A dataframe/tibble containing tweets/text.}

\item{tweet_text}{The tweet/text column.}
}
\value{
A summary tibble with the original row number and Emoji count.
}
\description{
If users would like to know how many Emojis and what kinds of Emojis each
Tweet has, \code{emoji_extract} is a useful function to output a global
summary with the row number of each Tweet containing Emoji and the Unicodes
associated with each Tweet.
}
\examples{
library(dplyr)
data.frame(tweets = c("I love tidyverse \U0001f600\U0001f603\U0001f603",
                      "R is my language! \U0001f601\U0001f606\U0001f605",
                      "This Tweet does not have Emoji!",
                      "Wearing a mask\U0001f637\U0001f637\U0001f637.",
                      "Emoji does not appear in all Tweets",
                      "A flag \U0001f600\U0001f3c1")) \%>\%
         emoji_extract_unnest(tweets)

}
