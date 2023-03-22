library(tidytext)
library(textdata)
library(tidyr)
library(dplyr)

setwd("/Users/cameronwoodard/documents")
stocks <- read.csv("stocks2.csv", header=TRUE)
#Put body into own variable so we can keep it when we unnest the tokens
tokenStocks2 <- stocks %>% unnest_tokens(word, body)
stocksSentiments <- tokenStocks2 %>% inner_join(get_sentiments("bing")) %>%
count(title, score, id, url, comms_num, created, timestamp, sentiment) %>%
spread(sentiment, n, fill = 0) %>%
mutate(sentiment = positive - negative)