library(tidytext)
library(textdata)
library(tidyr)
library(dplyr)

setwd("/Users/cameronwoodard/documents")
stocks <- read.csv("stocks2.csv", header=TRUE)
sp <- Index$new('^GSPC')
#Put body into own variable so we can keep it when we unnest the tokens
tokenStocks2 <- stocks %>% unnest_tokens(word, body)
stocksSentiments <- tokenStocks2 %>% inner_join(get_sentiments("bing")) %>%
count(title, score, id, url, comms_num, created, timestamp, sentiment) %>%
spread(sentiment, n, fill = 0) %>%
mutate(sentiment = positive - negative)
stocksSentiments <- stocksSentiments %>% mutate(direction = ifelse(sentiment > 0, "up", "down"))
stocksSentiments$direction <- factor(stocksSentiments$direction)
stockSentiments$timestamp <- gsub(" .*", "", stockSentiments$timestamp)
#g1 <- glm(direction ~ score + comms_num, family = binomial(), data = stocksSentiments2)

#stock symbol needs to be changed to a single company
addStocksOpen2 <- function(date, whichData) {
  items <- sp$get_history(start = as.Date(date), end = (as.Date(date) + 1), period = '1d')
  if (is.null(items$open)) {
    return(0)
  } else {
    return(items$open)
  }
}


addStocksClose2 <- function(date, whichData) {
  items <- sp$get_history(start = as.Date(date), end = (as.Date(date) + 1), period = '1d')
  if (is.null(items$close)) {
    return(0)
  } else {
    return(items$close)
  }
}

stockSentiments2 <- stockSentiments

#openClose <- stockSentiments2$timestamp
#openClose <- as.data.frame(openClose)
#openClose <- openClose %>% mutate(open = 0, close = 0)
thisOpenClose <- stockSentiments2$timestamp
stockOpen <- c()
stockClose <- c()

for (day in thisOpenClose) {
  print(addStocksClose2(day, open))
  stockClose <- append(stockClose, addStocksClose2(day, data))
}

for (day in thisOpenClose) {
  print(addStocksOpen2(day, open))
  stockOpen <- append(stockOpen, addStocksOpen2(day, data))
}

#for (day in thisOpenClose) {
#  print(addStocksClose2(day, open))
#  stockNumbers2 <- append(stockNumbers2, addStocksClose2(day, data))
#}

#stockSentiments5 <- stockSentiments4 %>% mutate(open = addStocks(timestamp, open), close = addStocks(timestamp, close))
#stockSentiments5 <- stockSentiments4 %>% mutate(open = sp$get_history(start = timestamp, end = (as.Date(timestamp) + 1), period = '1d'), close = timestamp)
stockSentiments2$open <- stockOpen
stockSentiments2$close <- stockClose
stockSentiments3 <- stockSentiments5 %>% mutate(net = close - open)
stockSentiments3 <- stockSentiments6 %>% mutate(BorS = ifelse(net > 0, "buy", "sell")) 
#stockSentiments4 <- subset(stockSentiments3, open == 0 & close == 0)
stockSentiments5 <- subset(stockSentiments3, open != 0 & close != 0)
trainData4 <- stockSentiments5[trainIndex3, ]
testData4 <- stockSentiments5[-trainIndex3, ]
#newStockModel <- glm(BorS ~ score + comms_num + sentiment, family=binomial(), data=stockSentiments8)
#newStockModel <- glm(BorS ~ score + comms_num + sentiment, family=binomial(), data=trainData4)
#newStockModel <- glm(BorS ~ score + comms_num + sentiment, family=binomial(), data=trainData4)
#newStockModelProbs <- predict(newStockModel, testData4, type = "response")
#newStockModelPred <- rep("sell", 182)
#newStockModelPred[newStockModelProbs > .6] <- "buy"
#table(newStockModelPred, testData4)
newStockModel2 <- glm(BorS ~ score + sentiment, family=binomial(), data=trainData4)
newStockModelProbs2 <- predict(newStockModel2, testData4, type = "response")
newStockModelPred2 <- rep("sell", 182)
newStockModelPred2[newStockModelProbs2 > .5] <- "buy"
table(newStockModelPred2, testData4$BorS)

#########################################################################################
library(tidytext)
library(tidyverse)
library(rvest)
library(caret)

# Set the stock symbol and subreddit to scrape
stock_symbol <- "AAPL"
subreddit <- "wallstreetbets"

# Scrape the subreddit for posts about the stock
url <- paste0("https://www.reddit.com/r/", subreddit, "/search?q=", stock_symbol, "&restrict_sr=1")
page <- read_html(url)
titles <- page %>% html_nodes(".title") %>% html_text()

# Calculate the sentiment of the titles
sentiment <- titles %>%
  unnest_tokens(word, text) %>%
  inner_join(get_sentiments("afinn")) %>%
  group_by(text) %>%
  summarize(sentiment = sum(value)) %>%
  ungroup()

# Split the data into training and testing sets
set.seed(123)
trainIndex <- createDataPartition(sentiment$sentiment, p = .8, list = FALSE)
trainData <- sentiment[trainIndex, ]
testData <- sentiment[-trainIndex, ]

# Train a linear regression model on the training data
model <- lm(sentiment ~ ., data = trainData)

# Make predictions on the test data
predictions <- predict(model, testData)

# Evaluate the model's performance
rmse <- RMSE(predictions, testData$sentiment)
rsq <- R2(predictions, testData$sentiment)

# Print the model's performance metrics
cat("RMSE:", rmse, "\n")
cat("R-squared:", rsq)

# Make a prediction on new data
new_data <- data.frame(text = "New post title about the stock")
new_sentiment <- new_data %>%
  unnest_tokens(word, text) %>%
  inner_join(get_sentiments("afinn")) %>%
  group_by(text) %>%
  summarize(sentiment = sum(value)) %>%
  ungroup()
prediction <- predict(model, new_sentiment)

# Print the prediction
print(prediction)