library(tidytext)
library(textdata)
library(tidyr)
library(dplyr)
library(caret)
install.packages("devtools")
devtools::install_github("rsquaredacademy/yahoofinancer")
library(yahoofinancer)

setwd("/Users/cameronwoodard/documents")
stocks <- read.csv("stocks2.csv", header=TRUE)
stocks2 <- read.csv("submissions_reddit.csv", header=TRUE)
stocks3 <- subset(stocks2, selftext != "")
stocks3 <- subset(stocks3, selftext != "[deleted]")
stocks3 <- subset(stocks3, selftext != "[removed]")
names(stocks3)[names(stocks3) == 'created'] <- 'timestamp'
names(stocks3)[names(stocks3) == 'selftext'] <- 'body'
names(stocks3)[names(stocks3) == 'num_comments'] <- 'comms_num'
testingStocks <- bind_rows(stocks, stocks3)
testingStocks[ ,c('author', 'retrieved', 'edited', 'pinned', 'archived', 'locked', 'removed', 'deleted', 'is_self', 'is_video', 'is_original_content', 'link_flair_text', 'upvote_ratio', 'gilded', 'total_awards_received', 'num_crossposts', 'thumbnail', 'shortlink')] <- list(NULL)
testingStocks[ ,c('total_awards_received')] <- list(NULL)
sp <- Index$new('^GSPC')
#Put body into own variable so we can keep it when we unnest the tokens
tokenStocks2 <- stocks3 %>% unnest_tokens(word, body)
stocksSentiments <- tokenStocks2 %>% inner_join(get_sentiments("bing")) %>%
count(title, score, id, comms_num, timestamp, sentiment) %>%
spread(sentiment, n, fill = 0) %>%
mutate(sentiment = positive - negative)
stocksSentiments <- stocksSentiments %>% mutate(direction = ifelse(sentiment > 0, "up", "down"))
stocksSentiments$direction <- factor(stocksSentiments$direction)
stocksSentiments$timestamp <- gsub(" .*", "", stocksSentiments$timestamp)
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

stockSentiments2 <- stocksSentiments
splitStocks <- stockSentiments2 %>% sample_frac(.2) ###Replace stock sentiments2 with split stocks from now on

thisOpenClose <- splitStocks$timestamp
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

splitStocks$open <- stockOpen
splitStocks$close <- stockClose
stockSentiments3 <- splitStocks %>% mutate(net = close - open)
stockSentiments4 <- stockSentiments3 %>% mutate(BorS = ifelse(net > 0, "buy", "sell")) 
stockSentiments5 <- subset(stockSentiments4, open != 0 & close != 0)
stockSentiments5$BorS <- factor(stockSentiments5$BorS)
pairs(~ score + comms_num + sentiment + net + BorS, data=stockSentiments6)
trainIndex3 <- createDataPartition(stockSentiments5$sentiment, p = .8, list = FALSE)
trainData4 <- stockSentiments5[trainIndex3, ]
testData4 <- stockSentiments5[-trainIndex3, ]
newStockModel2 <- glm(BorS ~ sentiment + comms_num + score, family=binomial(), data=trainData4)
newStockModelProbs2 <- predict(newStockModel2, testData4, type = "response")
newStockModelPred2 <- rep("down", 755)
newStockModelPred2[newStockModelProbs2 > .5] <- "up"
table(newStockModelPred2, testData4$BorS)
mean(newStockModelPred2 != testData4$BorS)
precision <- posPredValue(factor(newStockModelPred2), testData4$BorS)
recall <- sensitivity(factor(newStockModelPred2), testData4$BorS)
fScore <- (2 * precision * recall) / (precision + recall)
errorAnalysis <- data.frame(Model = "Logistic Regression", accuracy = 0.5682119, RMSE = sqrt (mean (newStockModel2$residuals^2)), precision = precision, recall = recall, fScore = fScore)

Stocks4 <- subset(stocks3, direction != "up")
Stocks5 <- subset(stocks3, direction != "down")
trainIndexNeg <- createDataPartition(Stocks4$sentiment, p = .8, list = FALSE)
trainDataNeg <- Stocks4[trainIndexNeg, ]
testDataNeg <- Stocks4[-trainIndexNeg, ]
newStockModelNeg <- glm(BorS ~ sentiment + comms_num + score, family=binomial(), data=trainDataNeg)
newStockModelProbsNeg <- predict(newStockModel2, testDataNeg, type = "response")
newStockModelPredNeg <- rep("sell", 257)
newStockModelPredNeg[newStockModelProbsNeg > .5] <- "buy"
table(newStockModelPredNeg, testDataNeg$BorS)
mean(newStockModelPredNeg != testDataNeg$BorS)
negPrecision <- posPredValue(factor(newStockModelPredNeg), testDataNeg$BorS) #Precision
negRecall <- sensitivity(factor(newStockModelPredNeg), testDataNeg$BorS) #Recall
negF <- (2 * negPrecision * negRecall) / (negPrecision + negRecall) #F-score

trainIndexPos <- createDataPartition(Stocks5$sentiment, p = .8, list = FALSE)
trainDataPos <- Stocks5[trainIndexPos, ]
testDataPos <- Stocks5[-trainIndexPos, ]
newStockModelPos <- glm(BorS ~ sentiment + comms_num + score, family=binomial(), data=trainDataPos)
newStockModelProbsPos <- predict(newStockModelPos, testDataPos, type = "response")
newStockModelPredPos <- rep("sell", 499)
newStockModelPredPos[newStockModelProbsPos > .5] <- "buy"
table(newStockModelPredPos, testDataPos$BorS)
mean(newStockModelPredPos != testDataPos$BorS)
posPrecision <- posPredValue(factor(newStockModelPredPos), testDataPos$BorS) #Precision
posRecall <- sensitivity(factor(newStockModelPredPos), testDataPos$BorS) #Recall
posF <- (2 * posPrecision * posRecall) / (posPrecision + posRecall)

trainDataBorS <- stocks3[trainIndex3, ]
testDataBorS <- stocks3[-trainIndex3, ]
newStockModelBorS <- glm(direction ~ net + comms_num + score, family=binomial(), data=trainDataBorS)
newStockModelProbsBorS <- predict(newStockModelBorS, testDataBorS, type = "response")
newStockModelPredBorS <- rep("down", 755)
newStockModelPredBorS[newStockModelProbsBorS > .5] <- "up"
table(newStockModelPredBorS, testData4$direction)
mean(newStockModelPredBorS != testData4$direction)
BorSPrecision <- posPredValue(factor(newStockModelPredBorS), testDataBorS$direction)
BorSRecall <- sensitivity(factor(newStockModelPredBorS), testDataBorS$direction)
BorSF <- (2 * BorSPrecision * BorSRecall) / (BorSPrecision + BorSRecall)