stock <- read.csv("D://R/Machine Learning/Stock Market Prediction Using random Forest/istanbul_stock_exchange.csv")
# print(head(stock))
# print(str(stock))
# print(summary(stock))

## DATA MANIPULATION ##
library(dplyr)

max.exchange <- filter(stock, TL.BASED.ISE > 0 & USD.BASED.ISE > 0)
print(head(max.exchange))

for(i in 1:dim(stock)) {
  if(stock$TL.BASED.ISE[i] < 0 && stock$USD.BASED.ISE[i] < 0) {
    stock$Result[i] <- 'Loss'
  } else {
    stock$Result[i] <- 'Profit'
  }
}

print(stock$Result)

## DATA CLEANING ##
print(is.na(stock))
print(sum(is.na(stock$date)))
print(sum(is.na(stock$TL.BASED.ISE)))
print(sum(is.na(stock$USD.BASED.ISE)))
print(sum(is.na(stock$SP)))
print(sum(is.na(stock$DAX)))
print(sum(is.na(stock$FTSE)))
print(sum(is.na(stock$NIKKEI)))
print(sum(is.na(stock$BOVESPA)))
print(sum(is.na(stock$EU)))
print(sum(is.na(stock$EM)))

## DATA VISUALIZATION ##
library(ggplot2)
library(plotly)

pl <- ggplot(max.exchange, aes(TL.BASED.ISE, USD.BASED.ISE)) + geom_point(color='red', alpha=0.5)
pl2 <- ggplotly(pl)
# print(pl2)

p <- ggplot(stock, aes(USD.BASED.ISE)) + geom_histogram(aes(fill=Result), color='black', bins=50, alpha=0.6) + theme_bw() + xlab('USD Limit') + ylab('Count') + ggtitle('Market Share')
# print(p)

plot <- ggplot(stock, aes(x=stock$TL.BASED.ISE, y=stock$USD.BASED.ISE))
plot2 <- plot + geom_hex() + xlab('Trade Limit') + ylab('USD Limit') + ggtitle('Stock Trade v USD')
# print(plot2 + scale_fill_gradient(high='red', low='blue'))

## APPLYING ML ##
## TRAIN TEST SPLIT ##
library(caTools)
set.seed(101)

sample <- sample.split(stock$Result, SplitRatio = 0.70)
train <- subset(stock, sample==T)
test <- subset(stock, sample==F)

## TRAIN MODEL ##
library(randomForest)
rf.model <- randomForest(as.factor(Result) ~ ., data = train, importance = TRUE)
# print(rf.model$confusion)
# print(rf.model$importance)

## PREDICTIONS ##
rf.preds <- predict(rf.model, test)
print(table(rf.preds, test$Result))














# TL BASED ISE (TRADE LIMIT BASED ISTANBUL STOCK EXCHANGE)
# USD BASED ISE (US DOLLAR BASED ISE)
# SP (STANDARD AND POOR'S)
# DAX (IT'S A PERFORMANCE INDEX)
# FTSE (FINANCIAL TIME STOCK EXCHANGE)
# NIKKEI (STOCK AVG)
# BEVASPA (BENCHMARK INDEX)
# EU (EUREOPEAN UNIT)
# EM (EMERGING MARKET)
