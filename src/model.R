# load packages -------------------------------------------------------------------------------

library(data.table)
library(dtplyr)
library(tidyverse)
library(ranger)
library(caret)

# read data -----------------------------------------------------------------------------------

# data <- fread("./data/clean/sample_bavaria.csv", sep = ",", header = TRUE)
data <- fread("./data/clean/sample_206934.csv", sep = ",", header = TRUE)

data$State <- as.factor(data$State)
data$X <- as.numeric(data$X)
data$Y <- as.numeric(data$Y)
data$Year <- as.factor(data$Year)
data$CType <- as.factor(data$CType)
data$PCType <- as.factor(data$PCType)
data$PPCType <- as.factor(data$PPCType)
data$SType <- as.factor(data$SType)
data$SElev <- as.numeric(data$SElev)

data <- data %>% drop_na()

# split data ----------------------------------------------------------------------------------

split_data <- function(dataset) {
  dataset <- dataset %>% select(-OBJECTID) %>% as.data.table()
  years <- unique(dataset$Year)
  tail <- years[(length(years)-2):(length(years)-1)]
  head <- years[1:(length(years)-2)]
  rest <- years[length(years)]
  
  train <- dataset %>% 
    filter(Year %in% head) %>% 
    as.data.table()
  
  test <- dataset %>% 
    filter(Year %in% tail) %>% 
    as.data.table()
  
  eval <- dataset %>% 
    filter(Year %in% rest) %>% 
    as.data.table()
  
  return(list(train,test,eval))
}

split <- split_data(data)
train_data <- split[[1]]
test_data <- split[[2]]
eval_data <- split[[3]]

# clean workplace -----------------------------------------------------------------------------

rm(data, split)
gc()

# model ---------------------------------------------------------------------------------------

res <- ranger(CType ~ ., data = train_data,
              importance="impurity",
              oob.error = TRUE,
              seed = 187,
              probability = FALSE,
              num.trees=100
)

pred <- predict(res, data = test_data)

gc()

mat <- confusionMatrix(pred$predictions ,test_data$CType)
mat$table %>% confusionMatrix()

plot(res$predictions, las = 2)

imp <- res$variable.importance
barplot(imp[order(imp, decreasing = TRUE)], las = 2)
