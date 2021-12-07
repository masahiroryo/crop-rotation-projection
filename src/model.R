# load packages -------------------------------------------------------------------------------

library(data.table)
library(dtplyr)
library(tidyverse)
library(tidymodels)
library(parsnip)
library(tidypredict)

# read data -----------------------------------------------------------------------------------

# data <- fread("./data/clean/sample_bavaria.csv", sep = ",", header = TRUE)
this is a test commit 

# split data ----------------------------------------------------------------------------------

split_data <- function(dataset, test_size = 2) {
  years <- unique(dataset$Year)
  tail <- years[(length(years)-test_size+1):length(years)]
  head <- years[1:(length(years)-test_size)]
  
  train <- dataset %>% 
    filter(Year %in% head) %>% 
    as.data.table()
  
  test <- dataset %>% 
    filter(Year %in% tail) %>% 
    as.data.table()
  
  return(list(train,test))
}

split <- split_data(data)
train_data <- split[[1]]
test_data <- split[[2]]

# clean workplace -----------------------------------------------------------------------------

rm(data, split)
gc()

# model ---------------------------------------------------------------------------------------

set.seed(187)
res <- rand_forest(mtry = 10, trees = 100) %>%
  set_engine("ranger", importance = "impurity") %>%
  set_mode("regression") %>%
  fit(CType ~ ., data = train_data)

fit <- tidypredict_fit(res)[[1]]
