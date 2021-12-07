# load packages -------------------------------------------------------------------------------

library(data.table)
library(dtplyr)
library(tidyverse)

# read data -----------------------------------------------------------------------------------

# data <- fread("./data/clean/sample_bavaria.csv", sep = ",", header = TRUE)
data <- fread("./data/clean/sample206934.csv", sep = ",", header = TRUE)
gc()

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
test <- split[[1]]
train <- split[[2]]