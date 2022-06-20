############################################################
# current version doesn't work
#
# TODO: revamp this. 
# save models as .rdata 
# load in here and compare results
############################################################

# load packages -------------------------------------------------------------------------------

library(data.table)
library(dtplyr)
library(tidyverse)
library(ranger)
library(caret)
library(tidymodels)

# read data -----------------------------------------------------------------------------------

split_data <- function(dataset,testyear = 1) {
  years = sort(unique(dataset$Year))
  tail <- years[c((length(years)-testyear+1):length(years))]
  head <- years[1:(length(years)-testyear)]
  train <- dataset %>%
    filter(Year %in% head) %>%
    as.data.table()
  test <- dataset %>%
    filter(Year %in% tail) %>%
    as.data.table()
  return(list(train, test))
}

build_model <- function(model_name) {
  file_name <- paste("./data/clean/", model_name, ".csv", sep="")
  data <- dtplyr::fread(file_name, sep = ",", header = TRUE)
  
  data$State <- as.factor(data$State)
  data$X <- as.numeric(data$X)
  data$Y <- as.numeric(data$Y)
  data$Year <- as.integer(data$Year)
  data$CType <- as.factor(data$CType)
  data$PCType <- as.factor(data$PCType)
  data$PPCType <- as.factor(data$PPCType)
  data$SType <- as.factor(data$SType)
  data$SElev <- as.numeric(data$SElev)
  
  data <- data %>%
    drop_na() %>%
    select(-OBJECTID) %>%
    as.data.table()
  
  split <- split_data(data,t)
  
  train_data <- split[[1]]
  test_data <- split[[2]]
  
  rm(split, data)
  gc()
  
  res <- ranger(CType ~ ., data = train_data,
                importance="impurity",
                mtry = floor(ncol(train_data)/3),
                num.trees=100,
                oob.error = TRUE,
                probability = FALSE,
  )
  gc()
  
  pred <- predict(res, data = test_data)
  
  confusion_matrix <- confusionMatrix(pred$predictions ,test_data$CType)
  (x <- confusion_matrix$table %>% confusionMatrix())
  print(x)
  hmap <- as.data.frame(x$table) %>%
    ggplot(aes(Prediction,Reference, fill=Freq))+
    scale_fill_gradient(low = "white", high = "red")+
    geom_tile()+
    coord_fixed()
  return(hmap)
}

(build_model("data_vanilla"))
(build_model("data_no_lowvalue_crops"))
