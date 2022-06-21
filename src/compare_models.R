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
  hmap <- as.data.frame(x$table) %>%
    ggplot(aes(Prediction,Reference, fill=Freq))+
    scale_fill_gradient(low = "white", high = "red")+
    geom_tile()+
    coord_fixed()
  hmap
  
  pred_train <- predict(res, data = train_data)
  cm <- confusionMatrix(pred_train$predictions ,train_data$CType)
  (x_train <-cm$table %>% confusionMatrix())
  hmap_train <- as.data.frame(x_train$table) %>%
    ggplot(aes(Prediction,Reference, fill=Freq))+
    scale_fill_gradient(low = "white", high = "red")+
    geom_tile()+
    coord_fixed()
  hmap_train
  
  results <- list(x, x_train, hmap, hmap_train)
  
  return(results)
}

model1 <- build_model("data_vanilla")
model2 <- build_model("data_no_grass")
model3 <- build_model("data_no_lowvalue_crops")
