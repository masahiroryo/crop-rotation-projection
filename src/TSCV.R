# load packages -------------------------------------------------------------------------------

library(data.table)
library(dtplyr)
library(tidyverse)
library(ranger)
library(caret)
library(tidymodels)

# read data -----------------------------------------------------------------------------------

data <- fread("./data/clean/sample_bavaria_half.csv", sep = ",", header = TRUE)

# prepare data for model building -----------------------------------------

data$CType <- dplyr::recode(data$CType, # makes it easier to understand
                            `70` = 20L,
                            `80` = 21L) 

data <- data %>%                              # NAs will be dropped for the model 
  mutate(price = replace_na(price, 0)) %>%    # But removing so much data from the time series
  as.data.table()                             # Would break the model

data <- data %>% 
  group_by(OBJECTID) %>%
  mutate(temp = mean(CType)) %>% # remove all data points with only grassland in it
  filter(temp != 18) %>%         # grassland is not interesting for the model
  select(-temp) %>%
  as.data.table()

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

set.seed(187)

# model building ----------------------------------------------------------

time_slices <- caret::createTimeSlices(unique(data$Year), initialWindow = 1, fixedWindow = FALSE, horizon = 1)
years = sort(unique(data$Year))

results.names <- c("train_length", "accuracy", "kappa")
results <- list(list(), list(), list())
names(results) <- results.names

t1 <- Sys.time()
for(i in 1:(length(time_slices[[1]]))) {
  print(i)
  data_train <- data %>% 
    filter(Year %in% years[time_slices[[1]][[i]]]) %>% 
    as.data.table()
  data_test <- data %>% 
    filter(Year %in% years[time_slices[[2]][[i]]]) %>% 
    as.data.table()
  
  rf <- ranger(CType ~ ., data = data_train,
               importance="impurity",
               mtry = floor(ncol(data_train)/3),
               num.trees=100,
               oob.error = TRUE,
               probability = FALSE,
  )
  
  pred <- predict(rf, data = data_test)
  confusion_matrix <- confusionMatrix(pred$predictions ,data_test$CType)
  res <- confusion_matrix$table %>% confusionMatrix()
  
  var_importance <- rf$variable.importance
  barplot(var_importance[order(var_importance, decreasing = TRUE)], las = 2)
  
  class_accuracy <- data.frame(pred$predictions, data_test$CType) %>%
    group_by(pred.predictions) %>%
    summarize(acc = mean(pred.predictions == data_test.CType)) %>%
    arrange(pred.predictions)
  
  ggplot(data=class_accuracy, aes(x=pred.predictions, y=acc)) +
    geom_bar(stat="identity")
  
  results$train_length <- c(results$train_length, length(time_slices[[1]][[i]]))
  results$accuracy <- c(results$accuracy, res$overall[1]) 
  results$kappa <- c(results$kappa, res$overall[2])  
  
  print(paste(i, "-- done"))
}
t2 <- Sys.time()

print(t2-t1)
gc()

df <- data.frame(num_years=unlist(results[[1]]),acc=unlist(results[[2]]))
df$test <- c(0.1, 0.4, 0.5, 0.55, 0.6, 0.61, 0.62, 0.63, 0.64, 0.65, 0.65, 0.65, 0.65, 0.65) # random stuff i just came up with so it looks pretty
ggplot(data=df ) +
  geom_line(aes(x=num_years, y=acc, group=1))+
  geom_point(aes(x=num_years, y=acc, group=1))+
  geom_line(aes(x=num_years, y=test, group=1), color="red")+
  geom_point(aes(x=num_years, y=test, group=1))+
  ylim(0,1)
