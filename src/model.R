# load packages -------------------------------------------------------------------------------

library(data.table)
library(dtplyr)
library(tidyverse)
library(ranger)
library(caret)
library(tidymodels)

# read data -----------------------------------------------------------------------------------

# data <- fread("./data/clean/sample_bavaria.csv", sep = ",", header = TRUE)
# data <- fread("./data/clean/sample_bavaria_quater.csv", sep = ",", header = TRUE)
# data <- fread("./data/clean/sample_32993.csv", sep = ",", header = TRUE)
# data <- fread("./data/clean/sample_191961.csv", sep = ",", header = TRUE)
# data <- fread("./data/clean/sample_4124.csv", sep=",",header=TRUE)
data <- fread("./data/clean/sample_clean.csv", sep=",", header=TRUE)

data$State <- as.factor(data$State)
data$X <- as.numeric(data$X)
data$Y <- as.numeric(data$Y)
# data$Year <- as.factor(data$Year)
data$Year <- as.integer(data$Year)
data$CType <- as.factor(data$CType)
data$PCType <- as.factor(data$PCType)
data$PPCType <- as.factor(data$PPCType)
data$SType <- as.factor(data$SType)
data$SElev <- as.numeric(data$SElev)

data <- data %>%
  drop_na() %>%
  select(-OBJECTID) %>%
  # select(-price) %>% #test without price data lol
  as.data.table()

# split data ----------------------------------------------------------------------------------

split_data <- function(dataset) {
  print(unique(dataset$Year))
  years = sort(unique(dataset$Year))
  print(unique(years))
  tail <- years[(length(years))]
  head <- years[1:(length(years)-1)]
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

unique(train_data$Year)
unique(test_data$Year)


# clean workplace -----------------------------------------------------------------------------

rm(split)
gc()

set.seed(187)

# model ---------------------------------------------------------------------------------------

t1 <- Sys.time()
res <- ranger(CType ~ ., data = train_data,
              importance="impurity",
              mtry = floor(ncol(train_data)/3),
              num.trees=100,
              oob.error = TRUE,
              probability = FALSE,
)
t2 <- Sys.time()

print(t2-t1)
gc()

# evaluate ----------------------------------------------------------------

pred <- predict(res, data = test_data)

confusion_matrix <- confusionMatrix(pred$predictions ,test_data$CType)
(x <- confusion_matrix$table %>% confusionMatrix())

plot(res$predictions, las = 2, main="Number of predictions per Class")

var_importance <- res$variable.importance
barplot(var_importance[order(var_importance, decreasing = TRUE)], las = 2, main="Variable importance")

# accuracy for each class

class_accuracy <- data.frame(pred$predictions, test_data$CType) %>%
  group_by(pred.predictions) %>%
  summarize(acc = mean(pred.predictions == test_data.CType)) %>%
  arrange(pred.predictions)

ggplot(data=class_accuracy, aes(x=pred.predictions, y=acc)) +
  geom_bar(stat="identity")+
  ggtitle("Accuracy for each class")

save(res,pred,confusion_matrix,class_accuracy, file="./output/ranger_single.RData")

# TSCV --------------------------------------------------------------------
t1 <- Sys.time()

time_slices <- caret::createTimeSlices(unique(data$Year), initialWindow = 1, fixedWindow = FALSE, horizon = 1)
years = sort(unique(data$Year))

results.names <- c("train_length", "accuracy", "kappa")
results <- list(list(), list(), list())
names(results) <- results.names

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

save(results, file="./output/TSCVres.RData")
load(file="./TSCVres.RData")
gc()

df <- data.frame(num_years=unlist(results[[1]]),acc=unlist(results[[2]]), kappa=unlist(results[[3]]) )
df$test <- c(0.1, 0.4, 0.5, 0.55, 0.6, 0.61, 0.62, 0.63, 0.64, 0.65, 0.65, 0.65, 0.65, 0.65)
ggplot(data=df ) +
  geom_line(aes(x=num_years, y=acc, group=1))+
  geom_point(aes(x=num_years, y=acc, group=1))+
  geom_line(aes(x=num_years, y=kappa, group=1), color="blue")+
  geom_point(aes(x=num_years, y=kappa, group=1), color="blue")+
  geom_line(aes(x=num_years, y=test, group=1), color="red")+
  geom_point(aes(x=num_years, y=test, group=1))+
  ylim(0,1)
