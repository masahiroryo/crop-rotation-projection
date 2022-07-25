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
  gc()
  file_name <- paste("./data/clean/", model_name, ".csv", sep="")
  data <- fread(file_name, sep = ",", header = TRUE)
  
  data <- data %>%
    drop_na() %>%
    select(-OBJECTID) %>%
    as.data.table()
  data$CType <- as.factor(data$CType)
  data$PCType <- as.factor(data$PCType)
  data$PPCType <- as.factor(data$PPCType)
  
  split <- split_data(data)
  
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
  
  class_accuracy <- data.frame(pred$predictions, test_data$CType) %>%
    group_by(pred.predictions) %>%
    summarize(acc = mean(pred.predictions == test_data.CType)) %>%
    arrange(pred.predictions)
  
  var_importance <- as.data.frame(res$variable.importance)
  var_importance$ct <- rownames(var_importance)
  colnames(var_importance) <- c("vi", "ct")
  var_importance <- var_importance %>% 
    arrange(vi)
  
  results <- list(x, x_train, hmap, hmap_train, class_accuracy, var_importance)
  
  return(results)
}

model1 <- build_model("set1")
save(model1, file="./output/model1.RData")
model2 <- build_model("set2")
save(model2, file="./output/model2.RData")
model3 <- build_model("set3")
save(model3, file="./output/model3.RData")
model4 <- build_model("set4")
save(model4, file="./output/model4.RData")

# viz 
theme_set(theme_bw())
primary_color = '#4477AA'
secondary_color = '#228833'

load(file="./output/model1.RData")
classacc <- ggplot(data=model1[[5]]) +
  geom_bar(mapping = aes(x=pred.predictions, y=acc), fill = primary_color, stat = "identity")+
  labs(x="Crop Type", y="Accurracy", title="Accurracy for each Class")+
  theme(plot.title = element_text(margin = margin(10, 0, 10, 0),
                                  size = 14))

png(filename="./output/class_accuracy_model1.png")
plot(classacc)
dev.off()

png(filename="./output/heatmap_model1.png")
plot(hmap)
dev.off()

var_imp <- ggplot(model1[6]) + 
  geom_bar(mapping = aes(x = ct, y=vi), fill=primary_color, stat="identity")+
  labs(x="", y="", title="Variable Importance")+
  theme(plot.title = element_text(margin = margin(10, 0, 10, 0),
                                  size = 14))+
  scale_x_discrete(limits=var_importance$ct)
png(filename="./output/variable_importance_model1.png")
plot(var_imp)
dev.off()



load(file="./output/model2.RData")
classacc <- ggplot(data=model2[[5]]) +
  geom_bar(mapping = aes(x=pred.predictions, y=acc), fill = primary_color, stat = "identity")+
  labs(x="Crop Type", y="Accurracy", title="Accurracy for each Class")+
  theme(plot.title = element_text(margin = margin(10, 0, 10, 0),
                                  size = 14))

png(filename="./output/class_accuracy_model2.png")
plot(classacc)
dev.off()




load(file="./output/model3.RData")
classacc <- ggplot(data=model3[[5]]) +
  geom_bar(mapping = aes(x=pred.predictions, y=acc), fill = primary_color, stat = "identity")+
  labs(x="Crop Type", y="Accurracy", title="Accurracy for each Class")+
  theme(plot.title = element_text(margin = margin(10, 0, 10, 0),
                                  size = 14))

png(filename="./output/class_accuracy_model3.png")
plot(classacc)
dev.off()




load(file="./output/model4.RData")
classacc <- ggplot(data=model4[[5]]) +
  geom_bar(mapping = aes(x=pred.predictions, y=acc), fill = primary_color, stat = "identity")+
  labs(x="Crop Type", y="Accurracy", title="Accurracy for each Class")+
  theme(plot.title = element_text(margin = margin(10, 0, 10, 0),
                                  size = 14))

png(filename="./output/class_accuracy_model4.png")
plot(classacc)
dev.off()

png(filename="./output/heatmap_model4.png")
plot(hmap)
dev.off()

var_imp <- ggplot(model4[6]) + 
  geom_bar(mapping = aes(x = ct, y=vi), fill=primary_color, stat="identity")+
  labs(x="", y="", title="Variable Importance")+
  theme(plot.title = element_text(margin = margin(10, 0, 10, 0),
                                  size = 14))+
  scale_x_discrete(limits=var_importance$ct)

png(filename="./output/variable_importance_model4.png")
plot(var_imp)
dev.off()