# load packages -------------------------------------------------------------------------------

library(data.table)
library(dtplyr)
library(tidyverse)
library(ranger)
library(caret)
library(tidymodels)

# read data -----------------------------------------------------------------------------------
test_years <- 1

# file_name <- "./data/clean/set1.csv"
# file_name <- "./data/clean/set2.csv"
file_name <- "./data/clean/set3.csv"
# file_name <- "./data/clean/set4.csv"

data <- fread(file_name, sep=",", header=TRUE)

data$CType <- as.factor(data$CType)
data$PCType <- as.factor(data$PCType)
data$PPCType <- as.factor(data$PPCType)

set.seed(1)

data <- data %>%
  drop_na() %>%
  select(-c(OBJECTID)) %>%
  as.data.table()

# split data ----------------------------------------------------------------------------------

split_data <- function(dataset,testyear = 3) {
  print(unique(dataset$Year))
  years = sort(unique(dataset$Year))
  print(unique(years))
  tail <- years[c((length(years)-testyear+1):length(years))]
  head <- years[1:(length(years)-testyear)]
  train <- dataset %>%
    filter(Year %in% head) %>%
    as.data.table()
  test <- dataset %>%
    filter(Year %in% tail) %>%
    as.data.table()
  return(list(train,test))
}

split <- split_data(data,test_years)
train_data <- split[[1]]
test_data <- split[[2]]

unique(train_data$Year)
unique(test_data$Year)

# clean workplace -----------------------------------------------------------------------------

rm(split, data)
gc()

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

save(res, file="./output/model3_res.RData")

# evaluate ----------------------------------------------------------------------------------------------
theme_set(theme_bw())
primary_color = '#4477AA'
secondary_color = '#228833'

pred <- predict(res, data = test_data)

confusion_matrix <- confusionMatrix(pred$predictions ,test_data$CType)
(x <- confusion_matrix$table %>% confusionMatrix())
hmap <- as.data.frame(x$table) %>%
  ggplot(aes(Prediction,Reference, fill=Freq))+
  scale_fill_gradient(low = "black", high = secondary_color)+
  geom_tile()+
  coord_fixed()
hmap

pred_train <- predict(res, data = train_data)
cm <- confusionMatrix(pred_train$predictions ,train_data$CType)
(x_train <-cm$table %>% confusionMatrix())
hmap_train <- as.data.frame(x_train$table) %>%
  ggplot(aes(Prediction,Reference, fill=Freq))+
  scale_fill_gradient(low = "white", high = secondary_color)+
  geom_tile()+
  coord_fixed()
hmap_train

var_importance <- as.data.frame(res$variable.importance)
var_importance$ct <- rownames(var_importance)
colnames(var_importance) <- c("vi", "ct")
var_importance <- var_importance %>% 
  arrange(desc(vi))

var_imp <- ggplot(var_importance) + 
  geom_col(mapping = aes(y=vi, x = ct ), fill=primary_color)+
  labs(x="", y="", title="Variable Importance")+
  theme(plot.title = element_text(margin = margin(10, 0, 10, 0),
                                  size = 14))+
  scale_x_discrete(limits=var_importance$ct)
var_imp

class_accuracy <- data.frame(pred$predictions, test_data$CType) %>%
  group_by(pred.predictions) %>%
  summarize(acc = mean(pred.predictions == test_data.CType)) %>%
  arrange(pred.predictions)

classacc <- ggplot(data=class_accuracy) +
  geom_bar(mapping = aes(x=pred.predictions, y=acc), fill = primary_color, stat = "identity")+
  labs(x="Crop Type", y="Accurracy", title="Accurracy for each Class")+
  theme(plot.title = element_text(margin = margin(10, 0, 10, 0),
                                  size = 14))
classacc
