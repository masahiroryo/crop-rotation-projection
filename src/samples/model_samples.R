# load packages -------------------------------------------------------------------------------

library(data.table)
library(dtplyr)
library(tidyverse)
library(ranger)
library(caret)
library(tidymodels)

# read data -----------------------------------------------------------------------------------

split_data <- function(dataset,testyear = 3) {
  years = sort(unique(dataset$Year))
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

build_model <- function(n, t=1) {
  file_name <- paste("./data/samples/sample_data_",n,".csv",sep="")
  data <- fread(file_name, sep=",", header=TRUE)
  
  data <- data %>%
    drop_na() %>%
    select(-OBJECTID) %>%
    as.data.table()
  
  data$CType <- as.factor(data$CType)
  data$PCType <- as.factor(data$PCType)
  data$PPCType <- as.factor(data$PPCType)
  
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
  res <- confusion_matrix$table %>% confusionMatrix()
  
  return(res$overall[1])
}

# samples =  seq(10000,1000000,25000)
samples = c(100, 1000, 10000, 100000, 1000000, 2000000)

sd=1
set.seed(seed = sd)

results <- vector("list", length=length(samples))
i <- 0
for (n in samples) {
  i=i+1
  print(paste(i, "out of",length(samples)))
  print(paste("building model for", n, "samples"))
  results[i] <- build_model(n,1)
  gc()
}

data <- data.frame(samples=samples, results=unlist(results))
save(data,file="./output/sample_models_low.RData")

# load(file="./output/sample_models_low.RData")

data %>% 
  ggplot()+
  geom_line(aes(x=samples, y=results))+
  geom_point(aes(y=results, x=samples))+
  ylim(0, 1) +
  scale_x_continuous(trans='log10')
