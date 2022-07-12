library(tidyverse)
library(data.table)
library(dtplyr)
library(ranger)
library(caret)


source("./src/bench/dplyr/dplyr_read_data.R")
source("./src/bench/dplyr/dplyr_split_data.R")

source("./src/bench/dtable/dtable_read_data.R")
source("./src/bench/dtable/dtable_split_data.R")

source("./src/bench/model.R")
source("./src/bench/eval.R")


all_dplyr <- function(n=10000) {
  t1 <- Sys.time()
  
  data <- dplyr_read_data(n=n, exclude_grass=FALSE, exclude_crops=FALSE,include_price=FALSE)
  
  split <- dplyr_split_data(data,1)
  train_data <- split[[1]]
  test_data <- split[[2]]
  
  model <- model(data, train_data, test_data)
  eval <- eval(model, test_data)
  
  t2 <- Sys.time()
  return(as.numeric(difftime(t1, t2, units="mins")))
}

all_dtable <- function(n=10000) {
  t1 <- Sys.time()
  data <- dtable_read_data(n=n, exclude_grass=FALSE, exclude_crops=FALSE,include_price=FALSE)
  
  split <- dtable_split_data(data,1)
  train_data <- split[[1]]
  test_data <- split[[2]]
  
  model <- model(data, train_data, test_data)
  eval <- eval(model, test_data)
  t2 <- Sys.time()
  return(as.numeric(difftime(t2, t1, units="mins")))
}

set.seed(1)
samples <- c(100, 1000, 10000, 50000, 100000, 250000, 500000, 1000000)
df <- data.frame("dplyr"=0, "dtable"=0, samples)

for(n in 1:length(samples)) {
  df[n, 1] <- all_dplyr(samples[n])
  df[n, 2] <- all_dtable(samples[n])
}

df %>% 
  ggplot(aes(x=samples))+
  geom_line(aes(y=dplyr), color = "green")+
  geom_line(aes(y=dtable), color = "blue")+
  ylab("Time")

(df)

save(df, file="./output/bench.RData")
