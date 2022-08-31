library(tidyverse)
library(data.table)
library(dtplyr)
library(reshape2)

sd=1
set.seed(seed = sd)

source("./src/preprocessing/read_data.R")

preprocess_set1 <- function() { 
  data <- read_data(n=2111571,exclude_grass=FALSE, exclude_crops=FALSE, include_price=FALSE)
  file_name <- "./data/clean/set1.csv"
  write.csv(data, file=file_name, row.names = FALSE)
}

preprocess_set2 <- function() { 
  data <- read_data(exclude_grass=TRUE, exclude_crops=TRUE, include_price=FALSE)
  file_name <- "./data/clean/set2.csv"
  write.csv(data, file=file_name, row.names = FALSE)
}

preprocess_set3 <- function() {
  data <- read_data(exclude_grass=TRUE, exclude_crops=TRUE, include_price=TRUE)
  file_name <- "./data/clean/set3.csv"
  write.csv(data, file=file_name, row.names = FALSE)
}

preprocess_set1()
preprocess_set2()
preprocess_set3()
