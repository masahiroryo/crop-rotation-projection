
library(data.table)
library(dtplyr)
library(tidyverse)
library(reshape2)

sd=1
set.seed(seed = sd)

source("./src/preprocessing/read_data.R")

preprocess_vanilla <- function() { 
  data <- read_data(exclude_grass=FALSE, exclude_crops=FALSE, include_price=FALSE)
  file_name <- "./data/clean/data_vanilla.csv"
  write.csv(data, file=file_name, row.names = FALSE)
}

preprocess_no_grass <- function() { 
  data <- read_data(exclude_grass=TRUE, exclude_crops=FALSE, include_price=FALSE)
  file_name <- "./data/clean/data_no_grass.csv"
  write.csv(data, file=file_name, row.names = FALSE)
}

preprocess_no_lowvalue_crops <- function() { 
  data <- read_data(exclude_grass=FALSE, exclude_crops=TRUE, include_price=FALSE)
  file_name <- "./data/clean/data_no_lowvalue_crops.csv"
  write.csv(data, file=file_name, row.names = FALSE)
}

preprocess_no_lowvalue_crops_with_price <- function() { #doesnt work currently. price problemos
  data <- read_data(exclude_grass=FALSE, exclude_crops=TRUE, include_price=TRUE)
  file_name <- "./data/clean/data.csv"
  write.csv(data, file=file_name, row.names = FALSE)
}

preprocess_vanilla()
preprocess_no_grass()
preprocess_no_lowvalue_crops()