# load packages -------------------------------------------------------------------------------
library(data.table)
library(dtplyr)
library(tidyverse)
library(reshape2)
library(parallel)

# read_functions ----------------------------------------------------------------------------------------
source("./src/preprocessing/read_data.R")

samples =  seq(25000,2000000,25000)

sd = 1
set.seed(seed = sd)

for(n in samples) {
  start_time <- Sys.time()
  
  print(paste("prepping data for", n, "samples"))
  
  data <- read_data(n, include_price=FALSE, exclude_grass=FALSE, exclude_crops=FALSE)
  file_name <- paste( "./data/samples/sample_data_",n,".csv",sep="")
  write.csv(data, file=file_name, row.names = FALSE)
  
  end_time <- Sys.time()
  print(end_time-start_time)
}