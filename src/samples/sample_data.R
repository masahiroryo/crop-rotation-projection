# load packages -------------------------------------------------------------------------------
library(data.table)
library(dtplyr)
library(tidyverse)
library(reshape2)
library(parallel)

# read_functions ----------------------------------------------------------------------------------------
source("./src/preprocessing/read_data.R")

samples = c(100, 1000, 10000, 100000, 1000000, 2000000)

sd = 1
set.seed(seed = sd)

start_time <- Sys.time()
for(n in samples) {
  t1 <- Sys.time()
  
  print(paste("prepping data for", n, "samples"))
  
  data <- read_data(n, exclude_grass=TRUE, exclude_crops=TRUE, include_price=FALSE)
  file_name <- paste( "./data/samples/sample_data_",n,".csv",sep="")
  write.csv(data, file=file_name, row.names = FALSE)
  
  t2 <- Sys.time()
  print(t2-t1)
}
print("overall time:")
end_time <- Sys.time()
print(end_time-start_time)