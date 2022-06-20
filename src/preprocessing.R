# load packages -------------------------------------------------------------------------------

library(data.table)
library(dtplyr)
library(tidyverse)
library(reshape2)

sd=1
set.seed(seed = sd)

# read ref data -------------------------------------------------------------------------------
t1 <- Sys.time()

source("./src/preprocessing/read_data.R")
data <- read_data(exclude_grass=FALSE, exclude_crops=TRUE, include_price=FALSE)

file_name <- "./data/clean/data.csv"
write.csv(data, file=file_name, row.names = FALSE)

t2 <- Sys.time()
print(t2-t1)


