library(ranger)
library(tidyverse)
library(data.table)

load(file="./output/model_final.RData")



test <- read_climate_data("Temp",2)
test2 <- read_climate_data("Prec",2)
test3 <- read_climate_data("Rad", 2)

init_data <- data
rm(data)
gc()

update_data(init_data)
