library(ranger)
library(tidyverse)
library(data.table)

source("./src/simulation/update_data.R")

load(file="./output/sim_dat_small.RData")
load(file="./output/model_final.RData")
data$CType <- as.character(data$CType)

dat <- update_data(data, res)

rm(data, res)
gc()

save(dat, file="./output/sim_updated_small.RData")