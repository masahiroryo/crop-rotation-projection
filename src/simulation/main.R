library(ranger)
library(caret)
library(reshape2)
library(data.table)
library(tidyverse)
library(dtplyr)


print("simulate data")
source("./src/simulation/simulation.R")
print("done")

print("preprocessing...")
source("./src/simulation/preprocessing_simulation.R")
print("done")

source("./src/simulation/update_data.R")

print("loading data...")
load(file="./output/sim_dat_small.RData")
load(file="./output/model_final.RData")
print("done")

print("updating data...")
dat <- update_data(data, res)
print("done")

rm(data, res)
gc()

print("saving data...")
save(dat, file="./output/sim_updated_small.RData")
print("done")