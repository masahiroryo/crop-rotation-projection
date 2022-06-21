library(ranger)
library(tidyverse)
library(data.table)

load(file="./output/model_final.RData")
load(file="./output/sim_dat.RData")
dat <- update_data(data, res)