# load packages -------------------------------------------------------------------------------

library(data.table)
library(dtplyr)
library(dplyr, warn.conflicts = FALSE)

# read data -----------------------------------------------------------------------------------

n = 7500000
set.seed(seed = 187)

data_crop <- fread("./data/reshaped_crop.csv", sep =",", header = TRUE)
data_crop <- data_crop[order(OBJECTID)]
data_crop <- data_crop %>% 
  sample_n(n) %>% 
  as.data.table()

data_temp <- fread("./data/reshaped_temp.csv", sep=",", header=TRUE)
data_temp <- data_temp[order(OBJECTID)]
data <- as.data.table(inner_join(data_crop, data_temp, by=c("OBJECTID", "cell_id", "Year")))
rm(data_crop,data_temp)
gc()

data_prec <- fread("./data/reshaped_prec.csv", sep =",", header = TRUE)
data_prec <- data_prec[order(OBJECTID)]
data <- as.data.table(inner_join(data,data_prec, by=c("OBJECTID", "cell_id", "Year")))
rm(data_prec)
gc()

data_rad <- fread("./data/reshaped_rad.csv", sep=",", header=TRUE)
data_rad <- data_rad[order(OBJECTID)]
data <- as.data.table(inner_join(data,data_rad, by=c("OBJECTID", "cell_id", "Year")))
rm(data_rad)
gc()

#data_stype <- fread("./data/reshaped_soil.csv", sep =",", header = TRUE)
#data_stype <- data_stype[order(OBJECTID)]
#data <- as.data.table(inner_join(data,data_stype, by=c("OBJECTID", "cell_id", "x_coord", "y_coord")))
#rm(data_stype)
#gc()

#write.csv(data, "./data/data_clean.csv", row.names = FALSE)
write.csv(data, paste("./data/data_sample_", n,".csv"), row.names = FALSE)
