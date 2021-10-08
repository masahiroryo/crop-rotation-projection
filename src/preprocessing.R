# load packages -------------------------------------------------------------------------------

library(data.table)
library(dtplyr)
library(tidyverse)

# read ref data -------------------------------------------------------------------------------
data_ref <- fread("./data/ref.csv", sep = ",", header = TRUE) %>% 
  mutate_at(3:4, round) %>% 
  #filter(OBJECTID > 0) %>%
  #distinct(OBJECTID, .keep_all = TRUE) %>%
  arrange(OBJECTID) %>% 
  as.data.table()

# read crop data ------------------------------------------------------------------------------

data_crop_raw <- fread("./data/crop.csv", sep=",", header = TRUE) %>% 
  mutate_at(3:4, round) %>% 
  rename_with(~ gsub("crop_", "", .x,fixed = TRUE)) %>% 
  as.data.table()

data_crop <- inner_join(data_ref, data_crop_raw, by = c("x_coord", "y_coord")) %>% 
  select(-c("state")) %>% 
  filter(OBJECTID >= 0) %>% 
  # distinct(OBJECTID, .keep_all = TRUE) %>% #
  arrange(OBJECTID) %>% 
  as.data.table() %>% 
  gather(variable, value, -c("FID", "State", "OBJECTID", "cell_id", "x_coord", "y_coord")) %>% 
  rename("Year" = variable, "crtype" = value)

rm(data_crop_raw)
gc()

# read soil data ------------------------------------------------------------------------------

data_soil_raw <- fread("./data/soil.csv", sep=",", header = TRUE) %>% 
  mutate_at(3:4, round) %>% 
  as.data.table()

crspd <- Reduce(function(...) merge(..., all = TRUE, by = c("x_coord", "y_coord")), list(data_crop,data_soil_raw))
crspd <- crspd[,-c(6:8,10)]

#idx1 <- which(crspd$OBJECTID.x > 0)
idx4 <- na.omit(unique(crspd$OBJECTID.x)) # Check for duplicates and erase them
#idx3 <- which(duplicated(crspd$OBJECTID.x[idx1])) # Check for duplicates and erase them

idx3 <- which (crspd$OBJECTID.x==crspd$OBJECTID.y)

if (length(idx4) == 0) {
  resh.stype <- crspd[idx3,]
} else {
  resh.stype <- crspd[idx3,]
  #resh.stype <- resh.stype[-c(idx4),]
}

resh.stype <- sort(resh.stype, by="OBJECTID.x")
resh.crop <- sort(resh.crop, by="OBJECTID")

test_stype <- which(resh.stype$OBJECTID.x==resh.crop$OBJECTID) # shou

# read temperature data -----------------------------------------------------------------------

data_temp <- fread("./data/temp.csv", sep=",", header = T)
temp <- data.frame("cell_id"=data_temp$cell_id)

i <- 5
j <- 4

for (i in 5:20){
  temp[paste(i,"tAVG",sep="")] <- data.frame(rowMeans(data_temp[,j:(j+5)]/10)) #March to August
  temp[paste(i,"tAM",sep="")] <- data.frame(rowMeans(data_temp[,(j+1):(j+2)]/10)) #April to May
  temp[paste(i,"tJJ",sep="")] <- data.frame(rowMeans(data_temp[,(j+3):(j+4)]/10)) #June to July
  j <- j+12
}

test <- data_temp %>% 
  rowwise() %>% 
  select(contains(c("1003","1004","1005","1006","1007","1008"))) %>% 
  mutate(m = list(mean)) %>% 
  as.data.table()
  
  
  df %>% rowwise() %>% mutate(m = mean(c(x, y, z)))

data_temp <- inner_join(temp, data_ref, by = c("cell_id")) %>% 
  filter(OBJECTID > 0) %>% 
  distinct(OBJECTID, .keep_all = TRUE) %>% 
  arrange(OBJECTID) %>% 
  select(1:50) %>% 
  as.data.table()

rm(temp,i,j)
gc()
resh_temp <- melt(data_temp[,c(1,2,5,8,11,14,17,20,23,26,29,32,35,38,41,44,47,50)], id = c("cell_id", "OBJECTID"))

