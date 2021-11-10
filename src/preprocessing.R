# load packages -------------------------------------------------------------------------------

library(data.table)
library(dtplyr)
library(tidyverse)
library(reshape2)

# set number of samples -----------------------------------------------------------------------

n = 4223142 
# n = 2111571 # half of data (4223142)
#n = 1407714 # a third of data (4223142)
#n = 1055786 # a forth of data (4223142)
set.seed(seed = 187)
print(paste("starting for", n ,"samples"))
start_time <- Sys.time()

# read ref data -------------------------------------------------------------------------------
data_ref <- fread("./data/orig/ref.csv", sep = ",", header = TRUE) %>% 
  mutate_at(3:4, round) %>% 
  arrange(OBJECTID) %>% 
  #sample_n(n) %>%
  filter(state == "BV") %>% 
  as.data.table()

# read crop data ------------------------------------------------------------------------------

print("starting crop data")

data_crop_raw <- fread("./data/orig/crop.csv", sep=",", header = TRUE) %>% 
  mutate_at(3:4, round) %>% 
  rename_with(~ gsub("crop_", "", .x,fixed = TRUE)) %>% 
  as.data.table()

data_crop <- inner_join(data_ref, data_crop_raw, by = c("x_coord", "y_coord")) %>% 
  select(-c("State")) %>% 
  arrange(OBJECTID) %>% 
  as.data.table() %>% 
  gather(variable, value, -c("FID", "state", "OBJECTID", "cell_id", "x_coord", "y_coord")) %>% 
  rename("Year" = variable, "crtype" = value)

rm(data_crop_raw)
gc()

print("finished crop data")

# read soil data ------------------------------------------------------------------------------
print("starting soil data")
data_soil_raw <- fread("./data/orig/soil.csv", sep=",", header = TRUE) %>% 
  mutate_at(3:4, round) %>% 
  arrange(OBJECTID) %>%
  as.data.table()

data <- inner_join(data_crop, data_soil_raw,by = c("OBJECTID", "x_coord", "y_coord", "state"))

rm(data_soil_raw,data_crop)
gc()
print("finished soil data")
# read temperature data -----------------------------------------------------------------------
print("starting temperature data")
data_temp_raw <- fread("./data/orig/temp.csv", sep=",", header = T)
temp <- data.frame("cell_id"=data_temp_raw$cell_id)

i <- 5
j <- 4

for (i in 5:20){
  temp[paste(i,"tAVG",sep="")] <- data.frame(rowMeans(data_temp_raw[,j:(j+5)]/10)) #March to August
  temp[paste(i,"tAM",sep="")] <- data.frame(rowMeans(data_temp_raw[,(j+1):(j+2)]/10)) #April to May
  temp[paste(i,"tJJ",sep="")] <- data.frame(rowMeans(data_temp_raw[,(j+3):(j+4)]/10)) #June to July
  j <- j+12
}

rm(data_temp_raw)
gc()

data_temp_ <- inner_join(temp, data_ref, by = c("cell_id")) %>% 
  filter(OBJECTID >= 0) %>% 
  arrange(OBJECTID) %>% 
  select(1:50) %>% 
  as.data.table()

rm(temp,i,j)
gc()

data_temp <- reshape2::melt(data_temp_[,c(1,2,5,8,11,14,17,20,23,26,29,32,35,38,41,44,47,50)], 
                            id = c("cell_id", "OBJECTID"))
data_temp[,5:8] <- reshape2::melt(data_temp_[,c(1,3,6,9,12,15,18,21,24,27,30,33,36,39,42,45,48,50)], 
                                  id = c("cell_id", "OBJECTID"))
data_temp[,9:12] <- reshape2::melt(data_temp_[,c(1,4,7,10,13,16,19,22,25,28,31,34,37,40,43,46,49,50)], 
                                   id = c("cell_id", "OBJECTID"))

data_temp <- data_temp[,-c(5:7,9:11)]
names(data_temp) <- c("cell_id", "OBJECTID", "Year", "TempAVG", "TempAM", "TempJJ")

data_temp$Year <- recode(data_temp$Year, 
                         "5tAVG" = "2005",
                         "6tAVG" = "2006",
                         "7tAVG" = "2007",
                         "8tAVG" = "2008",
                         "9tAVG" = "2009",
                         "10tAVG" = "2010",
                         "11tAVG" = "2011",
                         "12tAVG" = "2012",
                         "13tAVG" = "2013",
                         "14tAVG" = "2014",
                         "15tAVG" = "2015",
                         "16tAVG" = "2016",
                         "17tAVG" = "2017",
                         "18tAVG" = "2018",
                         "19tAVG" = "2019",
                         "20tAVG" = "2020")

rm(data_temp_)
gc()
data <- inner_join(data, data_temp,by = c("OBJECTID", "cell_id", "Year"))
rm(data_temp)
gc()
print("finished temperature data")
# read precipitation data -----------------------------------------------------------------------
print("starting precipitation data")
data_prec_raw <- fread("./data/orig/prec.csv", sep=",", header = T)
temp <- data.frame("cell_id"=data_prec_raw$cell_id)

i <- 5
j <- 4

for (i in 5:20){
  temp[paste(i,"pAVG",sep="")] <- data.frame(rowSums(data_prec_raw[,j:(j+5)]/10)) #March to August
  temp[paste(i,"pAM",sep="")] <- data.frame(rowSums(data_prec_raw[,(j+1):(j+2)]/10)) #April to May
  temp[paste(i,"pJJ",sep="")] <- data.frame(rowSums(data_prec_raw[,(j+3):(j+4)]/10)) #June to July
  j <- j+12
}

rm(data_prec_raw)
gc()

data_prec_ <- inner_join(temp, data_ref, by = c("cell_id")) %>% 
  filter(OBJECTID >= 0) %>% 
  arrange(OBJECTID) %>% 
  select(1:50) %>% 
  as.data.table()

rm(temp,i,j)
gc()

data_prec <- reshape2::melt(data_prec_[,c(1,2,5,8,11,14,17,20,23,26,29,32,35,38,41,44,47,50)], 
                            id = c("cell_id", "OBJECTID"))
data_prec[,5:8] <- reshape2::melt(data_prec_[,c(1,3,6,9,12,15,18,21,24,27,30,33,36,39,42,45,48,50)], 
                                  id = c("cell_id", "OBJECTID"))
data_prec[,9:12] <- reshape2::melt(data_prec_[,c(1,4,7,10,13,16,19,22,25,28,31,34,37,40,43,46,49,50)], 
                                   id = c("cell_id", "OBJECTID"))

data_prec <- data_prec[,-c(5:7,9:11)]
names(data_prec) <- c("cell_id", "OBJECTID", "Year", "PrecAVG", "PrecAM", "PrecJJ")

data_prec$Year <- recode(data_prec$Year, 
                         "5pAVG" = "2005",
                         "6pAVG" = "2006",
                         "7pAVG" = "2007",
                         "8pAVG" = "2008",
                         "9pAVG" = "2009",
                         "10pAVG" = "2010",
                         "11pAVG" = "2011",
                         "12pAVG" = "2012",
                         "13pAVG" = "2013",
                         "14pAVG" = "2014",
                         "15pAVG" = "2015",
                         "16pAVG" = "2016",
                         "17pAVG" = "2017",
                         "18pAVG" = "2018",
                         "19pAVG" = "2019",
                         "20pAVG" = "2020")

rm(data_prec_)
gc()
data <- inner_join(data, data_prec,by = c("OBJECTID", "cell_id", "Year"))
rm(data_prec)
gc()
print("finished precipitation data")
# read radiation data -------------------------------------------------------------------------
print("starting radiation data")
data_rad_raw <- fread("./data/orig/rad.csv", sep=",", header = T)
temp <- data.frame("cell_id"=data_rad_raw$cell_id)

i <- 5
j <- 4

for (i in 5:20){
  temp[paste(i,"rAVG",sep="")] <- data.frame(rowMeans(data_rad_raw[,j:(j+5)]/10)) #March to August
  temp[paste(i,"rAM",sep="")] <- data.frame(rowMeans(data_rad_raw[,(j+1):(j+2)]/10)) #April to May
  temp[paste(i,"rJJ",sep="")] <- data.frame(rowMeans(data_rad_raw[,(j+3):(j+4)]/10)) #June to July
  j <- j+12
}

rm(data_rad_raw)
gc()

data_rad_ <- inner_join(temp, data_ref, by = c("cell_id")) %>% 
  filter(OBJECTID >= 0) %>% 
  arrange(OBJECTID) %>% 
  select(1:50) %>% 
  as.data.table()

rm(temp,i,j)
gc()

data_rad <- reshape2::melt(data_rad_[,c(1,2,5,8,11,14,17,20,23,26,29,32,35,38,41,44,47,50)], 
                            id = c("cell_id", "OBJECTID"))
data_rad[,5:8] <- reshape2::melt(data_rad_[,c(1,3,6,9,12,15,18,21,24,27,30,33,36,39,42,45,48,50)], 
                                  id = c("cell_id", "OBJECTID"))
data_rad[,9:12] <- reshape2::melt(data_rad_[,c(1,4,7,10,13,16,19,22,25,28,31,34,37,40,43,46,49,50)], 
                                   id = c("cell_id", "OBJECTID"))

data_rad <- data_rad[,-c(5:7,9:11)]
names(data_rad) <- c("cell_id", "OBJECTID", "Year", "RadAVG", "RadAM", "RadJJ")

data_rad$Year <- recode(data_rad$Year, 
                         "5rAVG" = "2005",
                         "6rAVG" = "2006",
                         "7rAVG" = "2007",
                         "8rAVG" = "2008",
                         "9rAVG" = "2009",
                         "10rAVG" = "2010",
                         "11rAVG" = "2011",
                         "12rAVG" = "2012",
                         "13rAVG" = "2013",
                         "14rAVG" = "2014",
                         "15rAVG" = "2015",
                         "16rAVG" = "2016",
                         "17rAVG" = "2017",
                         "18rAVG" = "2018",
                         "19rAVG" = "2019",
                         "20rAVG" = "2020")

rm(data_rad_)
gc()
data <- inner_join(data, data_rad,by = c("OBJECTID", "cell_id", "Year"))
rm(data_rad)
gc()
print("finished radiation data")
end_time <- Sys.time()
print(end_time-start_time)
print(paste("for", n, "samples"))

print("saving")
# write.csv(data, paste("./data/clean/sample",n,".csv", sep = ""), row.names = FALSE)
#write.csv(data, "./data/clean/sample_bavaria.csv", row.names = FALSE)
print("finished")
