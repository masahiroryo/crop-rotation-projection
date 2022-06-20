
library(data.table)
library(dtplyr)
library(tidyverse)
library(reshape2)

###########
no_years <- 5
###########

data_ref <- fread("./data/orig/ref.csv", sep = ",", header = TRUE) %>% 
  mutate_at(3:4, round) %>% 
  as.data.table()

print("starting soil data")
data_soil_raw <- fread("./data/orig/soil.csv", sep=",", header = TRUE) %>% 
  mutate_at(3:4, round) %>% # round coordinates for better calculation
  arrange(OBJECTID) %>%
  as.data.table()

data <- inner_join(data_ref, data_soil_raw,by = c("OBJECTID", "x_coord", "y_coord", "state")) %>% 
  rename("SType" = buek1000, 
         "SElev" = dem1000, 
         "SSlope" = slope1000) %>% 
  as.data.table()

rm(data_soil_raw)
gc()
print("finished soil data")
# read temperature data -----------------------------------------------------------------------

print("starting temperature data")
data_temp_raw <- fread("./data/simulated/temp.csv", sep=",", header = T)
temp <- data.frame("cell_id"=data_temp_raw$cell_id)

i <- 20
j <- 4
for (i in 21:99){ # take average and summer data
  temp[paste(i,"tAVG",sep="")] <- data.frame(rowMeans(data_temp_raw[,j:(j+5)]/10)) #March to August
  temp[paste(i,"tAM",sep="")] <- data.frame(rowMeans(data_temp_raw[,(j+1):(j+2)]/10)) #April to May
  temp[paste(i,"tJJ",sep="")] <- data.frame(rowMeans(data_temp_raw[,(j+3):(j+4)]/10)) #June to July
  j <- j+12
}

rm(data_temp_raw)
gc()

temp <- temp %>% 
  select(c(1:(no_years*3+1))) ###############

data_temp_ <- inner_join(temp, data_ref, by = c("cell_id")) %>% 
  filter(OBJECTID >= 0) %>% 
  arrange(OBJECTID) %>% 
  select(-c("state","x_coord", "y_coord")) %>%
  as.data.table()

rm(temp,i,j)
gc() 

data_test <- data_temp_ %>% 
  select(cell_id, OBJECTID, ends_with('AVG')) %>% 
  pivot_longer(!c("cell_id", "OBJECTID"), names_to = "Year", values_to = "TempAVG") %>% 
  mutate(Year = gsub("^(.{2})(.*)$", "20\\1", Year)) %>% 
  as.data.table()
data_test <- data_temp_ %>% 
  select(cell_id, OBJECTID, ends_with('JJ')) %>% 
  pivot_longer(!c("cell_id", "OBJECTID"), names_to = "Year", values_to = "TempJJ") %>% 
  mutate(Year = gsub("^(.{2})(.*)$", "20\\1", Year)) %>% 
  inner_join(data_test, by=c("cell_id", "OBJECTID", "Year")) %>% 
  as.data.table() 
data_test <- data_temp_ %>% 
  select(cell_id, OBJECTID, ends_with('AM')) %>% 
  pivot_longer(!c("cell_id", "OBJECTID"), names_to = "Year", values_to = "TempAM") %>% 
  mutate(Year = gsub("^(.{2})(.*)$", "20\\1", Year)) %>% 
  inner_join(data_test, by=c("cell_id", "OBJECTID", "Year")) %>% 
  as.data.table() 

rm(data_temp_)
gc()
data <- inner_join(data, data_test,by = c("OBJECTID", "cell_id")) %>% as.data.table()
rm(data_test)
gc()


# read precipitation data -----------------------------------------------------------------------

print("starting precipitation data")
data_prec_raw <- fread("./data/simulated/prec.csv", sep=",", header = T)
prec <- data.frame("cell_id"=data_prec_raw$cell_id)

i <- 20
j <- 4
for (i in 21:99){
  prec[paste(i,"pAVG",sep="")] <- data.frame(rowSums(data_prec_raw[,j:(j+5)]/10)) #March to August
  prec[paste(i,"pAM",sep="")] <- data.frame(rowSums(data_prec_raw[,(j+1):(j+2)]/10)) #April to May
  prec[paste(i,"pJJ",sep="")] <- data.frame(rowSums(data_prec_raw[,(j+3):(j+4)]/10)) #June to July
  j <- j+12
}

rm(data_prec_raw)
gc()

prec <- prec %>%
  select(c(1:(no_years*3+1))) ###############

data_prec_ <- inner_join(prec, data_ref, by = c("cell_id")) %>% 
  filter(OBJECTID >= 0) %>% 
  arrange(OBJECTID) %>% 
  select(-c("state","x_coord", "y_coord")) %>%
  as.data.table()

rm(prec,i,j)
gc()

data_test2 <- data_prec_ %>% 
  select(cell_id, OBJECTID, ends_with('AVG')) %>% 
  pivot_longer(!c("cell_id", "OBJECTID"), names_to = "Year", values_to = "PrecAVG") %>% 
  mutate(Year = gsub("^(.{2})(.*)$", "20\\1", Year)) %>% 
  as.data.table()
data_test2 <- data_prec_ %>% 
  select(cell_id, OBJECTID, ends_with('JJ')) %>% 
  pivot_longer(!c("cell_id", "OBJECTID"), names_to = "Year", values_to = "PrecJJ") %>% 
  mutate(Year = gsub("^(.{2})(.*)$", "20\\1", Year)) %>% 
  inner_join(data_test2, by=c("cell_id", "OBJECTID", "Year")) %>% 
  as.data.table() 
data_test2 <- data_prec_ %>% 
  select(cell_id, OBJECTID, ends_with('AM')) %>% 
  pivot_longer(!c("cell_id", "OBJECTID"), names_to = "Year", values_to = "PrecAM") %>% 
  mutate(Year = gsub("^(.{2})(.*)$", "20\\1", Year)) %>% 
  inner_join(data_test2, by=c("cell_id", "OBJECTID", "Year")) %>% 
  as.data.table() 

rm(data_prec_)
gc()
data <- inner_join(data, data_test2, by = c("OBJECTID", "cell_id", "Year")) %>% as.data.table()
rm(data_test2)
gc()

print("finished precipitation data")

# read radiation data -------------------------------------------------------------------------

print("starting radiation data")
data_rad_raw <- fread("./data/simulated/rad.csv", sep=",", header = T)
rad <- data.frame("cell_id"=data_rad_raw$cell_id)

i <- 20
j <- 4
for (i in 21:99){
  rad[paste(i,"rAVG",sep="")] <- data.frame(rowMeans(data_rad_raw[,j:(j+5)]/10)) #March to August
  rad[paste(i,"rAM",sep="")] <- data.frame(rowMeans(data_rad_raw[,(j+1):(j+2)]/10)) #April to May
  rad[paste(i,"rJJ",sep="")] <- data.frame(rowMeans(data_rad_raw[,(j+3):(j+4)]/10)) #June to July
  j <- j+12
}

rm(data_rad_raw)
gc()

rad <- rad %>%
  select(c(1:(no_years*3+1))) ###############

data_rad_ <- inner_join(rad, data_ref, by = c("cell_id")) %>% 
  filter(OBJECTID >= 0) %>% 
  arrange(OBJECTID) %>% 
  select(-c("state","x_coord", "y_coord")) %>%
  as.data.table()

rm(rad,i,j)
gc()

data_test3 <- data_rad_ %>% 
  select(cell_id, OBJECTID, ends_with('AVG')) %>% 
  pivot_longer(!c("cell_id", "OBJECTID"), names_to = "Year", values_to = "RadAVG") %>% 
  mutate(Year = gsub("^(.{2})(.*)$", "20\\1", Year)) %>% 
  as.data.table()
data_test3 <- data_rad_ %>% 
  select(cell_id, OBJECTID, ends_with('JJ')) %>% 
  pivot_longer(!c("cell_id", "OBJECTID"), names_to = "Year", values_to = "RadJJ") %>% 
  mutate(Year = gsub("^(.{2})(.*)$", "20\\1", Year)) %>% 
  inner_join(data_test3, by=c("cell_id", "OBJECTID", "Year")) %>% 
  as.data.table() 
data_test3 <- data_rad_ %>% 
  select(cell_id, OBJECTID, ends_with('AM')) %>% 
  pivot_longer(!c("cell_id", "OBJECTID"), names_to = "Year", values_to = "RadAM") %>% 
  mutate(Year = gsub("^(.{2})(.*)$", "20\\1", Year)) %>% 
  inner_join(data_test3, by=c("cell_id", "OBJECTID", "Year")) %>% 
  as.data.table() 

rm(data_rad_)
gc()
data <- inner_join(data, data_test3, by = c("OBJECTID", "cell_id", "Year")) %>% as.data.table()
rm(data_test3)
gc()

print("finished radiation data")

# data$CType = NA
# data$PCType = NA
# data$PPCType = NA

data$Year <- as.integer(data$Year)

# crop data ---------------------------------------------------------------------------------------------

file_name <- "./data/clean/data_no-info-crops_price.csv"

dat <- fread(file_name, sep=",", header=TRUE)

dat$State <- as.factor(dat$State)
dat$X <- as.numeric(dat$X)
dat$Y <- as.numeric(dat$Y)
dat$Year <- as.integer(dat$Year)
dat$CType <- as.factor(dat$CType)
dat$PCType <- as.factor(dat$PCType)
dat$PPCType <- as.factor(dat$PPCType)
dat$SType <- as.factor(dat$SType)
dat$SElev <- as.numeric(dat$SElev)

dat <- dat %>%
  drop_na() %>%
  as.data.table()

split_data <- function(dataset,testyear = 1) {
  years = sort(unique(dataset$Year))
  tail <- years[c((length(years)-testyear+1):length(years))]
  head <- years[1:(length(years)-testyear)]
  train <- dataset %>%
    filter(Year %in% head) %>%
    as.data.table()
  test <- dataset %>%
    filter(Year %in% tail) %>%
    as.data.table()
  return(list(train,test))
}

split <- split_data(dat)
train_data <- split[[1]]
test_data <- split[[2]]

rm(split, dat, train_data)
gc()

# combine -----------------------------------------------------------------------------------------------

dat <- test_data %>% 
  select(OBJECTID, CType, PCType, Year) %>% 
  as.data.table()

dat$Year <- dat$Year+1

lol <- data
data <- data %>% 
  filter(OBJECTID %in% dat$OBJECTID) %>% 
  group_by(Year) %>% 
  distinct(OBJECTID, .keep_all = TRUE) %>%
  as.data.table()

table(data$Year)


data <- left_join(data, dat, by = c("OBJECTID", "Year")) %>% 
  rename(PPCType = PCType,
         PCType = CType,
         State=state,
         X=x_coord,
         Y=y_coord) %>% 
  group_by(Year) %>% 
  distinct(OBJECTID, .keep_all = TRUE) %>%
  as.data.table()

data$CType <- NA

table(data$Year)

save(data, file="./output/sim_dat.RData")
 
data <- lol
