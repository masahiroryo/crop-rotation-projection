library(data.table)
library(dtplyr)
library(tidyverse)

read_climate_data <- function(type,number_of_years, ref) {
  file_name <- paste("./data/simulated/",tolower(type),".csv",sep="")
  data_raw <- fread(file_name, sep=",", header = T)
  
  temp <- data.frame("cell_id"=data_raw$cell_id)
  
  i <- 20
  j <- 4
  for (i in 21:99){ # take average and summer data
    temp[paste(i,"cAVG",sep="")] <- data.frame(rowMeans(data_raw[,j:(j+5)]/10)) #March to August
    temp[paste(i,"cAM",sep="")] <- data.frame(rowMeans(data_raw[,(j+1):(j+2)]/10)) #April to May
    temp[paste(i,"cJJ",sep="")] <- data.frame(rowMeans(data_raw[,(j+3):(j+4)]/10)) #June to July
    j <- j+12
  }
  
  rm(data_raw)
  gc()
  
  temp <- temp %>% 
    select(c(1:(number_of_years*3+1))) ###############
  
  temp <- inner_join(temp, ref, by = c("cell_id")) %>% 
    filter(OBJECTID >= 0) %>% 
    arrange(OBJECTID) %>% 
    select(-c("state","x_coord", "y_coord")) %>%
    as.data.table()
  
  rm(i,j)
  gc() 
  
  res <- temp %>% 
    select(cell_id, OBJECTID, ends_with('AVG')) %>% 
    pivot_longer(!c("cell_id", "OBJECTID"), names_to = "Year", values_to = paste(type,"AVG",sep="")) %>% 
    mutate(Year = gsub("^(.{2})(.*)$", "20\\1", Year)) %>% 
    as.data.table()
  res <- temp %>% 
    select(cell_id, OBJECTID, ends_with('JJ')) %>% 
    pivot_longer(!c("cell_id", "OBJECTID"), names_to = "Year", values_to = paste(type,"JJ",sep="")) %>% 
    mutate(Year = gsub("^(.{2})(.*)$", "20\\1", Year)) %>% 
    inner_join(res, by=c("cell_id", "OBJECTID", "Year")) %>% 
    as.data.table() 
  res <- temp %>% 
    select(cell_id, OBJECTID, ends_with('AM')) %>% 
    pivot_longer(!c("cell_id", "OBJECTID"), names_to = "Year", values_to = paste(type,"AM",sep="")) %>% 
    mutate(Year = gsub("^(.{2})(.*)$", "20\\1", Year)) %>% 
    inner_join(res, by=c("cell_id", "OBJECTID", "Year")) %>% 
    as.data.table() 
  
  rm(temp)
  gc()
  
  return(res)
}

data_ref <- fread("./data/orig/ref.csv", sep = ",", header = TRUE) %>% 
  mutate_at(3:4, round) %>% 
  as.data.table()

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

climate <- c("Temp", "Prec", "Rad")

for(c in climate) {
  dat <- read_climate_data(c, 2, data_ref)
  data <- inner_join(data, dat,by = c("OBJECTID", "cell_id")) %>% as.data.table()
  rm(dat)
  gc()
}