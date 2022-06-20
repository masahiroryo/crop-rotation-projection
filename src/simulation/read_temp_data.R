library(data.table)
library(dtplyr)
library(tidyverse)

read_temperature_data <- function(number_of_years) {
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
    select(c(1:(number_of_years*3+1))) ###############
  
  data_temp_ <- inner_join(temp, data_ref, by = c("cell_id")) %>% 
    filter(OBJECTID >= 0) %>% 
    arrange(OBJECTID) %>% 
    select(-c("state","x_coord", "y_coord")) %>%
    as.data.table()
  
  rm(temp,i,j)
  gc() 
  
  res <- data_temp_ %>% 
    select(cell_id, OBJECTID, ends_with('AVG')) %>% 
    pivot_longer(!c("cell_id", "OBJECTID"), names_to = "Year", values_to = "TempAVG") %>% 
    mutate(Year = gsub("^(.{2})(.*)$", "20\\1", Year)) %>% 
    as.data.table()
  res <- data_temp_ %>% 
    select(cell_id, OBJECTID, ends_with('JJ')) %>% 
    pivot_longer(!c("cell_id", "OBJECTID"), names_to = "Year", values_to = "TempJJ") %>% 
    mutate(Year = gsub("^(.{2})(.*)$", "20\\1", Year)) %>% 
    inner_join(res, by=c("cell_id", "OBJECTID", "Year")) %>% 
    as.data.table() 
  res <- data_temp_ %>% 
    select(cell_id, OBJECTID, ends_with('AM')) %>% 
    pivot_longer(!c("cell_id", "OBJECTID"), names_to = "Year", values_to = "TempAM") %>% 
    mutate(Year = gsub("^(.{2})(.*)$", "20\\1", Year)) %>% 
    inner_join(res, by=c("cell_id", "OBJECTID", "Year")) %>% 
    as.data.table() 
  
  rm(data_temp_)
  gc()
  return(res)
}