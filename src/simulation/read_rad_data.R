library(data.table)
library(dtplyr)
library(tidyverse)

read_precipitation_data <- function(number_of_years) {
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
    select(c(1:(number_of_years*3+1))) ###############
  
  data_rad_ <- inner_join(rad, data_ref, by = c("cell_id")) %>% 
    filter(OBJECTID >= 0) %>% 
    arrange(OBJECTID) %>% 
    select(-c("state","x_coord", "y_coord")) %>%
    as.data.table()
  
  rm(rad,i,j)
  gc()
  
  res <- data_rad_ %>% 
    select(cell_id, OBJECTID, ends_with('AVG')) %>% 
    pivot_longer(!c("cell_id", "OBJECTID"), names_to = "Year", values_to = "RadAVG") %>% 
    mutate(Year = gsub("^(.{2})(.*)$", "20\\1", Year)) %>% 
    as.data.table()
  res <- data_rad_ %>% 
    select(cell_id, OBJECTID, ends_with('JJ')) %>% 
    pivot_longer(!c("cell_id", "OBJECTID"), names_to = "Year", values_to = "RadJJ") %>% 
    mutate(Year = gsub("^(.{2})(.*)$", "20\\1", Year)) %>% 
    inner_join(res, by=c("cell_id", "OBJECTID", "Year")) %>% 
    as.data.table() 
  res <- data_rad_ %>% 
    select(cell_id, OBJECTID, ends_with('AM')) %>% 
    pivot_longer(!c("cell_id", "OBJECTID"), names_to = "Year", values_to = "RadAM") %>% 
    mutate(Year = gsub("^(.{2})(.*)$", "20\\1", Year)) %>% 
    inner_join(res, by=c("cell_id", "OBJECTID", "Year")) %>% 
    as.data.table() 
  
  rm(data_rad_)
  gc()
  return(res)
}