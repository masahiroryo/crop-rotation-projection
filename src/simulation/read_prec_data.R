library(data.table)
library(dtplyr)
library(tidyverse)

read_precipitation_data <- function(number_of_years) {
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
    select(c(1:(number_of_years*3+1))) ###############
  
  data_prec_ <- inner_join(prec, data_ref, by = c("cell_id")) %>% 
    filter(OBJECTID >= 0) %>% 
    arrange(OBJECTID) %>% 
    select(-c("state","x_coord", "y_coord")) %>%
    as.data.table()
  
  rm(prec,i,j)
  gc()
  
  reshape() <- data_prec_ %>% 
    select(cell_id, OBJECTID, ends_with('AVG')) %>% 
    pivot_longer(!c("cell_id", "OBJECTID"), names_to = "Year", values_to = "PrecAVG") %>% 
    mutate(Year = gsub("^(.{2})(.*)$", "20\\1", Year)) %>% 
    as.data.table()
  res <- data_prec_ %>% 
    select(cell_id, OBJECTID, ends_with('JJ')) %>% 
    pivot_longer(!c("cell_id", "OBJECTID"), names_to = "Year", values_to = "PrecJJ") %>% 
    mutate(Year = gsub("^(.{2})(.*)$", "20\\1", Year)) %>% 
    inner_join(res, by=c("cell_id", "OBJECTID", "Year")) %>% 
    as.data.table() 
  res <- data_prec_ %>% 
    select(cell_id, OBJECTID, ends_with('AM')) %>% 
    pivot_longer(!c("cell_id", "OBJECTID"), names_to = "Year", values_to = "PrecAM") %>% 
    mutate(Year = gsub("^(.{2})(.*)$", "20\\1", Year)) %>% 
    inner_join(res, by=c("cell_id", "OBJECTID", "Year")) %>% 
    as.data.table() 
  
  rm(data_prec_)
  gc()
  return(res)
}