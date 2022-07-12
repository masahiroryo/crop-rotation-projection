dplyr_read_climate <- function(data_ref, type) {
  file_name <- paste("./data/orig/", tolower(type),".csv", sep="")
  data_temp_raw <- read_csv(file_name, show_col_types = FALSE)
  temp <- data.frame("cell_id"=data_temp_raw$cell_id)
  time_scale = c("05","06","07","08","09","10","11","12","13","14","15","16","17","18","19","20")
  
  j <- 4 # no idea
  for (i in time_scale){
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
    select(-c("state","x_coord", "y_coord"))
  
  rm(temp,i,j)
  gc() 
  
  data_temp <- data_temp_ %>% 
    select(cell_id, OBJECTID, ends_with('AVG')) %>% 
    pivot_longer(!c("cell_id", "OBJECTID"), names_to = "Year", values_to = paste(type,"AVG", sep="")) %>% 
    mutate(Year = gsub("^(.{2})(.*)$", "20\\1", Year))
  data_temp <- data_temp_ %>% 
    select(cell_id, OBJECTID, ends_with('JJ')) %>% 
    pivot_longer(!c("cell_id", "OBJECTID"), names_to = "Year", values_to = paste(type,"JJ", sep="")) %>% 
    mutate(Year = gsub("^(.{2})(.*)$", "20\\1", Year)) %>% 
    inner_join(data_temp, by=c("cell_id", "OBJECTID", "Year"))
  data_temp <- data_temp_ %>% 
    select(cell_id, OBJECTID, ends_with('AM')) %>% 
    pivot_longer(!c("cell_id", "OBJECTID"), names_to = "Year", values_to = paste(type,"AM", sep="")) %>% 
    mutate(Year = gsub("^(.{2})(.*)$", "20\\1", Year)) %>% 
    inner_join(data_temp, by=c("cell_id", "OBJECTID", "Year"))
  
  return(data_temp)
}