source("./src/bench/dtable/dtable_read_crop.R")
source("./src/bench/dtable/dtable_read_soil.R")
source("./src/bench/dtable/dtable_read_climate.R")

dtable_read_data <- function(n=4223142, exclude_grass=FALSE, exclude_crops=FALSE,include_price=FALSE) {
  # read ref data -------------------------------------------------------------------------------
  data_ref <- fread("./data/orig/ref.csv", sep = ",", header = TRUE) %>% 
    mutate_at(3:4, round) %>%
    arrange(OBJECTID) %>%
    sample_n(n, replace = FALSE) %>% 
    as.data.table()
  
  # read crop data ------------------------------------------------------------------------------
  data_crop <- dtable_read_crop(data_ref, exclude_grass, exclude_crops)
  
  # read soil data ------------------------------------------------------------------------------
  data_soil <- dtable_read_soil()
  
  data <- inner_join(data_crop, data_soil,by = c("OBJECTID", "x_coord", "y_coord", "state")) %>% 
    as.data.table()
  
  rm(data_soil,data_crop)
  gc()
  
  # read temperature data -----------------------------------------------------------------------
  data_temp <- dtable_read_climate(data_ref, "Temp")
  
  data <- inner_join(data, data_temp,by = c("OBJECTID", "cell_id", "Year")) %>% 
    as.data.table()
  
  rm(data_temp)
  gc()
  
  # read precipitation data -----------------------------------------------------------------------
  data_prec <- dtable_read_climate(data_ref, "Prec")
  
  data <- inner_join(data, data_prec,by = c("OBJECTID", "cell_id", "Year")) %>% 
    as.data.table()
  
  rm(data_prec)
  gc()
  
  # read radiation data -------------------------------------------------------------------------
  data_rad <- dtable_read_climate(data_ref, "Rad")
  
  data <- inner_join(data, data_rad,by = c("OBJECTID", "cell_id", "Year")) %>% 
    select(-c("cell_id","FID")) %>% 
    as.data.table()
  
  rm(data_rad)
  gc()
  
  # finish ------------------------------------------------------------------------------------------------
  data <- data %>% 
    rename(
      "State" = state,
      "X" = x_coord, 
      "Y" = y_coord
    )
  
  data$State <- as.factor(data$State)
  data$X <- as.numeric(data$X)
  data$Y <- as.numeric(data$Y)
  data$Year <- as.integer(data$Year)
  data$CType <- as.character(data$CType)
  data$CType <- as.factor(data$CType)
  data$PCType <- as.character(data$PCType)
  data$PCType <- as.factor(data$PCType)
  data$PPCType <- as.character(data$PPCType)
  data$PPCType <- as.factor(data$PPCType)
  data$SType <- as.factor(data$SType)
  data$SElev <- as.numeric(data$SElev)
  
  return(data)
}
