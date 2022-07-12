source("./src/bench/dplyr/dplyr_read_crop.R")
source("./src/bench/dplyr/dplyr_read_soil.R")
source("./src/bench/dplyr/dplyr_read_climate.R")

dplyr_read_data <- function(n=4223142, exclude_grass=FALSE, exclude_crops=FALSE,include_price=FALSE) {
  # read ref data -------------------------------------------------------------------------------
  data_ref <- read_csv("./data/orig/ref.csv", show_col_types = FALSE) %>% 
    mutate_at(3:4, round) %>%
    arrange(OBJECTID) %>%
    sample_n(n, replace = FALSE)
  
  # read crop data ------------------------------------------------------------------------------
  data_crop <- dplyr_read_crop(data_ref, exclude_grass, exclude_crops)
  
  # read soil data ------------------------------------------------------------------------------
  data_soil <- dplyr_read_soil()
  
  data <- inner_join(data_crop, data_soil,by = c("OBJECTID", "x_coord", "y_coord", "state"))
  
  rm(data_soil,data_crop)
  gc()
  
  # read temperature data -----------------------------------------------------------------------
  data_temp <- dplyr_read_climate(data_ref, "Temp")
  
  data <- inner_join(data, data_temp,by = c("OBJECTID", "cell_id", "Year"))
  
  rm(data_temp)
  gc()
  
  # read precipitation data -----------------------------------------------------------------------
  data_prec <- dplyr_read_climate(data_ref, "Prec")
  
  data <- inner_join(data, data_prec,by = c("OBJECTID", "cell_id", "Year"))
  
  rm(data_prec)
  gc()
  
  # read radiation data -------------------------------------------------------------------------
  data_rad <- dplyr_read_climate(data_ref, "Rad")
  
  data <- inner_join(data, data_rad,by = c("OBJECTID", "cell_id", "Year")) %>% 
    select(-c("cell_id","FID"))
  
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
