source("./src/read_crop.R")
source("./src/read_soil.R")
source("./src/read_climate.R")
# source("./src/samples/read_price.R")

read_data <- function(n=4223142, exclude_grass=FALSE, exclude_crops=FALSE,include_price=FALSE) {
  # read ref data -------------------------------------------------------------------------------
  data_ref <- fread("./data/orig/ref.csv", sep = ",", header = TRUE) %>% 
    mutate_at(3:4, round) %>%
    arrange(OBJECTID) %>%
    sample_n(n, replace = FALSE) %>%
    as.data.table()
  
  # read crop data ------------------------------------------------------------------------------
  data_crop <- read_crop(data_ref, exclude_grass, exclude_crops)
  
  # read soil data ------------------------------------------------------------------------------
  data_soil <- read_soil()
  
  data <- inner_join(data_crop, data_soil,by = c("OBJECTID", "x_coord", "y_coord", "state")) %>% 
    as.data.table()
  
  rm(data_soil,data_crop)
  gc()
  
  # read temperature data -----------------------------------------------------------------------
  data_temp <- read_climate(data_ref, "Temp")
  
  data <- inner_join(data, data_temp,by = c("OBJECTID", "cell_id", "Year")) %>% as.data.table()
  
  rm(data_temp)
  gc()
  
  # read precipitation data -----------------------------------------------------------------------
  data_prec <- read_climate(data_ref, "Prec")
  
  data <- inner_join(data, data_prec,by = c("OBJECTID", "cell_id", "Year")) %>% as.data.table()
  
  rm(data_prec)
  gc()
  
  # read radiation data -------------------------------------------------------------------------
  data_rad <- read_climate(data_ref, "Rad")
  
  data <- inner_join(data, data_rad,by = c("OBJECTID", "cell_id", "Year")) %>% 
    select(-c("cell_id","FID")) %>% 
    as.data.table()
  
  rm(data_rad)
  gc()
  
  if(include_price) {
    # read price data ---------------------------------------------------------------------------------------
    data_price <- read_price()

    cl = parallel::makeCluster(6L)
    parallel::clusterExport(cl, list("data", "data_price"))
    parallel::clusterEvalQ(cl, { library(data.table) })

    data$pprice <- read_previous_crop_price(data,data_price)
    data$lyprice <- read_previous_year_price(data,data_price)
    data$deltaPrice <- read_price_diff(data,data_price)

    parallel::stopCluster
  }

  # finish ------------------------------------------------------------------------------------------------
  data <- data %>% 
    rename(
      "State" = state,
      "X" = x_coord, 
      "Y" = y_coord
    ) %>% 
    as.data.table()
  
  return(data)
}
