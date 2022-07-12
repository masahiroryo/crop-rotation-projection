dtable_read_soil <- function() {
  data_soil_raw <- fread("./data/orig/soil.csv", sep = ",", header = TRUE) %>% 
    mutate_at(3:4, round) %>% # round coordinates for better calculation
    arrange(OBJECTID) %>%
    rename("SType" = buek1000, 
           "SElev" = dem1000, 
           "SSlope" = slope1000) %>% 
    as.data.table()
  return(data_soil_raw)
}
