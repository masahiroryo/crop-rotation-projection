dplyr_read_soil <- function() {
  data_soil_raw <- read_csv("./data/orig/soil.csv", show_col_types = FALSE) %>% 
    mutate_at(3:4, round) %>% # round coordinates for better calculation
    arrange(OBJECTID) %>%
    rename("SType" = buek1000, 
           "SElev" = dem1000, 
           "SSlope" = slope1000)
  return(data_soil_raw)
}
