
dplyr_clear_crop_data <- function(dat, no_y=16) {
  y <- which(table(dat$OBJECTID)==no_y, arr.ind = TRUE)
  z <- rownames(y)
  
  dat <- dat %>%
    filter(OBJECTID %in% z)
  table(table(dat$OBJECTID)==no_y) # should be 0 false
  return(dat)
}

dplyr_add_prev_crop_type <- function(dat){
  pctype <- dat$CType
  pctype[seq(0, nrow(dat), by=16)] <- NA
  pctype <- c(NA, pctype[-length(pctype)])
  return(pctype)
}

dplyr_add_prev_prev_crop_type <- function(dat){
  ppctype <- dat$CType
  ppctype[sort(c(seq(0, nrow(dat), by=16), seq(-1, nrow(dat)-1, by=16)))[-c(1:2)]] <- NA
  ppctype <- c(NA, NA, ppctype[-c(length(ppctype)-1, length(ppctype))])
  return(ppctype)
}

dplyr_read_crop <- function(data_ref, exclude_grass=FALSE, exclude_crops=FALSE) {
  data_crop_raw <- read_csv("./data/orig/crop.csv", show_col_types = FALSE) %>% 
    mutate_at(3:4, round) %>% 
    rename_with(~ gsub("crop_", "", .x,fixed = TRUE)) %>% 
    rename(state=State)
  
  data_crop <- inner_join(data_ref, data_crop_raw, by = c("x_coord", "y_coord", "state")) %>% 
    pivot_longer(-c("FID", "state", "OBJECTID", "cell_id", "x_coord", "y_coord"), names_to = "Year", values_to = "CType") %>% 
    arrange(OBJECTID)
  
  if (exclude_grass) {
    data_crop <- data_crop %>% 
      group_by(OBJECTID) %>% 
      filter(all(CType==18))
  }
  data_crop <- data_crop %>% 
    filter(!(CType %in% c(0,18,19,70,80)))
  
  data_crop <- dplyr_clear_crop_data(data_crop, 16)
  
  data_crop$PCType <- dplyr_add_prev_crop_type(data_crop)
  data_crop$PPCType <- dplyr_add_prev_prev_crop_type(data_crop)
  
  rm(data_crop_raw)
  gc()
  return(data_crop)
}
