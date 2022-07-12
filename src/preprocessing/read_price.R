read_price <- function() { 
  data_price_raw <- fread("./data/orig/price.csv", sep=",", header = T)
  
  data_p <- data_price_raw %>% 
    mutate(Item = recode(Item, "Vegetables, leguminous nes" = "Vegetables, leguminous")) %>% 
    filter(Unit == 'USD') %>%
    as.data.table()
  
  data_p <- data_p %>% 
    pivot_wider(id_cols = Item, values_from = Value,names_from = Year) %>% 
    as.data.table()
  
  k <- which(is.na(data_p), arr.ind=TRUE)
  data_p[k] <- rowMeans(data_p[,-1], na.rm=TRUE)[k[,1]]
  data_p$`2020` <- round(rowMeans(data_p[,-1]),2) # set data for 2020 as mean over rows
  
  scheme <- fread("./data/orig/classification.csv", sep="\t", header = TRUE)
  scheme$`Price Description (as of 24.09.2021, subject to changes!)` <- tolower(scheme$`Price Description (as of 24.09.2021, subject to changes!)`)
  data_p$Item <- tolower(data_p$Item)
  
  temp <- scheme %>% 
    select(`Crop Class ID`, `Price Description (as of 24.09.2021, subject to changes!)`) %>% 
    as.data.table()
  colnames(temp) <- c('ID', 'Item')
  temp[7,2] <- "triticale"
  
  data_price <- inner_join(temp, data_p, by = "Item") %>% 
    as.data.table()
}

read_previous_crop_price <- function(dat,data_price) {
  res <- sapply(1:nrow(dat), function(i) {
    id <- dat[i,]$PCType
    year <- dat[i,]$Year
    return(as.numeric(data_price[which(data_price$ID==id), ..year]))
  })  
  return(res)
}

read_previous_year_price <- function(dat,data_price) {
  res <- sapply(1:nrow(dat), function(i) {
    id <- dat[i,]$PCType
    y <- as.numeric(dat[i,]$Year)
    year <- ifelse(y>2005, as.character(y-1), NA)
    return(as.numeric(data_price[which(data_price$ID==id), ..year]))
  })  
  idx <- !(sapply(res, length))
  res[idx] <- NA
  res <- unlist(res)
  return(res)
}

read_price_diff <- function(dat,data_price) {
  res <- sapply(1:nrow(dat), function(i) {
    id <- dat[i,]$PCType
    y <- as.numeric(dat[i,]$Year)
    year <- ifelse(y>2005, as.character(y-1), NA)
    pyear <- ifelse(y>2006, as.character(y-2), NA)
    delta_price <- as.numeric(data_price[which(data_price$ID==id), ..pyear]) - as.numeric(data_price[which(data_price$ID==id), ..year])
    return(delta_price)
  })  
  idx <- !(sapply(res, length))
  res[idx] <- NA
  res <- unlist(res)
  return(res)
}

sapply(c(1:5), function(i) {
  print(i)
} )
