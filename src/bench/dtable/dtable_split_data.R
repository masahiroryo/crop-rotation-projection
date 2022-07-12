dtable_split_data <- function(dataset,testyear = 1) {
  dataset <- dataset %>%
    drop_na() %>%
    select(-c(OBJECTID)) %>% 
    as.data.table()
  years = sort(unique(dataset$Year))
  tail <- years[c((length(years)-testyear+1):length(years))]
  head <- years[1:(length(years)-testyear)]
  train <- dataset %>%
    filter(Year %in% head) %>% 
    as.data.table()
  test <- dataset %>%
    filter(Year %in% tail) %>% 
    as.data.table()
  return(list(train,test))
}