# load packages -------------------------------------------------------------------------------

library(data.table)
library(dtplyr)
library(tidyverse)

# read data -----------------------------------------------------------------------------------

data <- fread("./data/clean/sample_bavaria.csv", sep = ",", header = TRUE)
data$crtype <- dplyr::recode(data$crtype, 
                             `70` = 20L,
                             `80` = 21L)
gc()

# exploration ------------------------------------------------------------------------------------

smaller <- data %>% 
  sample_n(50000) %>% 
  as.data.table()

ggplot(data = smaller) + 
  geom_point(mapping = aes(x = x_coord, y = y_coord), position = "jitter", stroke = 0.1)

ggplot(smaller) + 
  geom_bar(mapping = aes(x = crtype))


plot_crops_per_year <- function(year){
  dat <- smaller %>% 
    filter(Year == year) %>% 
    as.data.table()
  
  ggplot(data = dat) + 
    geom_point(mapping = aes(x = x_coord, y = y_coord, color=factor(crtype)), position = "jitter", stroke = 0.1)
}

plot_crops_per_year(2018)

# crop sequence -----------------------------------------------------------

n = 16*1000

sliced_data <- data %>% 
  select(OBJECTID, Year, crtype) %>% 
  arrange(OBJECTID) %>% 
  slice(1:n) %>%
  as.data.table()
gc()

crop_sequences <- sliced_data %>% 
  pivot_wider(names_from = Year, values_from=crtype, values_fn = list(. = mean)) %>% 
  as.data.table()
rm(sliced_data,data)
gc()

# most common n=2 crop sequence
k = 2
find_k_consecutives <- function(y, k){
  res <- lapply(k, function(a) sapply(1:a, function(x) y[(0 + x):(length(y) - a + x)])) %>% 
    lapply(as.data.frame) %>% 
    setNames(sapply(2:(length(.) + 1), function(a) paste0("Consecutive", a)))
  return(res)
}

find_most_freq <- function(z) {
  lapply(1:length(z), function(x) (as.data.frame(table(do.call(
    paste, z[[x]])), stringsAsFactors = F) %>% 
      dplyr::mutate(length = nchar(Var1)) %>% 
      dplyr::filter(Freq == max(Freq) & Freq > 0)) ) %>% 
    .[which(sapply(., nrow) > 0)] %>% 
    dplyr::bind_rows() %>%
    dplyr::filter(Freq == max(Freq)) %>%
    dplyr::filter(length == max(length)) %>%
    dplyr::rename(Sequence = Var1) %>%
    dplyr::select(-length)
}

get_most_frequent_sequence <- function(vec,k=2){
  l <- find_k_consecutives(as.character(vec),k)
  res <- find_most_freq(l)
  
  return(res)
}

start_time <- Sys.time()
most_freq = get_most_frequent_sequence((crop_sequences[1,-1]),k)
for(i in c(2:nrow(crop_sequences))){
  temp <- (crop_sequences[i,-1])
  most_freq <- most_freq %>% add_row(get_most_frequent_sequence((temp),k))
}
end_time <- Sys.time()
print(end_time-start_time)
which(table(most_freq$Sequence) == max(table(most_freq$Sequence)))
