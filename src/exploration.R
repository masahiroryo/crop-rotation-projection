# load packages -------------------------------------------------------------------------------

library(data.table)
library(dtplyr)
library(tidyverse)

# read data -----------------------------------------------------------------------------------

data <- fread("./data/clean/sample_bavaria.csv", sep = ",", header = TRUE)
gc()

# analysis ------------------------------------------------------------------------------------

smaller <- data %>% 
  sample_n(50000) %>% 
  as.data.table()

ggplot(data = smaller) + 
  geom_point(mapping = aes(x = x_coord, y = y_coord), position = "jitter", stroke = 0.1)

ggplot(smaller) + 
  geom_bar(mapping = aes(x = crtype))

crop <- data %>% 
  select(crType)

# crop sequence -----------------------------------------------------------

n = 16*100

sliced_data <- data %>% 
  select(OBJECTID, Year, crtype) %>% 
  arrange(OBJECTID) %>% 
  slice(1:n) %>% 
  as.data.table()
gc()

crop_sequences <- sliced_data %>% 
  pivot_wider(names_from = Year, values_from=crtype, values_fill = 0, values_fn = list(. = mean)) %>% 
  as.data.table()
rm(sliced_data,data)
gc()

# most common n=2 crop sequence

list_consecutives <- function(y){
  res <- lapply(2, function(a) sapply(1:a, function(x) y[(0 + x):(length(y) - a + x)])) %>% 
    lapply(as.data.frame) %>% 
    setNames(sapply(2:(length(.) + 1), function(a) paste0("Consecutive", a)))
  return(res)
}

find_most_freq <- function(z) {
  lapply(1:length(z), function(x) (as.data.frame(table(do.call(
    paste, z[[x]])), stringsAsFactors = F) %>% 
      dplyr::mutate(length = nchar(Var1)) %>% 
      dplyr::filter(#length == max(length) & 
        Freq == max(Freq) & Freq > 1)) ) %>% 
    .[which(sapply(., nrow) > 0)] %>% 
    dplyr::bind_rows() %>% 
    dplyr::filter(Freq == max(Freq)) %>% 
    dplyr::filter(length == max(length)) %>% 
    dplyr::rename(Sequence = Var1) %>% 
    dplyr::select(-length)
}

get_most_frequent_sequence <- function(vec){
  l <- list_consecutives(vec)
  res <- find_most_freq(l)
  
  return(res)
}

test = get_most_frequent_sequence(as.character(crop_sequences[1,-1]))
for(i in c(2:n/16)){
  t <- as.character(crop_sequences[i,-1])
  test <- test %>% add_row(get_most_frequent_sequence(as.character(t)))
}

