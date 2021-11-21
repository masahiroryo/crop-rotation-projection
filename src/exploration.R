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
  sample_n(100000) %>%
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

plot_crops_per_year(2005)

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
# rm(sliced_data,data)
gc()

# parallel approach
# library(foreach)
# library(doParallel)  
# no_cores <- detectCores() - 1  
# cl <- makeCluster(no_cores, type="FORK")  
# registerDoParallel(cl)  

# testing ground ----------------------------------------------------------
crop_sequences <- as.data.frame(crop_sequences[,-1])

mat <- matrix(0, nrow = 21, ncol = 21)
colnames(mat) <- c(1:21)

start_time <- Sys.time()
invisible(foreach (i=1:nrow(crop_sequences)) %do% 
  for (j in 2:ncol(crop_sequences)) {
    a = crop_sequences[i,j-1]
    b = crop_sequences[i,j]
    mat[a, b] = mat[a, b] + 1
  })
end_time <- Sys.time()
old <- end_time-start_time

a = 2
n_largest <- function(m,n) {
  res <- sort(mat, decreasing=TRUE)[1:n]
  return(res)
}
res <- lapply(1:a, function(x) which(mat==n_largest(mat,4)[x], arr.ind=TRUE))
for (l in 1:a) {
  print(res[[l]])
}

start_time <- Sys.time()
s = parLapply(cl, 1:nrow(crop_sequences), function(i) {
  xx <- matrix(0, nrow = 21, ncol = 21)
  colnames(xx) <- c(1:21)
  for (j in 2:ncol(crop_sequences)) {
    a = crop_sequences[i,j-1]
    b = crop_sequences[i,j]
    xx[a, b] = xx[a, b] + 1
  }
  return(xx)
}) 
res <- Reduce(`+`, s)
end_time <- Sys.time()
new <- end_time-start_time


print(old)
print(new)
all(res == mat)

res <- s[[1]]
for(i in 1:length(s)) {
  res <- res + s[[i]]
}
