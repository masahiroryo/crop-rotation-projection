library(tidyverse)

theme_set(theme_bw())
primary_color = '#4477AA'
secondary_color = '#228833'


load(file="./output/sim_updated_small.RData")

# frequency of each crop type
dat$CType <- as.factor(as.numeric(dat$CType))
freq_plot <- ggplot(dat) + 
  geom_bar(mapping = aes(x = CType), fill=primary_color)+
  labs(x="Crop Type", y="Frequency", title="Frequence of each crop type")+
  theme(plot.title = element_text(margin = margin(10, 0, 10, 0),
                                  size = 14))
freq_plot

# crop sequence -----------------------------------------------------------------------------------------
data <- dat %>% 
  select(OBJECTID, Year, CType) %>% 
  arrange(OBJECTID) %>% 
  as.data.table()
gc()

crop_sequences <- data %>% 
  pivot_wider(names_from = Year, values_from=CType, values_fn = list(. = mean)) %>% 
  as.data.table()
gc()

crop_sequences <- as.data.frame(crop_sequences[,-1])

get_frequences <- function(sequences) {
  start_time <- Sys.time()
  s = lapply(1:nrow(crop_sequences), function(i) {
    xx <- matrix(0, nrow = 17, ncol = 17)
    colnames(xx) <- c(1:17)
    for (j in 2:ncol(crop_sequences)) {
      a = crop_sequences[i,j-1]
      b = crop_sequences[i,j]
      xx[a, b] = xx[a, b] + 1
    }
    return(xx)
  }) 
  res <- Reduce(`+`, s)
  end_time <- Sys.time()
  print(end_time-start_time)
  return(res)
}

n_largest <- function(m,n) {
  res <- sort(m, decreasing=TRUE)[1:n]
  return(res)
}
get_most_frequent_sequences <- function(freq_mat, a) {
  res <- lapply(1:a, function(x) which(freq_mat==n_largest(freq_mat,4)[x], arr.ind=TRUE))
  for (l in 1:a) {
    print(res[[l]])
  }
  return(res)
}

freq_matrix <- get_frequences(crop_sequences)
most_freq <- get_most_frequent_sequences(freq_matrix, 3)
