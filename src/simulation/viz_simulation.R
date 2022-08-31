library(tidyverse)
library(data.table)
library(dtplyr)


theme_set(theme_bw())
primary_color = '#4477AA'
secondary_color = '#228833'

load(file="./output/sim_updated_small.RData")

classification <- read.csv("./data/orig/classification.csv",sep="\t")[,1:2]
c <- vector("character", nrow(dat))
i<-1
for (type in dat$CType) {
  c[i] = classification$Crop.Class.Name[classification$Crop.Class.ID==type]
  i=i+1
}
dat$CType.Name <- c

# frequency of each crop type
dat$CType <- as.factor(as.numeric(dat$CType))
freq_plot <- ggplot(dat) + 
  geom_bar(mapping = aes(x = CType.Name), fill=primary_color)+
  labs(x="Crop Type", y="Frequency", title="Frequence of each crop type")+
  theme(plot.title = element_text(margin = margin(10, 0, 10, 0), size = 14),
        axis.text.x = element_text(angle=45, hjust = 1))
freq_plot
png(filename="./output/projection_frequency_all.png")
freq_plot
dev.off()

data <- dat %>% 
  filter(Year %in% c(2021:2030)) %>% 
  as.data.table()

freq_plot_start <- ggplot(data) + 
  geom_bar(mapping = aes(x = CType), fill=primary_color)+
  labs(x="Crop Type", y="Frequency", title="Frequence of each crop type")+
  theme(plot.title = element_text(margin = margin(10, 0, 10, 0),
                                  size = 14))

png(filename="./output/projection_frequency_beg.png")
plot(freq_plot_start)
dev.off()

data <- dat %>% 
  filter(Year %in% c(2061:2070)) %>% 
  as.data.table()

freq_plot_end <- ggplot(data) + 
  geom_bar(mapping = aes(x = CType), fill=primary_color)+
  labs(x="Crop Type", y="Frequency", title="Frequence of each crop type")+
  theme(plot.title = element_text(margin = margin(10, 0, 10, 0),
                                  size = 14))
png(filename="./output/projection_frequency_end.png")
plot(freq_plot_end)
dev.off()

# crop sequence -----------------------------------------------------------------------------------------
dat$CType <- as.numeric(dat$CType)
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

get_frequences <- function(sequences, n) {
  start_time <- Sys.time()
  s = lapply(1:nrow(sequences), function(i) {
    xx <- matrix(0, nrow = 20, ncol = 20)
    colnames(xx) <- 1:20
    for (j in 2:ncol(sequences)) {
      a = sequences[i,j-1]
      b = sequences[i,j]
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
  res <- lapply(1:a, function(x) which(freq_mat==n_largest(freq_mat,a)[x], arr.ind=TRUE))
  for (l in 1:a) {
    print(res[[l]])
  }
  return(res)
}

freq_matrix <- get_frequences(crop_sequences, length(unique(dat$CType)))
most_freq <- get_most_frequent_sequences(freq_matrix, 5)

# relative frequency --------------------------------------------------------------------------

crops_of_interest <- c("Maize (silage)", "Grassland", "Legumes", "Spring Wheat, triticale and rye",
                       "Winter barley", "Winter wheat", "Maize (grain)")

rel <- dat %>%  
  count(CType.Name, Year) %>%
  group_by(Year) %>% 
  mutate(freq = n / sum(n)) %>% 
  ungroup() %>% 
  select(-n) %>% 
  filter(CType.Name %in% crops_of_interest) %>% 
  #filter(CType %in% c(11:18)) %>% 
  as.data.frame()

#png(filename="./output/freq_projection.png")
ggplot(rel, aes(y=(freq*100),x=Year, color=CType.Name))+
  labs(y="Relative Frequency", x="Year", title=paste("Relative Frequency for each Year"))+
  geom_line(size=1)+
  geom_point()+
  scale_color_npg()
#dev.off()

