# load packages -------------------------------------------------------------------------------

library(data.table)
library(dtplyr)
library(tidyverse)

# read data -----------------------------------------------------------------------------------

data <- fread("./data/clean/sample_clean.csv", sep=",", header=TRUE)

# data$CType <- dplyr::recode(data$CType, 
#                              `70` = 20L,
#                              `80` = 21L)
gc()

# data <- data %>%
#   group_by(OBJECTID) %>%
#   mutate(hoi = mean(CType)) %>%
#   filter(hoi != 18) %>%
#   select(-hoi) %>%
#   as.data.table()

# data_crop_raw <- fread("./data/orig/crop.csv", sep=",", header = TRUE) %>% 
#   mutate_at(3:4, round) %>% 
#   rename_with(~ gsub("crop_", "", .x,fixed = TRUE)) %>% 
#   as.data.table()

# summary statistics --------------------------------------------------------------------------

smaller <- data %>% 
  sample_n(100000) %>%
  as.data.table()
rm(data)
gc()

# frequency of each crop type
ggplot(data) + 
  geom_bar(mapping = aes(x = CType))

# frequency of each crop type for specific year
freq_crops_per_year <- function(year){
  dat <- data %>% 
    filter(Year == year) %>% 
    as.data.table()
  
  ggplot(dat) + 
    geom_bar(mapping = aes(x = CType))
}
freq_crops_per_year(2020)

# map with most frequent crop type for specific year
plot_crops_per_year <- function(year){
  dat <- data %>% 
    filter(Year == year) %>% 
    as.data.table()
  
  ggplot(data = dat) + 
    geom_point(mapping = aes(x = X, y = Y, color=factor(CType)), position = "jitter", stroke = 0.1)
}

plot_crops_per_year(2005)
plot_crops_per_year(2020)
# summary statistics for environmental data
summary(data$TempAVG)
summary(data$PrecAVG)
summary(data$RadAVG)

# summary about states
table(data_crop_raw$State)

#pdf(file = "./output/boxplot_states.jpg")
ggplot(data = data_crop_raw) + 
  geom_bar(mapping = aes(x = State))
#dev.off()

# Proportion of crop types -> Crops of minor importance 
prop_crtype_per_year <- function(y){
  dat <- smaller %>% 
    filter(Year == y) %>% 
    as.data.table()
  N <- sum(nrow(dat))
  
  res <- lapply(c(0:21), function(x){
    return((sum(dat$crtype==x))/N)
  })
  res <- do.call(rbind.data.frame, res)
  colnames(res) <- "prop"
  return(res)
}
# undervalued crop types
find_undervalued_crtypes <- function(props){
  return(which(props < quantile(props$prop, 0.25))-1)
}
find_overvalued_crtypes <- function(props){
  return(which(props > quantile(props$prop, 0.75))-1)
}
y = 2020
cr20 <- prop_crtype_per_year(y)

print(paste("the following crops are underrepresented for the year ", y))
print(find_undervalued_crtypes(cr20))
print(paste("the following crops are overrepresented for the year ", y))
print(find_overvalued_crtypes(cr20))

# crop sequence -----------------------------------------------------------

library(doParallel)
no_cores <- detectCores() - 1
cl <- makeCluster(no_cores, type="FORK")
# registerDoParallel(cl)

n = 16*100000

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

crop_sequences <- as.data.frame(crop_sequences[,-1])

get_frequences <- function(sequences) {
  start_time <- Sys.time()
  s = lapply(1:nrow(crop_sequences), function(i) {
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
  print(end_time-start_time)
  return(res)
}

par_get_frequences <- function(sequences) {
  start_time <- Sys.time()
  s <- parLapply(cl, 1:nrow(crop_sequences), function(i) {
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
most_freq <- get_most_frequent_sequences(freq_matrix, 4)

# grassland ---------------------------------------------------------------

ggplot(data) + 
  geom_bar(mapping = aes(x = CType))

round(table(data$CType) / nrow(data),2)

without_grassland <- data %>% 
  filter(CType != 18) %>% 
  as.data.table()

ggplot(without_grassland) + 
  geom_bar(mapping = aes(x = CType))

round(table(without_grassland$CType) / nrow(without_grassland),2)
sum(table(without_grassland$CType) / nrow(without_grassland))

test <- data %>% 
  sample_n(16) %>% 
  as.data.table()

test_IDs <- test$OBJECTID

test <- data %>% 
  filter(OBJECTID %in% test_IDs) %>% 
  as.data.table()

test <- test %>% 
  group_by(OBJECTID) %>% 
  mutate(hoi = mean(CType)) %>% 
  filter(hoi != 18) %>% 
  select(-hoi) %>% 
  as.data.table()

ggplot(test) + 
  geom_bar(mapping = aes(x = CType))

round(table(test$CType) / nrow(test),2)
sum(table(test$CType) / nrow(test))

test <- data %>% 
  group_by(OBJECTID) %>% 
  mutate(hoi = mean(CType)) %>% 
  filter(hoi != 18) %>% 
  select(-hoi) %>% 
  as.data.table()

ggplot(test) + 
  geom_bar(mapping = aes(x = CType))

round(table(test$CType) / nrow(test),2)
sum(table(test$CType) / nrow(test))

# price and ctype ---------------------------------------------------------
print("starting economy data")

data_price_raw <- fread("./data/orig/price.csv", sep=",", header = T)
data_p <- data_price_raw %>% 
  mutate(Item = recode(Item, "Vegetables, leguminous nes" = "Vegetables, leguminous")) %>% 
  filter(Unit == 'USD') %>%
  as.data.table()

data_p <- data_p %>% 
  pivot_wider(id_cols = Item, values_from = Value,names_from = Year) %>% 
  as.data.table()

# encode NA data as mean over rows
k <- which(is.na(data_p), arr.ind=TRUE)
data_p[k] <- rowMeans(data_p[,-1], na.rm=TRUE)[k[,1]]
data_p$`2020` <- round(rowMeans(data_p[,-1]),2) # set data for 2020 as mean over rows

data_p$mean <- round(rowMeans(data_p[,-1]),2)

scheme <- fread("./data/orig/classification.csv", sep="\t", header = TRUE)
scheme$`Price Description (as of 24.09.2021, subject to changes!)` <- tolower(scheme$`Price Description (as of 24.09.2021, subject to changes!)`)
data_p$Item <- tolower(data_p$Item)

temp <- scheme %>% 
  select(`Crop Class ID`, `Price Description (as of 24.09.2021, subject to changes!)`) %>% 
  as.data.table()
colnames(temp) <- c('ID', 'Item')
temp[7,2] <- "triticale"

data_price <- inner_join(temp, data_p, by = "Item") %>% 
  # select(-"mean") %>% #only take years average
  as.data.table()
dat <- data_price %>% 
  # select(-Item) %>% 
  select(c(ID,mean)) %>% 
  arrange(ID) %>% 
  as.data.table()

dat %>% 
  ggplot(aes(x=ID,y=mean))+
  geom_bar()

ggplot(data_price) + 
  geom_line(mapping = aes(x = ID, y = mean))

ggplot(data_price) + 
  geom_bar(mapping = aes(x = ID, y=mean), stat="identity")

corr <- ggplot() + 
  geom_bar(data=data, mapping = aes(x = CType))+
  geom_line(data=data_price, mapping = aes(x = ID, y = mean*500), color="red")+
  labs(x="Crop Type", y="")
save(corr, file="./output/correlation.RData")


# change of price over the years

data_price_raw <- fread("./data/orig/price.csv", sep=",", header = T)
data_p <- data_price_raw %>% 
  mutate(Item = recode(Item, "Vegetables, leguminous nes" = "Vegetables, leguminous")) %>% 
  filter(Unit == 'USD') %>%
  as.data.table()

data_p <- data_p %>% 
  select(Item, Year, Value) %>% 
  as.data.table()

price_per_year_per_crop<- function(crop){
  data_p %>% 
    filter(Item==crop) %>% 
    as.data.table() %>% 
    ggplot()+
    geom_line(mapping=aes(x=Year,y=Value))+
    ggtitle(crop)+
    geom_vline(xintercept = 2012, color="red")+
    geom_vline(xintercept = 2016, color="red")
}
cr <- "Barley"#####################
price_per_year_per_crop(cr)
cr <- "Carrots and turnips"
price_per_year_per_crop(cr)
cr <- "Maize"#######################
price_per_year_per_crop(cr)
cr <- "Oats"
price_per_year_per_crop(cr)
cr <- "Onions, dry"
price_per_year_per_crop(cr)
cr <- "Potatoes"
price_per_year_per_crop(cr)
cr <- "Rapeseed"
price_per_year_per_crop(cr)
cr <- "Rye"
price_per_year_per_crop(cr)
cr <- "Sugar beet"
price_per_year_per_crop(cr)
cr <- "Triticale"
price_per_year_per_crop(cr)
cr <- "Wheat" ####################
price_per_year_per_crop(cr)
cr <- "Vegetables, leguminous"
price_per_year_per_crop(cr)
cr <- "Onions, shallots, green"
price_per_year_per_crop(cr)
cr <- "Sunflower seed"
price_per_year_per_crop(cr)


test <- data_p %>% 
  group_by(Year) %>% 
  mutate(test=mean(Value)) %>% 
  as.data.table()

mean_price <- test %>% 
  group_by(Year) %>% 
  as.data.table() %>% 
  ggplot()+
  geom_line(mapping=aes(x=Year,y=test))+
  ggtitle("mean price for all crops")+
  geom_vline(xintercept = 2012, color="red")+
  geom_vline(xintercept = 2016, color="red")

save(mean_price,file="./output/mean_price_crops.RData")

# crop frequency throughout the years

freq_crops_per_year <- function(year){
  data %>% 
    filter(Year == year) %>% 
    as.data.table() %>% 
    ggplot() + 
      geom_bar(mapping = aes(x = CType))+
      ggtitle(paste("Most frequent crop type for", year))
}
freq_crops_per_year(2005)
freq_crops_per_year(2006)
freq_crops_per_year(2007)
freq_crops_per_year(2008)
freq_crops_per_year(2009)
freq_crops_per_year(2010)
freq_crops_per_year(2011)
freq_crops_per_year(2012)
freq_crops_per_year(2013)
freq_crops_per_year(2014)
freq_crops_per_year(2015)
freq_crops_per_year(2016)
freq_crops_per_year(2017)
freq_crops_per_year(2018)
freq_crops_per_year(2019)
ctype_by_year_plot <- freq_crops_per_year(2020)
save(ctype_by_year_plot,file="./output/freq_plot_2020.Rdata")
data %>% 
  ggplot()+
  geom_bar(mapping=aes(x=CType))+
  ggtitle("Most frequent crop type for all years")

# yearly mean correlation with price
