# load packages -------------------------------------------------------------------------------

library(data.table)
library(dtplyr)
library(tidyverse)

# read data -----------------------------------------------------------------------------------

data <- fread("./data/clean/data_clean_BV.csv", sep=",", header=TRUE)

# summary statistics --------------------------------------------------------------------------

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
freq_crops_per_year(2005)
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

# Proportion of crop types -> Crops of minor importance 
prop_crtype_per_year <- function(y){
  dat <- data %>% 
    filter(Year == y) %>% 
    as.data.table()
  N <- sum(nrow(dat))
  
  res <- lapply(c(1:17), function(x){
    return((sum(dat$CType==x))/N)
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

# crop sequence -----------------------------------------------------------------------------------------

# library(parallel)
# cl = makeCluster(6L)
# clusterExport(cl, list("data", "data_price"))
# clusterEvalQ(cl, { library(data.table) })

dat <- data %>% 
  select(OBJECTID, Year, CType) %>% 
  arrange(OBJECTID) %>% 
  as.data.table()
gc()

crop_sequences <- dat %>% 
  pivot_wider(names_from = Year, values_from=CType, values_fn = list(. = mean)) %>% 
  as.data.table()
rm(dat,data)
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
most_freq <- get_most_frequent_sequences(freq_matrix, 4)

# price and ctype ---------------------------------------------------------------------------------------

# get mean price over all years
print("starting economy data")

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
data_p$`2020` <- round(rowMeans(data_p[,-1]),2) 

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
  as.data.table()
dat <- data_price %>% 
  select(-Item) %>%
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
  geom_line(data=data_price, mapping = aes(x = ID, y = mean*1000), color="red")+
  labs(x="Crop Type", y="")
save(corr, file="./output/correlation_BV.RData")

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


temp <- data_p %>% 
  group_by(Year) %>% 
  mutate(temp=mean(Value)) %>% 
  as.data.table()

mean_price <- temp %>% 
  group_by(Year) %>% 
  as.data.table() %>% 
  ggplot()+
  geom_line(mapping=aes(x=Year,y=temp))+
  ggtitle("mean price for all crops")+
  geom_vline(xintercept = 2012, color="red")+
  geom_vline(xintercept = 2016, color="red")

save(mean_price,file="./output/mean_price_crops_BV.RData")

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
save(ctype_by_year_plot,file="./output/freq_plot_2020_BV.Rdata")
data %>% 
  ggplot()+
  geom_bar(mapping=aes(x=CType))+
  ggtitle("Most frequent crop type for all years")

# freq of price per year --------------------------------------------------------------------------------

dat <- data %>% 
  filter(CType==5) %>% 
  select(CType,Year, price) %>% 
  group_by(Year) %>% 
  mutate(freq = n()) %>% 
  filter(unique(Year)) %>% 
  as.data.table()

test<-function(ctype){
  dat <- data %>% 
    filter(CType==ctype) %>% 
    select(CType,Year, price) %>% 
    group_by(Year) %>% 
    mutate(freq = n()) %>% 
    filter(unique(Year)) %>% 
    as.data.table()
  
  dat %>% 
    ggplot(mapping=aes(x=price,y=freq))+
    geom_point()+
    geom_label(label=dat$Year)+
    ggtitle(paste("price x freq for ctype", ctype))
    
}
test(2)
test(3)
test(4)
test(5)
test(6)
test(7)
test(8)
test(9)
test(10)
test(12)





