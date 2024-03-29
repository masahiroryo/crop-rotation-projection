# load packages -------------------------------------------------------------------------------

library(data.table)
library(dtplyr)
library(tidyverse)
library(ggsci)
library(scales)

theme_set(theme_bw())
primary_color = '#4477AA'
secondary_color = '#228833'

# read data -----------------------------------------------------------------------------------

set <- "set2"
file_name <- paste("./data/clean/", set, ".csv", sep="")
data <- fread(file_name, sep=",", header=TRUE)

classification <- read.csv("./data/orig/classification.csv",sep="\t")[,1:2]

# summary statistics --------------------------------------------------------------------------

# frequency of each crop type
data$CType <- as.factor(data$CType)
c <- vector("character", nrow(data))
i<-1
for (type in data$CType) {
  c[i] = classification$Crop.Class.Name[classification$Crop.Class.ID==type]
  i=i+1
}
data$CType.Name <- c

freq_plot <- ggplot(data) + 
  geom_bar(mapping = aes(x = CType.Name), fill=primary_color)+
  labs(x="", y="Frequency", title="Frequence of each crop type")+
  theme(plot.title = element_text(margin = margin(10, 0, 10, 0), size = 14),
        axis.text.x = element_text(angle=45, hjust = 1))
fname = paste("./output/crop_frequency_", set, ".png", sep="")
png(filename=fname)
freq_plot
dev.off()

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

data_small <- data %>% 
  sample_n(500000) %>% 
  as.data.table()

plot_crops_per_year <- function(year){
  dat <- data_small %>% 
    filter(Year == year) %>% 
    as.data.table()
  
  ggplot(data = dat) + 
    geom_point(mapping = aes(x = X, y = Y), position = "jitter", stroke = 0.1)+
    coord_cartesian(xlim = c(4065000,4650000), ylim= c(2720000, 3600000))
}
plot_crops_per_year(2020)
plot_crops_per_year(2020)
# , color=factor(CType)

library(sf)
library(rnaturalearth)
library(rnaturalearthdata)
library(terra)
points <- cbind(data_small$X, data_small$Y)
v <- vect(points, crs="+proj=utm +zone=10 +datum=WGS84  +units=m")
v
y <- project(v, "+proj=longlat +datum=WGS84")
y
lonlat <- as.data.frame(geom(y)[, c("x", "y")])
head(lonlat, 3)

germany <- ne_countries(scale="medium", returnclass="sf", country="germany")
ggplot()+
  geom_sf(data=lonlat)
  geom_sf(data=germany)

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
  
  res <- lapply(c(1:(length(unique(data$CType)))), function(x){
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

ggplot(data_price) + 
  geom_line(mapping = aes(x = ID, y = mean))

ggplot(data_price) + 
  geom_bar(mapping = aes(x = ID, y=mean), stat="identity")

corr <- ggplot() + 
  geom_bar(data=data, mapping = aes(x = CType))+
  geom_line(data=data_price, mapping = aes(x = ID, y = mean*1000), color="red")+
  labs(x="Crop Type", y="")
corr
# save(corr, file="./output/correlation_BV.RData")

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
mean_price
# save(mean_price,file="./output/mean_price_crops_BV.RData")

# crop frequency throughout the years

freq_crops_per_year <- function(year){
  print(year)
  data %>% 
    filter(Year %in% year) %>% 
    as.data.table() %>% 
    ggplot() + 
      geom_bar(mapping = aes(x = CType))
}
freq_crops_per_year(c(2005))
ctype_by_year_plot <- freq_crops_per_year(c(2018,2019,2020))
# save(ctype_by_year_plot,file="./output/freq_plot_3year_BV.Rdata")

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


# price x freq ------------------------------------------------------------------------------------------

dat <- as.data.frame(table(data$price))
colnames(dat) <- c("price", "freq")

ggplot(data=dat)+
  geom_point(mapping=aes(x=price,y=freq))

dat<-as.data.frame(table(data$price, data$CType))
colnames(dat)<-c("price","ctype","freq")
dat %>% 
  filter(freq>0) %>% 
  ggplot(mapping=aes(x=price,y=freq, color=ctype))+
    geom_point()#+
    scale_x_continuous(breaks = scales::breaks_extended())
    

# data diagnostics --------------------------------------------------------
crops_of_interest <- c("Maize (silage)", "Grassland", "Legumes", "Spring Wheat, triticale and rye",
                   "Winter barley", "Winter wheat", "Maize (grain)")
    
rel <- data %>%  
  count(CType.Name, Year) %>%
  group_by(Year) %>% 
  mutate(freq = n / sum(n)) %>% 
  ungroup() %>% 
  select(-n) %>% 
  filter(CType.Name %in% crops_of_interest) %>% 
  #filter(CType %in% c(11:18)) %>% 
  as.data.frame()

    
#png(filename="./output/freq_0520.png")
ggplot(rel, aes(y=(freq*100),x=Year, color=CType.Name))+
  labs(y="Relative Frequency", x="Year", title=paste("Relative Frequency for each Year"))+
  geom_line(size=1)+
  geom_point()+
  scale_color_npg()
#dev.off()
