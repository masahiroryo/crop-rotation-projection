# load packages -------------------------------------------------------------------------------

library(data.table)
library(dtplyr)
library(dplyr, warn.conflicts = FALSE)

# read crop data ------------------------------------------------------------------------------

data_ref <- fread("./data/ref.csv", sep =",", header = T)
data_crop_raw <- fread("./data/crop.csv", sep=",", header = T)

data_ref$x_coord <- round(data_ref$x_coord)
data_ref$y_coord <- round(data_ref$y_coord)
data_crop_raw$x_coord <- round(data_crop_raw$x_coord)
data_crop_raw$y_coord <-round(data_crop_raw$y_coord)

data_crop <- inner_join(data_ref, data_crop_raw, by = c("x_coord", "y_coord")) %>% 
  select(-c("state")) %>% 
  as.data.table()

rm(data_crop_raw,data_ref)
gc()

resh_crop <- melt(data_crop, id = c("FID", "State", "OBJECTID", "cell_id", "x_coord", "y_coord"))
names(resh_crop) <- c("FID", "State", "OBJECTID", "cell_id", "x_coord", "y_coord", "Year", "crtype")
resh_crop$crtype <- replace(resh_crop$crtype, resh_crop$crtype == 8, 4)
resh_crop$crtype <- replace(resh_crop$crtype, resh_crop$crtype == 2, 1)
resh_crop$crtype <- replace(resh_crop$crtype, resh_crop$crtype == 14, 13)

# read climate data ---------------------------------------------------------------------------

# temperature
data_temp <- fread("./data/temp.csv", sep=",", header = T)
temp <- data.frame("cell_id"=data_temp$cell_id)

i <- 5
j <- 4

for (i in 5:20){
  temp[paste(i,"tAVG",sep="")] <- data.frame(rowMeans(data_temp[,j:(j+5)]/10)) #March to August
  temp[paste(i,"tAM",sep="")] <- data.frame(rowMeans(data_temp[,(j+1):(j+2)]/10)) #April to May
  temp[paste(i,"tJJ",sep="")] <- data.frame(rowMeans(data_temp[,(j+3):(j+4)]/10)) #June to July
  j <- j+12
}

data_temp <- inner_join(temp, data_crop, by = c("cell_id")) %>% 
  filter(OBJECTID > 0) %>% 
  select(1:50) %>% 
  as.data.table()

rm(temp,i,j)
gc()

resh_temp <- melt(data_temp[,c(1,2,5,8,11,14,17,20,23,26,29,32,35,38,41,44,47,50)], id = c("cell_id", "OBJECTID"))
resh_temp2 <- melt(data_temp[,c(1,3,6,9,12,15,18,21,24,27,30,33,36,39,42,45,48,50)], id = c("cell_id", "OBJECTID"))
resh_temp3 <- melt(data_temp[,c(1,4,7,10,13,16,19,22,25,28,31,34,37,40,43,46,49,50)], id = c("cell_id", "OBJECTID"))
resh_temp <- full_join(resh_temp,resh_temp2, by=c("cell_id","OBJECTID"))
resh_temp <- as.data.frame(full_join(resh_temp,resh_temp3, by=c("cell_id","OBJECTID")))

resh_temp <- resh_temp[,-c(5:7,9:11)]
names(resh_temp) <- c("cell_id", "OBJECTID", "Year", "TempAVG", "TempAM", "TempJJ")



# precipitation 
data_prec <- fread("./data/prec.csv", sep=",", header = T)
temp <- data.frame("cell_id"=data_prec$cell_id)

i <- 5
j <- 4

for (i in 5:20){
  temp[paste(i,"tAVG",sep="")] <- data.frame(rowMeans(data_prec[,j:(j+5)]/10)) #March to August
  temp[paste(i,"tAM",sep="")] <- data.frame(rowMeans(data_prec[,(j+1):(j+2)]/10)) #April to May
  temp[paste(i,"tJJ",sep="")] <- data.frame(rowMeans(data_prec[,(j+3):(j+4)]/10)) #June to July
  j <- j+12
}

data_prec <- inner_join(temp, data_crop, by = c("cell_id")) %>% 
  filter(OBJECTID > 0) %>% 
  select(1:50) %>% 
  as.data.table()

rm(temp,i,j)
gc()

resh_prec <- melt(data_prec[,c(1,2,5,8,11,14,17,20,23,26,29,32,35,38,41,44,47,50)], id = c("cell_id", "OBJECTID"))
resh_prec[,5:8] <- melt(data_prec[,c(1,3,6,9,12,15,18,21,24,27,30,33,36,39,42,45,48,50)], id = c("cell_id", "OBJECTID"))
resh_prec[,9:12] <- melt(data_prec[,c(1,4,7,10,13,16,19,22,25,28,31,34,37,40,43,46,49,50)], id = c("cell_id", "OBJECTID"))
resh_prec <- resh_prec[,-c(5:7,9:11)]
names(resh_prec) <- c("cell_id", "OBJECTID", "Year", "PrecAVG", "PrecAM", "PrecJJ")

# radiation 
data_rad <- fread("./data/rad.csv", sep=",", header = T)

temp <- data.frame("cell_id"=data_rad$cell_id)

i <- 5
j <- 4

for (i in 5:20){
  temp[paste(i,"tAVG",sep="")] <- data.frame(rowMeans(data_rad[,j:(j+5)]/10)) #March to August
  temp[paste(i,"tAM",sep="")] <- data.frame(rowMeans(data_rad[,(j+1):(j+2)]/10)) #April to May
  temp[paste(i,"tJJ",sep="")] <- data.frame(rowMeans(data_rad[,(j+3):(j+4)]/10)) #June to July
  j <- j+12
}

data_rad <- inner_join(temp, data_crop, by = c("cell_id")) %>% 
  filter(OBJECTID > 0) %>% 
  select(1:50) %>% 
  as.data.table()

rm(temp,i,j)
gc()

resh_rad <- melt(data_rad[,c(1,2,5,8,11,14,17,20,23,26,29,32,35,38,41,44,47,50)], id = c("cell_id", "OBJECTID"))
resh_rad[,5:8] <- melt(data_rad[,c(1,3,6,9,12,15,18,21,24,27,30,33,36,39,42,45,48,50)], id = c("cell_id", "OBJECTID"))
resh_rad[,9:12] <- melt(data_rad[,c(1,4,7,10,13,16,19,22,25,28,31,34,37,40,43,46,49,50)], id = c("cell_id", "OBJECTID"))

resh_rad <- resh_rad[,-c(5:7,9:11)]

names(resh_rad) <- c("cell_id", "OBJECTID", "Year", "RadAVG", "RadAM", "RadJJ")

# soil data

data_soil <- fread("./data/soil.csv", sep=",", header = T)

data_soil$x_coord <- round(data_soil$x_coord)
data_soil$y_coord <- round(data_soil$y_coord)
# soil Type

#Find the corresponding x and y coordinates from crop

test <- inner_join(data_crop, data_soil, by = c("cell_id")) %>% 
  filter(OBJECTID > 0) %>% 
  distinct(OBJECTID, .keep_all = TRUE) %>% 
  select(6:8,10) %>% 
  as.data.table()


idx4 <- na.omit(unique(crspd$OBJECTID.x)) # Check for duplicates and erase them
#idx3 <- which(duplicated(crspd$OBJECTID.x[idx1])) # Check for duplicates and erase them

idx3 <- which (crspd$OBJECTID.x==crspd$OBJECTID.y)

if (length(idx4) == 0) {
  resh.stype <- crspd[idx3,]
} else {
  resh.stype <- crspd[idx3,]
  #resh.stype <- resh.stype[-c(idx4),]
}