
library(data.table)
library(tidyverse)
library(reshape)
library(reshape2)

start_time <- Sys.time()
print("starting process ...")

### Paths and file names ###

mainPath <- "/home/pietn/Master/thesis"

crop <- "crop.csv"
temp <- "temp.csv"
prec <- "prec.csv"
rad <- "rad.csv"
ref <- "ref.csv"
soil <- "soil.csv"

# Amount of samples and seed

n=1000
sd=1

### Extract data, check for NAs the class of each variable, and pre-process each variable ###

## Reference File #######################################
setwd(paste(mainPath,"/data", sep = ""))
dataRef <- fread(ref, sep = ",", header = T)
dataRef$x_coord <- round(dataRef$x_coord)
dataRef$y_coord <- round(dataRef$y_coord)

#Select n amount of samples from crop and find their corresponding OBJECTID and cell_id from ref
set.seed(seed = sd)

## Crop Data ############################################
print("starting crop data ...")
#Read
setwd(paste(mainPath,"/data", sep = ""))

dataCrop <- fread(crop, sep=",", header = T) 
 
names(dataCrop) <- gsub(x = names(dataCrop), pattern = "crop_", replacement = "")  

dataCrop$x_coord <- round(dataCrop$x_coord)
dataCrop$y_coord <- round(dataCrop$y_coord)

#which(is.na(dataCrop))
#sapply(dataCrop,class)

#Find the corresponding OBJECTID and cell_id from ref
crspd <- Reduce(function(...) merge(..., all = TRUE, by = c("x_coord", "y_coord")), list(dataRef, dataCrop))
crspd <- crspd[,-c(4)]

idx1 <- which(crspd$OBJECTID > 0)
#idx2 <- na.omit(unique(crspd$OBJECTID)) # Check for duplicates and erase them
idx2 <- which(duplicated(crspd$OBJECTID[idx1])) # Check for duplicates and erase them

if (length(idx2) == 0) {
  sampleDE_crop <- crspd[idx1]
} else {
  sampleDE_crop <- crspd[idx1]
  sampleDE_crop <- sampleDE_crop[-c(idx2[c(2:5)]),]
}

sampleDE_crop <- sampleDE_crop[order(sampleDE_crop$OBJECTID,)]
dataRef <- dataRef[order(dataRef$OBJECTID,)]

#Reshape and replace crop codes according to price ! To be updated @prices !

resh.crop <- melt(sampleDE_crop, id = c("FID", "State", "OBJECTID", "cell_id", "x_coord", "y_coord"))
names(resh.crop) <- c("FID", "State", "OBJECTID", "cell_id", "x_coord", "y_coord", "Year", "crtype")
resh.crop$crtype <- replace(resh.crop$crtype, resh.crop$crtype == 8, 4)
resh.crop$crtype <- replace(resh.crop$crtype, resh.crop$crtype == 2, 1)
resh.crop$crtype <- replace(resh.crop$crtype, resh.crop$crtype == 14, 13)

crop_data_time <- Sys.time()
print("-----------------------------")
print("finished Crop Data processing")
print(crop_data_time-start_time)

#write.csv(resh.crop, "./reshaped_crop.csv", row.names = FALSE)
rm(crspd,dataCrop,resh.crop,sampleDE_crop)
gc()

## Climate Data #########################################
print("starting climate data ...")
#Temperature --------------------------------------------
print("starting temperature data ...")
setwd(paste(mainPath,"/data", sep = ""))

dataTemp <- fread(temp, sep=",", header = T)
dataTemp1 <- data.frame("cell_id"=dataTemp$cell_id)

i <- 5
j <- 4

for (i in 5:20){
  dataTemp1[paste(i,"tAVG",sep="")] <- data.frame(rowMeans(dataTemp[,j:(j+5)]/10)) #March to August
  dataTemp1[paste(i,"tAM",sep="")] <- data.frame(rowMeans(dataTemp[,(j+1):(j+2)]/10)) #April to May
  dataTemp1[paste(i,"tJJ",sep="")] <- data.frame(rowMeans(dataTemp[,(j+3):(j+4)]/10)) #June to July
  j <- j+12
}

dataTemp <- dataTemp1

crspd <- Reduce(function(...) merge(..., all = TRUE, by = c("cell_id")), list(dataTemp1, dataRef))
idx <- which(crspd$OBJECTID > 0)

sampleDE_temp <- crspd[idx,1:50]
#sampleDE_temp <- sort(sampleDE_temp, by="cell_id")

#Reshape
rm(crspd, dataTemp, dataTemp1)
gc()
resh.temp <- melt(sampleDE_temp[,c(1,2,5,8,11,14,17,20,23,26,29,32,35,38,41,44,47,50)], id = c("cell_id", "OBJECTID"))
resh.temp[,5:8] <- melt(sampleDE_temp[,c(1,3,6,9,12,15,18,21,24,27,30,33,36,39,42,45,48,50)], id = c("cell_id", "OBJECTID"))
resh.temp[,9:12] <- melt(sampleDE_temp[,c(1,4,7,10,13,16,19,22,25,28,31,34,37,40,43,46,49,50)], id = c("cell_id", "OBJECTID"))

resh.temp <- resh.temp[,-c(5:7,9:11)]
names(resh.temp) <- c("cell_id", "OBJECTID", "Year", "TempAVG", "TempAM", "TempJJ")

resh.temp$Year <- recode(resh.temp$Year, 
                    "5tAVG" = "2005",
                    "6tAVG" = "2006",
                    "7tAVG" = "2007",
                    "8tAVG" = "2008",
                    "9tAVG" = "2009",
                    "10tAVG" = "2010",
                    "11tAVG" = "2011",
                    "12tAVG" = "2012",
                    "13tAVG" = "2013",
                    "14tAVG" = "2014",
                    "15tAVG" = "2015",
                    "16tAVG" = "2016",
                    "17tAVG" = "2017",
                    "18tAVG" = "2018",
                    "19tAVG" = "2019",
                    "20tAVG" = "2020")

#write.csv(resh.temp, "./reshaped_temp.csv", row.names = FALSE)
rm(resh.temp, sampleDE_temp)
gc()

print("finished")
#Precipitation ------------------------------------------
print("starting precipitation data ...")

setwd(paste(mainPath,"/data", sep = ""))
dataPrec <- fread(prec, sep=",", header = T)

dataPrec1 <- data.frame("cell_id"=dataPrec$cell_id)

i <- 5
j <- 4

for (i in 5:20){
  dataPrec1[paste(i,"pAVG",sep="")] <- data.frame(rowSums(dataPrec[,j:(j+5)]/10)) #March to August
  dataPrec1[paste(i,"pAM",sep="")] <- data.frame(rowSums(dataPrec[,(j+1):(j+2)]/10)) #April to May
  dataPrec1[paste(i,"pJJ",sep="")] <- data.frame(rowSums(dataPrec[,(j+3):(j+4)]/10)) #June to July
  j <- j+12
}

dataPrec <- dataPrec1

#Find the corresponding OBJECTID and cell_id from ref

crspd <- Reduce(function(...) merge(..., all = TRUE, by = c("cell_id")), list(dataPrec, dataRef))
idx <- which(crspd$OBJECTID > 0)

sampleDE_prec <- crspd[idx,1:50]
sampleDE_prec <- sort(sampleDE_prec, by="cell_id")

#Reshape
rm(crspd, dataPrec, dataPrec1)
gc()
resh.prec <- melt(sampleDE_prec[,c(1,2,5,8,11,14,17,20,23,26,29,32,35,38,41,44,47,50)], id = c("cell_id", "OBJECTID"))
resh.prec[,5:8] <- melt(sampleDE_prec[,c(1,3,6,9,12,15,18,21,24,27,30,33,36,39,42,45,48,50)], id = c("cell_id", "OBJECTID"))
resh.prec[,9:12] <- melt(sampleDE_prec[,c(1,4,7,10,13,16,19,22,25,28,31,34,37,40,43,46,49,50)], id = c("cell_id", "OBJECTID"))

resh.prec <- resh.prec[,-c(5:7,9:11)]

names(resh.prec) <- c("cell_id", "OBJECTID", "Year", "PrecAVG", "PrecAM", "PrecJJ")
resh.prec$Year <- recode(resh.prec$Year, 
                         "5pAVG" = "2005",
                         "6pAVG" = "2006",
                         "7pAVG" = "2007",
                         "8pAVG" = "2008",
                         "9pAVG" = "2009",
                         "10pAVG" = "2010",
                         "11pAVG" = "2011",
                         "12pAVG" = "2012",
                         "13pAVG" = "2013",
                         "14pAVG" = "2014",
                         "15pAVG" = "2015",
                         "16pAVG" = "2016",
                         "17pAVG" = "2017",
                         "18pAVG" = "2018",
                         "19pAVG" = "2019",
                         "20pAVG" = "2020")


write.csv(resh.prec, "./reshaped_prec.csv", row.names = FALSE)
rm(resh.prec, sampleDE_prec)
gc()

print("finished")
#Radiation ----------------------------------------------
print("starting radiation data ...")

setwd(paste(mainPath,"/data", sep = ""))

dataRad <- fread(rad, sep=",", header = T)
dataRad1 <- data.frame("cell_id"=dataRad$cell_id)

i <- 5
j <- 4

for (i in 5:20){
  dataRad1[paste(i,"rAVG",sep="")] <- data.frame(rowMeans(dataRad[,j:(j+5)]/10)) #March to August
  dataRad1[paste(i,"rAM",sep="")] <- data.frame(rowMeans(dataRad[,(j+1):(j+2)]/10)) #April to May
  dataRad1[paste(i,"rJJ",sep="")] <- data.frame(rowMeans(dataRad[,(j+3):(j+4)]/10)) #June to July
  j <- j+12
}

dataRad <- dataRad1

#Find the corresponding OBJECTID and cell_id from ref

crspd <- Reduce(function(...) merge(..., all = TRUE, by = c("cell_id")), list(dataRad, dataRef))
idx <- which(crspd$OBJECTID > 0)

sampleDE_rad <- crspd[idx,1:50]
sampleDE_rad <- sort(sampleDE_rad, by="cell_id")

#Reshape
rm(crspd, dataRad,dataRad1)
gc()

resh.rad <- melt(sampleDE_rad[,c(1,2,5,8,11,14,17,20,23,26,29,32,35,38,41,44,47,50)], id = c("cell_id", "OBJECTID"))
resh.rad[,5:8] <- melt(sampleDE_rad[,c(1,3,6,9,12,15,18,21,24,27,30,33,36,39,42,45,48,50)], id = c("cell_id", "OBJECTID"))
resh.rad[,9:12] <- melt(sampleDE_rad[,c(1,4,7,10,13,16,19,22,25,28,31,34,37,40,43,46,49,50)], id = c("cell_id", "OBJECTID"))

resh.rad <- resh.rad[,-c(5:7,9:11)]

names(resh.rad) <- c("cell_id", "OBJECTID", "Year", "RadAVG", "RadAM", "RadJJ")
resh.rad$Year <- recode(resh.rad$Year, 
                         "5rAVG" = "2005",
                         "6rAVG" = "2006",
                         "7rAVG" = "2007",
                         "8rAVG" = "2008",
                         "9rAVG" = "2009",
                         "10rAVG" = "2010",
                         "11rAVG" = "2011",
                         "12rAVG" = "2012",
                         "13rAVG" = "2013",
                         "14rAVG" = "2014",
                         "15rAVG" = "2015",
                         "16rAVG" = "2016",
                         "17rAVG" = "2017",
                         "18rAVG" = "2018",
                         "19rAVG" = "2019",
                         "20rAVG" = "2020")

#write.csv(resh.rad, "./reshaped_rad.csv", row.names = FALSE)
rm(resh.rad, sampleDE_rad)
gc()

## Soil data ##############################################
print("starting soil data ...")
#Read 

setwd(paste(mainPath,"/data", sep = ""))
dataSoil <- fread(soil, sep=",", header = T)
#which(is.na(dataSoil))
#sapply(dataSoil,class)
#dataSoil <- dataSoil[order(dataSoil$x_coord),]
dataSoil$x_coord <- round(dataSoil$x_coord)
dataSoil$y_coord <- round(dataSoil$y_coord)

#dataRef_DE <- dataRef_DE[order(dataRef_DE$x_coord),]

#Soil Type -----------------------------------------------
#Find the corresponding x and y coordinates from crop
#crspd <- Reduce(function(...) merge(..., all = TRUE, by = c("x_coord", "y_coord")), list(resh.crop,dataSoil))
#crspd <- crspd[,-c(6:8,10)]
crspd <- Reduce(function(...) merge(..., all = TRUE, by = c("x_coord", "y_coord")), list(dataRef,dataSoil))

#idx1 <- which(crspd$OBJECTID.x > 0)
idx4 <- na.omit(unique(crspd$OBJECTID.x)) # Check for duplicates and erase them
#idx3 <- which(duplicated(crspd$OBJECTID.x[idx1])) # Check for duplicates and erase them

idx3 <- which (crspd$OBJECTID.x==crspd$OBJECTID.y)

if (length(idx4) == 0) {
  resh.stype <- crspd[idx3,]
} else {
  resh.stype <- crspd[idx3,]
  #resh.stype <- resh.stype[-c(idx4),]
}

resh.stype <- melt(resh.stype, id = c("slope1000", "dem1000", "buek1000", "OBJECTID.x", "cell_id", "x_coord", "y_coord"))
resh.stype <- resh.stype[,-c(8,9)]
names(resh.stype)[names(resh.stype) == 'OBJECTID.x'] <- 'OBJECTID'

#write.csv(resh.stype, "./reshaped_soil.csv", row.names = FALSE)

################### Create the data formatting used by the crop rotation  ###################

## Order them so that the values match ! ##
resh.stype <- sort(resh.stype, by="OBJECTID")
resh.crop <- sort(resh.crop, by="OBJECTID")
resh.prec <- sort(resh.prec, by="OBJECTID")
resh.rad <- sort(resh.rad, by="OBJECTID")
resh.temp <- sort(resh.temp, by="OBJECTID")

identical(resh.crop$OBJECTID, resh.stype$OBJECTID)
identical(resh.crop$OBJECTID, resh.prec$OBJECTID)
identical(resh.crop$OBJECTID, resh.temp$OBJECTID)
identical(resh.crop$OBJECTID, resh.rad$OBJECTID)
print("finished")
## Price ------------------------------------------------- Needs to be updated!
print("starting price data ...")
#setwd(paste(mainPath,"/OrigData", sep = ""))
#dataPrice <- fread("price.csv", sep = ",", header = T)
#dataPrice <- dataPrice[367:549,c("Year", "Item", "Value","Element")]

#cropsPrice <- unique(dataPrice$Item)
#cropsCrop <- c("4", "17", "1", "9", "16", "12", "10", "5", "11", "6", "3", "13", "0", "15")
#crops <- data.frame(cropsPrice, cropsCrop)
#dataPrice$crtype <- crops$cropsCrop[match(dataPrice$Item, cropsPrice)]
#dataPrice$crtype <- as.numeric(dataPrice$crtype)
#dataPrice$Year <- as.numeric(dataPrice$Year)

yearOld <- unique(resh.crop$Year)
yearNew <- c(2005, 2006, 2007, 2008, 2009, 2010, 2011, 2012, 2013, 2014, 2015, 2016, 2017, 2018, 2019, 2020)
resh.crop$Year1[resh.crop$Year %in% yearOld] <- yearNew[match(resh.crop$Year, yearOld)]
resh.crop$Year <- resh.crop$Year1

#crspd <- Reduce(function(...) merge(..., all = TRUE, by = c("Year", "crtype")), list(dataPrice, resh.crop))
#idx5 <- which(crspd$OBJECTID>0)

#resh.price <- crspd[c(idx5),]

data <- data.frame(resh.crop$Year, resh.crop$x_coord, resh.crop$y_coord, resh.crop$crtype, 
                   resh.temp$TempAVG, resh.temp$TempAM, resh.temp$TempJJ, resh.prec$PrecAVG,
                   resh.prec$PrecAM, resh.prec$PrecJJ, resh.rad$RadAVG, resh.rad$RadAM, 
                   resh.rad$RadJJ, resh.stype$buek1000, resh.stype$dem1000, resh.stype$slope1000)
names(data) <- c("Year", "X", "Y", "CType", "tAVG", "tAM", "tJJ", "pAVG", "pAM", "pJJ",
                 "rAVG", "rAM", "rJJ", "SType", "SElev", "SSlope")

#Erase workspace, and perform garbage collection
gc() 
rm(resh.crop, resh.prec, resh.rad, resh.stype, resh.temp,
   crop, i, idx, idx1, idx2, idx3, idx4, j, prec, rad, ref, soil, temp, yearNew, yearOld)

#}
print("finished")
end_time <- Sys.time()
print("--------------------------------")
print("finished Climate Data processing")
print(end_time - crop_data_time)

print("---------------------------------")
print("finished Complete Data processing")
print(end_time-start_time)

#write.csv(data, "D:/Projects_IACS/Database_DE/CropRotPred_DE/Output_210912/dataDE_5000_5.csv", row.names = FALSE)












