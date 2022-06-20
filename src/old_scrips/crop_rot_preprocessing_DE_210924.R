
library(data.table)
library(tidyverse)
library(reshape)
library(reshape2)

start_time <- Sys.time()
#preProcessing <- function(mainPath, fedState, n)
#{
   ### Sort Function
   sort.data.frame <- function(x, decreasing=FALSE, by=1, ... ){
      f <- function(...) order(...,decreasing=decreasing)
      i <- do.call(f,x[by])
      x[i,,drop=FALSE]
      }

### Paths and file names ###

mainPath <- "F:\dev\Master\thesis\data\orig"

crop <- "crop.csv"
temp <- "temp.csv"
prec <- "prec.csv"
rad <- "rad.csv"
ref <- "ref.csv"
soil <- "soil.csv"

# Amount of samples and seed

n=100
sd=1

### Extract data, check for NAs the class of each variable, and pre-process each variable ###

## Reference File #######################################
setwd(paste(mainPath,"/OrigData", sep = ""))
dataRef <- fread(ref, sep = ",", header = T)
dataRef$x_coord <- as.character(round(dataRef$x_coord))
dataRef$y_coord <- as.character(round(dataRef$y_coord))

#Select n amount of samples from crop and find their corresponding OBJECTID and cell_id from ref
set.seed(seed = sd)
sampleDE_ref <- sample_n(dataRef, n, replace = F)

## Crop Data ############################################

#Read
setwd(paste(mainPath,"/OrigData", sep = ""))
dataCrop <- fread(crop, sep=",", header = T)
dataCrop$x_coord <- as.character(round(dataCrop$x_coord))
dataCrop$y_coord <- as.character(round(dataCrop$y_coord))

#which(is.na(dataCrop))
#sapply(dataCrop,class)

#Find the corresponding OBJECTID and cell_id from ref
crspd <- Reduce(function(...) merge(..., all = TRUE, by = c("x_coord", "y_coord")), list(sampleDE_ref, dataCrop))
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
sampleDE_ref <- sampleDE_ref[order(sampleDE_ref$OBJECTID,)]

test_crop <- which(sampleDE_crop$OBJECTID==sampleDE_ref$OBJECTID) # should be from 1 to n

#Reshape and replace crop codes according to price ! To be updated @prices !

resh.crop <- melt(sampleDE_crop, id = c("FID", "State", "OBJECTID", "cell_id", "x_coord", "y_coord"))
names(resh.crop) <- c("FID", "State", "OBJECTID", "cell_id", "x_coord", "y_coord", "Year", "crtype")
resh.crop$crtype <- replace(resh.crop$crtype, resh.crop$crtype == 8, 4)
resh.crop$crtype <- replace(resh.crop$crtype, resh.crop$crtype == 2, 1)
resh.crop$crtype <- replace(resh.crop$crtype, resh.crop$crtype == 14, 13)

## Climate Data #########################################

#Temperature --------------------------------------------
setwd(paste(mainPath,"/OrigData", sep = ""))

dataTemp <- fread(temp, sep=",", header = T)
dataTemp1 <- data.frame("cell_id"=dataTemp$cell_id)

i <- 5
j <- 4

for (i in 5:20){
   dataTemp1[paste(i,"tAVG",sep="")] <- data.frame(apply(dataTemp[,j:(j+5)]/10, 1, mean)) #March to August
   dataTemp1[paste(i,"tAM",sep="")] <- data.frame(apply(dataTemp[,(j+1):(j+2)]/10, 1, mean)) #April to May
   dataTemp1[paste(i,"tJJ",sep="")] <- data.frame(apply(dataTemp[,(j+3):(j+4)]/10, 1, mean)) #June to July
   j <- j+12
}

dataTemp <- dataTemp1

#Erase workspace, and perform garbage collection
gc()
rm(crspd, dataCrop, dataTemp1)

#Find the corresponding OBJECTID and cell_id from ref

crspd <- Reduce(function(...) merge(..., all = TRUE, by = c("cell_id")), list(dataTemp, sampleDE_ref))
idx <- which(crspd$OBJECTID > 0)

sampleDE_temp <- crspd[idx,1:50]
sampleDE_temp <- sort(sampleDE_temp, by="cell_id")
sampleDE_ref <- sampleDE_ref[order(sampleDE_ref$cell_id,)]

test_temp <- which(sampleDE_temp$cell_id==sampleDE_ref$cell_id) # should be from 1 to n

#Reshape

resh.temp <- melt(sampleDE_temp[,c(1,2,5,8,11,14,17,20,23,26,29,32,35,38,41,44,47,50)], id = c("cell_id", "OBJECTID"))
resh.temp[,5:8] <- melt(sampleDE_temp[,c(1,3,6,9,12,15,18,21,24,27,30,33,36,39,42,45,48,50)], id = c("cell_id", "OBJECTID"))
resh.temp[,9:12] <- melt(sampleDE_temp[,c(1,4,7,10,13,16,19,22,25,28,31,34,37,40,43,46,49,50)], id = c("cell_id", "OBJECTID"))

resh.temp <- resh.temp[,-c(5:7,9:11)]

names(resh.temp) <- c("cell_id", "OBJECTID", "Year", "TempAVG", "TempAM", "TempJJ")

#Erase workspace, and perform garbage collection
gc()
rm(dataTemp, crspd)

#Precipitation ------------------------------------------
setwd(paste(mainPath,"/OrigData", sep = ""))
dataPrec <- fread(prec, sep=",", header = T)

dataPrec1 <- data.frame("cell_id"=dataPrec$cell_id)

i <- 5
j <- 4

for (i in 5:20){
   dataPrec1[paste(i,"pAVG",sep="")] <- data.frame(apply(dataPrec[,j:(j+5)]/10, 1, sum)) #March to August
   dataPrec1[paste(i,"pAM",sep="")] <- data.frame(apply(dataPrec[,(j+1):(j+2)]/10, 1, sum)) #April to May
   dataPrec1[paste(i,"pJJ",sep="")] <- data.frame(apply(dataPrec[,(j+3):(j+4)]/10, 1, sum)) #June to July
   j <- j+12
}

dataPrec <- dataPrec1

#Find the corresponding OBJECTID and cell_id from ref

crspd <- Reduce(function(...) merge(..., all = TRUE, by = c("cell_id")), list(dataPrec, sampleDE_ref))
idx <- which(crspd$OBJECTID > 0)

sampleDE_prec <- crspd[idx,1:50]
sampleDE_prec <- sort(sampleDE_prec, by="cell_id")
sampleDE_ref <- sampleDE_ref[order(sampleDE_ref$cell_id,)]

test_prec <- which(sampleDE_prec$cell_id==sampleDE_ref$cell_id) # should be from 1 to n

#Reshape

resh.prec <- melt(sampleDE_prec[,c(1,2,5,8,11,14,17,20,23,26,29,32,35,38,41,44,47,50)], id = c("cell_id", "OBJECTID"))
resh.prec[,5:8] <- melt(sampleDE_prec[,c(1,3,6,9,12,15,18,21,24,27,30,33,36,39,42,45,48,50)], id = c("cell_id", "OBJECTID"))
resh.prec[,9:12] <- melt(sampleDE_prec[,c(1,4,7,10,13,16,19,22,25,28,31,34,37,40,43,46,49,50)], id = c("cell_id", "OBJECTID"))

resh.prec <- resh.prec[,-c(5:7,9:11)]

names(resh.prec) <- c("cell_id", "OBJECTID", "Year", "PrecAVG", "PrecAM", "PrecJJ")

#Erase workspace, and perform garbage collection
gc()
rm(dataPrec, dataPrec1, crspd)

#Radiation ----------------------------------------------
setwd(paste(mainPath,"/OrigData", sep = ""))
dataRad <- fread(rad, sep=",", header = T)

dataRad1 <- data.frame("cell_id"=dataRad$cell_id)

i <- 5
j <- 4

for (i in 5:20){
   dataRad1[paste(i,"rAVG",sep="")] <- data.frame(apply(dataRad[,j:(j+5)]/10, 1, mean)) #March to August
   dataRad1[paste(i,"rAM",sep="")] <- data.frame(apply(dataRad[,(j+1):(j+2)]/10, 1, mean)) #April to May
   dataRad1[paste(i,"rJJ",sep="")] <- data.frame(apply(dataRad[,(j+3):(j+4)]/10, 1, mean)) #June to July
   j <- j+12
}

dataRad <- dataRad1

#Find the corresponding OBJECTID and cell_id from ref

crspd <- Reduce(function(...) merge(..., all = TRUE, by = c("cell_id")), list(dataRad, sampleDE_ref))
idx <- which(crspd$OBJECTID > 0)

sampleDE_rad <- crspd[idx,1:50]
sampleDE_rad <- sort(sampleDE_rad, by="cell_id")
sampleDE_ref <- sampleDE_ref[order(sampleDE_ref$cell_id,)]

test_rad <- which(sampleDE_rad$cell_id==sampleDE_ref$cell_id) # should be from 1 to n

#Reshape

resh.rad <- melt(sampleDE_rad[,c(1,2,5,8,11,14,17,20,23,26,29,32,35,38,41,44,47,50)], id = c("cell_id", "OBJECTID"))
resh.rad[,5:8] <- melt(sampleDE_rad[,c(1,3,6,9,12,15,18,21,24,27,30,33,36,39,42,45,48,50)], id = c("cell_id", "OBJECTID"))
resh.rad[,9:12] <- melt(sampleDE_rad[,c(1,4,7,10,13,16,19,22,25,28,31,34,37,40,43,46,49,50)], id = c("cell_id", "OBJECTID"))

resh.rad <- resh.rad[,-c(5:7,9:11)]

names(resh.rad) <- c("cell_id", "OBJECTID", "Year", "RadAVG", "RadAM", "RadJJ")

#Erase workspace, and perform garbage collection
gc()
rm(crspd, dataRad, dataRad1)

## Soil data ##############################################

#Read 

setwd(paste(mainPath,"/OrigData", sep = ""))
dataSoil <- fread(soil, sep=",", header = T)
#which(is.na(dataSoil))
#sapply(dataSoil,class)
#dataSoil <- dataSoil[order(dataSoil$x_coord),]
dataSoil$x_coord <- as.character(round(dataSoil$x_coord))
dataSoil$y_coord <- as.character(round(dataSoil$y_coord))

#dataRef_DE <- dataRef_DE[order(dataRef_DE$x_coord),]

#Soil Type -----------------------------------------------
#Find the corresponding x and y coordinates from crop
crspd <- Reduce(function(...) merge(..., all = TRUE, by = c("x_coord", "y_coord")), list(resh.crop,dataSoil))
crspd <- crspd[,-c(6:8,10)]

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

resh.stype <- sort(resh.stype, by="OBJECTID.x")
resh.crop <- sort(resh.crop, by="OBJECTID")

test_stype <- which(resh.stype$OBJECTID.x==resh.crop$OBJECTID) # should be from 1 to n x 16

#Erase workspace, and perform garbage collection
gc()
rm(crspd, dataRef, dataSoil, sampleDE_crop, sampleDE_prec, sampleDE_rad, sampleDE_ref, sampleDE_temp)

################### Create the data formatting used by the crop rotation  ###################

## Order them so that the values match ! ##
resh.stype <- sort(resh.stype, by="OBJECTID.x")
resh.crop <- sort(resh.crop, by="OBJECTID")
resh.prec <- sort(resh.prec, by="OBJECTID")
resh.rad <- sort(resh.rad, by="OBJECTID")
resh.temp <- sort(resh.temp, by="OBJECTID")

identical(resh.crop$OBJECTID, resh.stype$OBJECTID.x)
identical(resh.crop$OBJECTID, resh.prec$OBJECTID)
identical(resh.crop$OBJECTID, resh.temp$OBJECTID)
identical(resh.crop$OBJECTID, resh.rad$OBJECTID)

## Price ------------------------------------------------- Needs to be updated!
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

end_time <- Sys.time()
preProc <- end_time-start_time
preProc

#write.csv(data, "D:/Projects_IACS/Database_DE/CropRotPred_DE/Output_210912/dataDE_5000_5.csv", row.names = FALSE)












