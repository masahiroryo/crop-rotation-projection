library(data.table)
library(dtplyr)
library(tidyverse)
library(matrixStats)
library(progress)

simulate_data <- function(file) {
  read_name <- paste("./data/orig/",file,".csv",sep="")
  dat <- fread(read_name, sep=",", header = T)
  print(read_name)
  
  sim_dat <- dat[,1]
  months <- c("01","02","03","04","05","06","07","08","09","10","11","12")
  
  
  df <- data.frame(matrix(NA, nrow = nrow(dat), ncol = 0))
  names <- vector(mode="character", length=24)
  k=1
  
  for(month in months) {
    temp <- dat %>%
      select(ends_with(month)) %>%
      as.data.table()
  
    ne <- data.frame(mean=rowMeans(temp), std=rowSds(as.matrix(temp), na.rm=TRUE))
    df <- cbind(df, ne)
    
    names[k] <- paste("mean",month,sep="")
    names[k+1] <- paste("std",month,sep="")
    k=k+2
  }
  
  names(df) <- names
  rm(dat)
  gc()
  
  pb <- progress_bar$new(total = 79)
  stepi = 1
  for(i in 21:99) {
    pb$tick()
    for(month in months) {
      temp <- df %>%
        select(ends_with(month)) %>%
        as.data.frame()
      m <- mean(temp[,1])
      sd <- mean(temp[,2])
      
      t <- rnorm(nrow(sim_dat), m, sd)

      col <- paste(file,"_20",i,month, sep="")
      sim_dat[,col] <- round(t,0)
    }
    stepi = stepi + 1
  }
  
  save_name <- paste("./data/simulated/",file,".csv",sep="")
  print(save_name)
  write.csv(sim_dat, save_name, row.names = FALSE)
}

simulate_data("temp")
simulate_data("prec")
simulate_data("rad")

