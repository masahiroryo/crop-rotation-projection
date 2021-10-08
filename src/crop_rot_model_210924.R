#install.packages("tidyverse")
#install.packages("ranger")

library(tidyverse)
library(ranger)
library(caret)
library(vip)
library(pdp)
library(partykit)

# Create the crop types for the previous 2 years

data <- sort(data, by="Year")
id05 <- which(data$Year==2005)
id06 <- which(data$Year==2006)

data$PCType <- NA # previous crop type
data$PCType[(n+1):(n*16)] <- data$CType[1:(n*16-n)]
data$PPCType <- NA # previous previous crop type
data$PPCType[(n*2+1):(n*16)] <- data$CType[1:(n*16-n*2)]

data1 <- data # backup 
data <- data[-c(1:(n*2)),]

start_time <- Sys.time()

# Data has to have the following:
names(data) <- c("Year", "X", "Y", "CType", "tAVG", "tAM", "tJJ", "pAVG", "pAM", "pJJ",
                 "rAVG", "rAM", "rJJ", "SType", "SElev", "SSlope")
data[,1:3] <- as.numeric(data[,1:3])

data$X <- as.numeric(data$X)
data$Y <- as.numeric(data$Y)

data$CType <- as.factor(data$CType)
data$PCType <- as.factor(data$PCType)
data$PPCType <- as.factor(data$PPCType)
data$SType <- as.factor(data$SType)
data$SElev <- as.numeric(data$SElev)
#data$price <- as.numeric(data$price)

# Check for and remove NAs
data.NA <- list(which(is.na(data$Year)), which(is.na(data$X)), which(is.na(data$Y)), which(is.na(data$PCType)), 
                which(is.na(data$PPCType)), which(is.na(data$CType)), which(is.na(data$temp)),which(is.na(data$prec)), 
                which(is.na(data$rad)), which(is.na(data$SType)), which(is.na(data$SElev)), which(is.na(data$SSlope)))
idx <- 0
for (i in 1:13){
  if (length(data.NA[[i]] != 0)){
    idx <- c(data.NA[[i]],idx)
  }
}
idx <- unique(idx)

if (idx !=0){
  data <- data[-idx,]
}

## Split Training & Test Data
data.train <- with(data, data[(Year >= 1 & Year <= 11), ])
data.test <- with(data, data[(Year >= 12), ])

## Create Ranger & Predict
data.rg <- ranger(CType ~ ., data = data.train, 
                        importance="impurity",
                        oob.error = T,
                        seed = 1,
                        probability = F
                        )

# train: OOB error rate
data.rg$prediction.error

# train: evaluation

data.rg$confusion.matrix %>% confusionMatrix()
cm1 <- data.rg$confusion.matrix %>% confusionMatrix()

# test: evaluation
data.pred <- predict(data.rg, data = data.test)
table(data.test$CType, data.pred$predictions) %>% confusionMatrix()
cm2 <- table(data.test$CType, data.pred$predictions) %>% confusionMatrix()

# varimp

vip(data.rg)

# pdp: one class, one predictor

data.rg$prob <- ranger(CType ~ ., data = data.train, 
                        importance="impurity",
                        seed = 1,
                        probability = T
)

partial(data.rg$prob, pred.var = c("prec"), which.class = "1") %>% autoplot()

# pdp: all class, one predictor
pd <- NULL
for (i in levels(data$CType)) {
  tmp <- partial(data.rg$prob, pred.var = c("prec"),
                 which.class = i)
  pd <- rbind(pd, cbind(tmp, Class = i))
}

ggplot(pd, aes(x=prec,y=yhat, color=Class, group=Class)) + geom_line()

#another approach: ctree

model.ctree <- partykit::ctree(CType ~ ., data = data.train, 
                               control = ctree_control(maxdepth = 3))
plot(model.ctree, gp=gpar(fontsize=8))

end_time <- Sys.time()
proc <- end_time - start_time

proc

data.rg$prediction.error
cm2[["overall"]][["Accuracy"]]
cm2[["overall"]][["Kappa"]]

