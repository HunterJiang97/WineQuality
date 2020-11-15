rm(list = ls())
setwd(dirname(rstudioapi::getActiveDocumentContext()$path))
wine <- read.csv(file = "data/winequality-red.csv", head = TRUE, sep = ";")
par(mfrow = c(3,4))
name = colnames(wine)
for (ii in 1:12){
  hist(wine[,ii], breaks = 30, main = name[ii], xlab = "")  
}


# Set seed
set.seed(563)

# Slice the data, 80% to the training and 20% to the test
train <- sample(1:nrow(wine), size = nrow(wine) * 0.8)
test <- dplyr::setdiff(1:nrow(wine), train)
wine[,1:11] <- scale(wine[,1:11])
for (ii in 1:12){
  hist(wine[,ii], breaks = 30, main = name[ii], xlab = "")  
}
TrainDat <- wine[train,]
TestDat <- wine[test, ]

# 10fold-CV
CVtrain <- list()
CVtest <- list()
pool <- sample(1:10, size = nrow(TrainDat), replace = TRUE)
for (ii in 1:10){
  CVtrain[[ii]] <- TrainDat[pool != ii,]
  CVtest[[ii]]  <- TrainDat[pool == ii,]
}

Ttrain <- wine[train,]
Ttest <- wine[test,]
save(Ttrain, Ttest, CVtrain, CVtest, file = "CVdataset.RData")
load("CVdataset.RData")

