# Init
rm(list = ls())
setwd(dirname(rstudioapi::getActiveDocumentContext()$path))
load("CVdataset.RData")
library(MASS)
library(glm.predict)
library(class)
library(tree)
library(randomForest)
library(adabag)

# model functions
model.LDA <- function(train, test){
  # Linear Discriminate Analysis
  model_LDA <- lda(quality ~., data = train)
  yhat_LDA <- predict(model_LDA, test)
  return(yhat_LDA$class)
}

model.QDA <- function(train, test){
  # Quadratic Discriminate Analysis
  model_QDA <- qda(quality ~., data = train)
  yhat_QDA <- predict(model_QDA, test)
  return(yhat_QDA$class)
}

model.Logit <- function(train, test){
  train$quality <- train$quality - 1
  # Logistic Regression
  model_Logit <- glm(quality ~ ., family = binomial(), data = train)
  p_Logit <- predict(model_Logit, newdata = test, type = "response")
  yhat_Logit <- rep(0, length(p_Logit))
  yhat_Logit[p_Logit > 0.5] <- 1
  return(yhat_Logit + 1)
}

model.knn <- function(train, test, K){
  # K nearest neighbor
  train.m <- as.matrix(train[,1:11])
  test.m <- as.matrix(test[,1:11])
  train.q <- train$quality
  yhat_knn <- knn(train.m, test.m, train.q, k = K)
  return(yhat_knn)
}

model.opttree <- function(train, test){
  # Best Tree model
  model_tree <- tree(as.factor(quality) ~ ., data = train)
  cv_tree <- cv.tree(model_tree, FUN = prune.misclass)
  model_opt <- prune.tree(model_tree, best = cv_tree$size[which.min(cv_tree$dev)])
  yhat <- predict(model_opt, test, type = "class")
  return(yhat)
}

model.rf <- function(train, test, k){
  # Random Forest
  train$quality <- as.factor(train$quality)
  model_rf <- randomForest(quality~.,train, mtry=k, ntree = 100, importance=TRUE)
  yhat <- predict(model_rf, test)
  return(yhat)
}

model.bag <- function(train, test){
  # Bagging
  train$quality <- as.factor(train$quality)
  model_bag <- randomForest(quality~.,train, mtry=10, ntree = 100, importance=TRUE)
  yhat <- predict(model_bag, test)
  return(yhat)
}

# cv function to use
CV_fun <- function(trainset, testset){
  # Train methods
  ptm1 <- proc.time()
  yhat_LDA <- model.LDA(trainset, testset)
  ptm2 <- proc.time()
  yhat_QDA <- model.QDA(trainset, testset)
  ptm3 <- proc.time()
  yhat_Log <- model.Logit(trainset, testset)
  ptm4 <- proc.time()
  yhat_knn1 <- model.knn(trainset, testset, 1)
  ptm5 <- proc.time()
  yhat_knn3 <- model.knn(trainset, testset, 3)
  ptm6 <- proc.time()
  yhat_knn5 <- model.knn(trainset, testset, 5)
  ptm7 <- proc.time()
  yhat_knn7 <- model.knn(trainset, testset, 7)
  ptm8 <- proc.time()
  yhat_tree <- model.opttree(trainset, testset)
  ptm9 <- proc.time()
  yhat_rf <- model.rf(trainset, testset, 5)
  ptm10 <- proc.time()
  yhat_bag <- model.bag(trainset, testset)
  ptm11 <- proc.time()
  
  # Find the time for each method
  time <- rep(0, 10)
  time[1] <- (ptm2 - ptm1)[3]
  time[2] <- (ptm3 - ptm2)[3]
  time[3] <- (ptm4 - ptm3)[3]
  time[4] <- (ptm5 - ptm4)[3]
  time[5] <- (ptm6 - ptm5)[3]
  time[6] <- (ptm7 - ptm6)[3]
  time[7] <- (ptm8 - ptm7)[3]
  time[8] <- (ptm9 - ptm8)[3]
  time[9] <- (ptm10 - ptm9)[3]
  time[10] <- (ptm11 - ptm10)[3]
  
  # Form a list
  Res <- list(yhat_LDA, yhat_QDA, yhat_Log, yhat_knn1, yhat_knn3, 
                   yhat_knn5, yhat_knn7, yhat_tree, yhat_rf, yhat_bag, time)
  return(Res)
}

# stats to use
Stat_OR <- function(YList, YhatList){
  n <- length(YhatList)
  nn <- length(YhatList[[1]]) - 1
  time <- rep(0, nn)
  Stat <- array(NA, dim = c(n, nn))
  tbl <- matrix(0, nrow = 2, ncol = 2)
  for (ii in 1:n){
    for (jj in 1:nn){
      Y <- YList[[ii]]$quality
      Yhat <- as.numeric(YhatList[[ii]][[jj]])
      Stat[ii,jj] <- length(which(Y == Yhat)) / length(Y)
      time[jj] <- time[jj] + YhatList[[ii]][[11]][jj]
      tbl <- tbl + as.matrix(table(Y, Yhat))
    }
  }
  return(list(Stat, time, tbl))
}

# 10-Fold CV for two class
K = 10
YhatsLList <- list()
CVtrain2 <- CVtrain
CVtest2 <- CVtest
for (ii in 1:K){
  CVtrain2[[ii]]$quality[CVtrain2[[ii]]$quality <=5] <- 1 
  CVtrain2[[ii]]$quality[CVtrain2[[ii]]$quality >=6] <- 2
  CVtest2[[ii]]$quality[CVtest2[[ii]]$quality <=5] <- 1 
  CVtest2[[ii]]$quality[CVtest2[[ii]]$quality >=6] <- 2
  train <- CVtrain2[[ii]]
  test <- CVtest2[[ii]]
  YhatsLList[[ii]] <- CV_fun(train, test)
}
OR_table <- Stat_OR(CVtest2, YhatsLList)
m <- colMeans(OR_table[[1]])
s <- apply(OR_table[[1]], 2, sd)
Result <- rbind(m, s, OR_table[[2]])
colnames(Result) <- c("LDA", "QDA", "Logistic", "KNN1", "KNN3", "KNN5", "KNN7" , "Optimal Tree", "Random Forest", "Bagging")
rownames(Result) <- c("avg", "sd", "time")
Result2 <- Result
Result2
OR_table[[3]]
OR_table[[3]] / sum(OR_table[[3]])