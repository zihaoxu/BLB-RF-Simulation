#library(randomForest)
library(MASS)


ptime1 <- system.time(
  for(i in 1:25){
    train <- sample(1:nrow(Boston), nrow(Boston)/2)
    boston.train <- Boston[train,]
    rf.boston <- randomForest(medv~., boston.train, ntree = 500,importance = TRUE)
  })
print(ptime1)


library(doParallel)
library(MASS)
registerDoParallel(cores = detectCores())
#cl <- makeCluster(detectCores())
#registerDoParallel(cl)
ptime2 <- system.time(
  foreach(i=1:25)%dopar%{
    train <- sample(1:nrow(Boston), nrow(Boston)/2)
    boston.train <- Boston[train,]
    rf.boston <- randomForest(medv~., boston.train, ntree = 500,importance = TRUE)
  })
print(ptime2)

#stopCluster(cl)


library(ranger)
library(MASS)
ptime3 <- system.time(
  foreach(i=1:25)%dopar%{
    train <- sample(1:nrow(Boston), nrow(Boston)/2)
    boston.train <- Boston[train,]
    rf.boston <- ranger(medv~., boston.train)
  })
print(ptime3)
