library(randomForest)
library(MASS)
library(MixSim)
source('/home/zxu/func.R')

#library(devtools)
#install_github("zihaoxu/randomForestBLB")


makeTestTrainList <- function(N, k, ndim, linear = TRUE, clustered = FALSE, cosine = FALSE, seed = 47){
  set.seed(seed)
  
  if(linear){
    data.all <- makeLinearDF(N + N/k, seq(5,5*ndim,5), 2)
  }else if(clustered){
    num.cluster = 20
    data.all <- makeClusteredDF(num.dim = ndim, sample.size = N+N/k, num.clusters = num.cluster, cluster.means = seq(10,10*num.cluster,10), error.SD = 1, seed = 47)
  }else if(cosine){
    data.all <- makeCosineDF(N + N/k, numberX = ndim)
  }
  
  test.index <- sample(1:(N + N/k), (N/k))
  data.test <- data.all[test.index,]
  rownames(data.test) <- 1:nrow(data.test)
  
  data.train <- data.all[-test.index,]
  rownames(data.train) <- 1:nrow(data.train)
  return(list(data.train, data.test))
}


calculateMSE <- function(test.train.list, N, ntree = 500, rep.time = 5){

  times <- c()
  
  data.train <- test.train.list[[1]]
  data.train <- data.train[,1:(ncol(data.train)-1)]
  data.test <- test.train.list[[2]]
  data.test.y <- data.test[, "y"]
  pred.data <- data.test[,2:(ncol(data.test)-1)]
  
  for(kj in 1:rep.time){
    start.time <- as.numeric(Sys.time()) 
    rf <- randomForest(y~., data.train, ntree = ntree)
    finalPred <- predict(rf, newdata = pred.data)
    time.taken <- as.numeric(Sys.time()) - start.time
    times <- c(times, time.taken)
    result <- mean((finalPred - data.test.y)^2)
  }
  return(list(result, mean(times)))
}

N <- 10000
k <- 5

train.test.list <- makeTestTrainList(N, k, ndim = 5, linear = FALSE, clustered  = TRUE)
result.list <- calculateMSE(train.test.list, N = N)
mse <- result.list[[1]]
time.taken <- result.list[[2]]
print(paste("N =", N, 'gamma = ', 1,'s =',1, 'ntree =',500, "time =", time.taken, 'MSE =', mse))



