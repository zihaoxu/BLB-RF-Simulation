library(MixSim)
library(doParallel)
registerDoParallel(cores = 20)
print(paste("detectCores() = ", detectCores()))



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




makeTestTrainList_real <- function(df, seed = 47){
  set.seed(seed)
  N <- nrow(df)
  
  test.index <- sample(1:N, N/6)
  data.test <- df[test.index,]
  rownames(data.test) <- 1:nrow(data.test)
  
  data.train <- df[-test.index,]
  rownames(data.train) <- 1:nrow(data.train)
  return(list(data.train, data.test))
}








createSplits <- function(df, gamma, s){
  n <- nrow(df)
  rownames(df) <- 1:n
  subsamples <- matrix(nrow = s, ncol = n^gamma)
  for(i in 1:s){
    r.sample.index <- sample(1:n, n^gamma, replace = FALSE)
    subsamples[i,] <- r.sample.index
  }
  return(subsamples)
}


calculateMSE <- function(test.train.list, N, gamma, s, ntree, rep.time = 5){
  result <- c()
  times <- c()
  sampling_factor = round(N/(N^gamma))
  
  data.train <- test.train.list[[1]]
  data.train <- data.train[,1:(ncol(data.train)-1)]
  data.test <- test.train.list[[2]]
  data.test.y <- data.test[, "y"]
  pred.data <- data.test[,2:(ncol(data.test)-1)]
  
  for(kj in 1:rep.time){
    start.time <- as.numeric(Sys.time())
    subsamples <- createSplits(data.train, gamma = gamma, s=s)
    #start.time <- as.numeric(Sys.time())
    subsampleResults <- matrix(nrow = nrow(data.test))
    subsampleResults <- foreach(ww=1:nrow(subsamples), .combine = cbind)%dopar%{
      rf = randomForest(y~., data.train[subsamples[ww,],], ntree = ntree, sampling_factor = sampling_factor)
      yhat.rf = predict(rf, newdata = pred.data)
    }
    if(s!=1){
      finalPred <- rowMeans(subsampleResults)
    }else{
      finalPred <- subsampleResults
    }
    time.taken <- as.numeric(Sys.time()) - start.time
    times <- c(times, time.taken)
    result <- c(result, mean((finalPred - data.test.y)^2))
  }
  return(list(mean(result), mean(times)))
}





calculateMSE_real <- function(test.train.list, N, gamma, s, ntree, rep.time = 5){
  result <- c()
  times <- c()
  sampling_factor = round(N/(N^gamma))
  
  drops <- c("quality")
  data.train <- test.train.list[[1]]
  #data.train <- data.train[ , !(names(data.train) %in% drops)]
  #data.train <- data.train[,1:(ncol(data.train)-1)]
  data.test <- test.train.list[[2]]
  data.test.y <- data.test[, "quality"]
  pred.data <- data.test[ , !(names(data.test) %in% drops)]
  
  for(kj in 1:rep.time){
    start.time <- as.numeric(Sys.time())
    subsamples <- createSplits(data.train, gamma = gamma, s=s)
    #start.time <- as.numeric(Sys.time())
    subsampleResults <- matrix(nrow = nrow(data.test))
    subsampleResults <- foreach(ww=1:nrow(subsamples), .combine = cbind)%dopar%{
      rf = randomForest(quality~., data.train[subsamples[ww,],], ntree = ntree, sampling_factor = sampling_factor)
      yhat.rf = predict(rf, newdata = pred.data)
    }
    if(s!=1){
      finalPred <- rowMeans(subsampleResults)
    }else{
      finalPred <- subsampleResults
    }
    time.taken <- as.numeric(Sys.time()) - start.time
    times <- c(times, time.taken)
    result <- c(result, mean((finalPred - data.test.y)^2))
  }
  return(list(mean(result), mean(times)))
}
