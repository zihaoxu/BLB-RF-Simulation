library(randomForest)
library(MASS)
library(MixSim)
library(doParallel)
registerDoParallel(cores = 20)
print(paste("detectCores() = ", detectCores()))
source('/home/zxu/func.R')


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

createSplits <- function(df, gamma, s){
    n <- nrow(df)
    rownames(df) <- 1:n
    allIndex <- 1:n 
    subsamples <- list()
    for(i in 1:s){
      r.sample.index <- sample(allIndex, n^gamma, replace = FALSE)
      subsamples[[i]] <- df[r.sample.index,]
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
      subsampleResults <- matrix(nrow = nrow(data.test))
      subsampleResults <- foreach(ww=1:length(subsamples), .combine = cbind)%dopar%{
        rf = randomForest(y~., subsamples[[ww]], ntree = ntree, sampling_factor = sampling_factor)
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



# different Ns, ks (pct), and ss

Ns <- c(10000)
k <- 5
df <- data.frame(n = numeric(0), gamma = numeric(0),s = numeric(0), ntree = numeric(0), time = numeric(0),  MSE = numeric(0))
#ss <- c(1,3,6,9,12) #seq(2,22,4)
ntrees <- c(500) # c(50,100,200)
gas <- c(.5,.6, .7,.8, .9) #seq(0.5,.9,.1)

#mean((yhat.rf - data.test)^2)
for(N in Ns){
    for(ga in gas){
      if(ga == .5){
        ss <- seq(1,18,3)
      }else if(ga == .6){
        ss <- seq(1,18,3)
      }else if(ga == .7){
        ss <- seq(1,18,3)
      }else if(ga == .8){
        ss <- seq(1,15,3)
      }else{
        ss <- c(1,3,5)
      }
      for(ntree in ntrees){
        for(s in ss){
          train.test.list <- makeTestTrainList(N, k, ndim = 5, linear = FALSE, clustered  = TRUE)
          result.list <- calculateMSE(train.test.list, N = N, gamma = ga, s = s, ntree = ntree)
          mse <- result.list[[1]]
          time.taken <- result.list[[2]]
          print(paste("N =", N, 'gamma = ', ga,'s =',s, 'ntree =',ntree, "time =", time.taken, 'MSE =', mse))
          df<-rbind(df, data.frame(n = N, gamma = ga, s = s, ntree = ntree, time = time.taken, MSE = mse))
        }
      }
    }
}
write.csv(df, 'MSE~s_clustered.csv')




