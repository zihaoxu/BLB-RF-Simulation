library(randomForest)
library(MASS)
library(MixSim)
library(doParallel)
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

createSplits <- function(df, gamma, s){
  rownames(df) <- 1:nrow(df)
  n <- nrow(df)
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
    start.time <- Sys.time()
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
    time.taken <- Sys.time() - start.time
    times <- c(times, time.taken)
    result <- c(result, mean((finalPred - data.test.y)^2))
    #print(result)
  }
  return(list(mean(result), mean(times)))
}



# different Ns, ks (pct), and ss


df <- data.frame(ncore = numeric(0), time = numeric(0),  MSE = numeric(0))
cores <- c(5,10,15,20,25)

#mean((yhat.rf - data.test)^2)
for(core in cores){
        registerDoParallel(cores = core)
        train.test.list <- makeTestTrainList(N = 1000, k=5, ndim = 5, linear = FALSE, clustered  = TRUE)
        result.list <- calculateMSE(train.test.list, N = 500, gamma = .8, s = 15, ntree = 500)
        mse <- result.list[[1]]
        time.taken <- result.list[[2]]
        print(paste("ncore = ", core , "time =", time.taken, 'MSE =', mse))
        df<-rbind(df, data.frame(ncore = core, time = time.taken, MSE = mse))
}

write.csv(df, 'ncore.csv')
#write.csv(df, '/Users/zihaoxu/R_repos/BLB-RF-Sim/CSV/Clustered_n_s_pct_MSE.csv')




