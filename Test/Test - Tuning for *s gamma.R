library(randomForest)
library(MASS)
library(caret)
library(doParallel)
registerDoParallel(cores = detectCores())



createSplits <- function(df, nfolds = 10){
  folds <- createFolds(df[,1], k = nfolds)
  subsamples <- lapply(folds, function(ind, dat) dat[ind,], dat = df)
  return(subsamples)
}


calculateMSE <- function(N, k, s, linear = TRUE, clustered = FALSE, cosine = FALSE, rep.time = 10, seed = 47){
  result <- c()
  set.seed(seed)
  
  for(q in 1:rep.time){
    if(linear){
      myDF <- makeLinearDF(N, c(2, 4, 6), 1)
    }else if(clustered){
      myDF <- makeClusteredDF(N, c(5,10,15,20), 1)
    }else if(cosine){
      myDF <- makeCosineDF(N)
    }
    testIndex <- seq(0,nrow(myDF)/k,1)
    data.train <- myDF[-testIndex,]
    data.test = myDF[testIndex, "y"]
    subsamples <- createSplits(data.train, s)
    subsampleResults <- matrix(nrow = length(testIndex))
    subsampleResults <- foreach(ww=1:length(subsamples), .combine = cbind)%dopar%{
          rf.boston = randomForest(y~.-yexp, subsamples[[ww]], ntree = 500, sampling_factor = s)
          yhat.rf = predict(rf.boston, newdata = myDF[testIndex,])
    }
    if(s!=1){
        finalPred <- rowMeans(subsampleResults)
    }else{
        finalPred <- subsampleResults
    }
    result <- c(result, mean((finalPred - data.test)^2))
    #print(result)
  }
  return(mean(result))
}



# different Ns, ks (pct), and ss

Ns <- c(25,50,100,200,400)
ks <- c(5)
df <- data.frame(n = numeric(0), s = numeric(0), pct = numeric(0), MSE = numeric(0))
ss <- c(2,4,6,8,10)

#mean((yhat.rf - data.test)^2)
for(N in Ns){
    for(k in ks){
      for(s in ss){
        mse <- calculateMSE(N = N, k = k, s = s)
        print(paste("N =", N, "pct =", 1-1/k, 's =',s, 'MSE =', mse))
        df<-rbind(df, data.frame(n = N, s = s, pct = 1-1/k, MSE = mse))
      }
    }
}
#write.csv(df, './CSV/Linear_n_s_pct_MSE.csv')







library(ggplot2)

ggplot(df, aes(x=gamma, y=MSE, col=as.factor(pct))) + geom_line()





