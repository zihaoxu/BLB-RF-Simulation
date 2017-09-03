library(randomForest)
library(MASS)
source('/home/zxu/calculateMSE.R')
source('/home/zxu/func.R')



Ns <- c(10000)
k <- 5
df <- data.frame(n = numeric(0), gamma = numeric(0),s = numeric(0), ntree = numeric(0), time = numeric(0),  MSE = numeric(0))
ss <- c(5) #seq(2,22,4)
ntrees <- seq(25,500,25) # c(50,100,200)
gas <- seq(0.5,.9,.1)

#mean((yhat.rf - data.test)^2)
for(N in Ns){
  for(ga in gas){
    for(ntree in ntrees){
      for(s in ss){
        train.test.list <- makeTestTrainList(N, k, ndim = 5, linear = TRUE)
        result.list <- calculateMSE(train.test.list, N = N, gamma = ga, s = s, ntree = ntree)
        mse <- result.list[[1]]
        time.taken <- result.list[[2]]
        print(paste("N =", N, 'gamma = ', ga,'s =',s, 'ntree =',ntree, "time =", time.taken, 'MSE =', mse))
        df<-rbind(df, data.frame(n = N, gamma = ga, s = s, ntree = ntree, time = time.taken, MSE = mse))
      }
    }
  }
}
write.csv(df, 'MSE~ntree_linear.csv')




