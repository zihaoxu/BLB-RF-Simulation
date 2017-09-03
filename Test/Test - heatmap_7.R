library(randomForest)
library(MASS)
source('/home/zxu/calculateMSE.R')
source('/home/zxu/func.R')



N <- 100
k <- 5
df <- data.frame()
ss <- seq(1,20,1) #seq(2,22,4)
ntrees <- seq(20,500,10) # c(50,100,200)
gas <- c(.5,.6, .7) #seq(0.5,.9,.1)


#mean((yhat.rf - data.test)^2)
for(ga in gas){
  for(ntree in ntrees){
    for(s in ss){
      train.test.list <- makeTestTrainList(N, k, ndim = 5, linear = FALSE, clustered = TRUE)
      result.list <- calculateMSE(train.test.list, N = N, gamma = ga, s = s, ntree = ntree)
      mse <- result.list[[1]]
      time.taken <- result.list[[2]]
      print(paste("N =", N, 'gamma = ', ga,'s =',s, 'ntree =',ntree, "time =", time.taken, 'MSE =', mse))
      df<-rbind(df, data.frame(n = N, gamma = ga, s = s, ntree = ntree, time = time.taken, MSE = mse))
    }
  }
}

write.csv(df, 'heatmap.csv')






