library(randomForest)
library(MASS)
source('/home/zxu/calculateMSE.R')
source('/home/zxu/func.R')



Ns <- c(10000)
k <- 5
df <- data.frame(n = numeric(0), gamma = numeric(0),s = numeric(0), ntree = numeric(0), time = numeric(0),  MSE = numeric(0))
#ss <- c(1,3,6,9,12) #seq(2,22,4)
ntrees <- c(500) # c(50,100,200)
gas <- c(.5,.6, .7,.8, .9,1.) #seq(0.5,.9,.1)

#mean((yhat.rf - data.test)^2)
for(N in Ns){
  for(ga in gas){
    if(ga == .5){
      ss <- seq(1,18,2)
    }else if(ga == .6){
      ss <- seq(1,18,2)
    }else if(ga == .7){
      ss <- seq(1,18,2)
    }else if(ga == .8){
      ss <- seq(1,15,2)
    }else if(ga == .9){
      ss <- c(1,3,5,7)
    }else{
      ss <- c(1)
    }
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
write.csv(df, 'MSE~s_linear.csv')




