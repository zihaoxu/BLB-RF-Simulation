library(randomForest)
library(MASS)
source('/home/zxu/calculateMSE.R')


#https://archive.ics.uci.edu/ml/datasets/Wine+Quality

df <- read.csv('/home/zxu/winequality-white.csv', sep = ';')
#df <- df[complete.cases(df),]
#df$larea = log(df$area+1)

train.test.list <- makeTestTrainList_real(df)

Ns <- c(nrow(df))
k <- 5
result_df <- data.frame(n = numeric(0), gamma = numeric(0),s = numeric(0), ntree = numeric(0), time = numeric(0),  MSE = numeric(0))
#ss <- c(1,3,6,9,12) #seq(2,22,4)
ntrees <- c(500) # c(50,100,200)
gas <- seq(0.5,1,.1) #seq(0.5,.9,.1)

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
    }else if(ga==.9){
      ss <- seq(1,7,3)
    }else{
      ss <- c(1)
      ntrees <- c(500,1000)
    }
    for(ntree in ntrees){
      for(s in ss){
        result.list <- calculateMSE_real(train.test.list, N = N, gamma = ga, s = s, ntree = ntree)
        mse <- result.list[[1]]
        time.taken <- result.list[[2]]
        print(paste("N =", N, 'gamma = ', ga,'s =',s, 'ntree =',ntree, "time =", time.taken, 'MSE =', mse))
        result_df<-rbind(result_df, data.frame(n = N, gamma = ga, s = s, ntree = ntree, time = time.taken, MSE = mse))
      }
    }
  }
}
write.csv(result_df, 'MSE~s_wine.csv')




###
#drops <- c("area")
#data.train <- train.test.list[[1]]
#data.train <- data.train[ , !(names(data.train) %in% drops)]
#####data.train <- data.train[,1:(ncol(data.train)-1)]
#data.test <- train.test.list[[2]]
#data.test.y <- data.test[, "larea"]
#pred.data <- data.test[ , !(names(data.test) %in% drops)]

#start.time <- as.numeric(Sys.time())
#rrff <- randomForest(larea~., data = data.train, ntree=500)
#re <- predict(rrff, pred.data)
#print(paste("RF time:",as.numeric(Sys.time()) - start.time))
#print(paste("RF MSE:",mean((re-data.test.y)^2)))
###