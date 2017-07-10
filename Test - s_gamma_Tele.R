#https://archive.ics.uci.edu/ml/datasets/Parkinsons+Telemonitoring

df <- read.csv("/Users/zihaoxu/Desktop/parkinsons_updrs.csv", sep=";")
drops <- c('subject.', 'age', 'sex', 'test_time','motor_UPDRS')



df <- df[ , !(names(df) %in% drops)]


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
write.csv(result_df, 'MSE~s_teleP.csv')