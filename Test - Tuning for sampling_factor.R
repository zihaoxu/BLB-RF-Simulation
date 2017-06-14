# Random forest and bagging

library(randomForest)
library(MASS)

result <- c()
set.seed(47)

for(i in 1:20){
    train = sample(1:nrow(Boston), nrow(Boston)/2)
    boston.train = Boston[train,]
    rf.boston = randomForest(medv~., boston.train, ntree = 500, sampling_factor = 1, importance = TRUE)
    yhat.rf = predict(rf.boston, newdata = Boston[-train,])
    boston.test = Boston[-train, "medv"]
    print(mean((yhat.rf - boston.test)^2))
    result <- c(result, mean((yhat.rf - boston.test)^2))
}


print(mean(result))
importance(rf.boston)


#varImpPlot(rf.boston)

set.seed(47)
lambdas <- c(.5, .6, .7, .8, .9, 1)
treesizes <- c(500, 1000, 2000, 4000)
boston.train = Boston[train,]
table <- c()
for (treesize in treesizes){
  for (lambda in lambdas){
    #print(lambda)
    N = floor(nrow(boston.train)/(nrow(boston.train)^lambda))
    boston.list = split(boston.train, sample(1:N, nrow(boston.train), replace=T))
    result = c()
    for (i in 1:N){
      subsample <- boston.list[[i]]
      if (N!=1){
        subsample <- subsample[sample(1:nrow(subsample), nrow(boston.train), replace = T), ]
      }
      rf.boston = randomForest(medv~., subsample, ntree = treesize, importance = TRUE)
      yhat.rf = predict(rf.boston, newdata = Boston[-train,])
      boston.test = Boston[-train, "medv"]
      result = c(result, mean((yhat.rf- boston.test)^2))
    }
    print(paste("the mean MSE for lambda =", lambda, "and treesize = ",treesize,  "is", mean(result)))
    table = c(table, mean(result))
  }
}

t <- matrix(table, ncol = 6, nrow = length(treesizes), byrow=T, dimnames = list(treesizes, lambdas))
t

colorlist = c('yellow', 'orange', 'red', 'purple')
for (i in 1:length(treesizes)){
  if(i==1){
    plot(lambdas, t[i,], type="l", col = colorlist[i], ylim=c(0,400))
  }else{
    lines(lambdas, t[i,], type="l", col = colorlist[i])
  }
}

library(ggplot2)

firstsim <- data.frame(table, lamb = rep(lambdas,4), trees =rep(treesizes, each=6))

ggplot(firstsim, aes(x=lamb, y=table, col=as.factor(trees))) + geom_line()









