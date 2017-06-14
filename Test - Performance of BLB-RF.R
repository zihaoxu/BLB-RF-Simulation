library(MASS)
library(doParallel)
registerDoParallel(cores = detectCores())

set.seed(47)


tempfunc <- function (df, train_test_ratio = 0.5, gamma = .7, ntrees = 500){
  
  train.index <- sample(1:nrow(df), nrow(df)*train_test_ratio)
  train <- df[train.index,]
  test <- df[-train.index,]
  
  x <- 1:nrow(train) #indices of training set
  n <- length(x) #size of training set
  s <- floor(n / n^gamma) #number of subsamples
  
  subsamps <- foreach(i=1:s, .combine = cbind)%dopar%{
    # (1) Draw a subsample and create vector to store all the resampled responses. 
    subsample <- sample(x, n^gamma, FALSE)
    x <- setdiff(x, subsample)
    
    rf <- randomForest(medv~., data = train[subsample, ], ntree = ntrees, sampling_factor = s)
    
    predict(rf, test)
  }
  
  yhat.rf <- apply(subsamps, 1, mean)
  se <- apply(subsamps, 1, sd) / sqrt(s)
  real_y <- df[-train.index, "medv"]
  
  return(data.frame(yhat = yhat.rf, se, yexp = real_y, diff = real_y-yhat.rf))
  
}

result <- tempfunc(Boston, train_test_ratio = .5, gamma = .7)

# uninstall ramdomForestBLB and install the normal randomForest package


library(randomForest)
library(MASS)
normal_rf <- randomForest(medv~., data = Boston[train.index, ], ntree = 500)
rf_result <- predict(normal_rf, test)
real_y_rf <- Boston[-train.index, "medv"]

result <- cbind(result, rf_result, real_y_rf)

write.csv(result, file = '/Users/zihaoxu/Desktop/result.csv')










library(doParallel)
registerDoParallel(cores = detectCores())
set.seed(47)
data <- makeLinearDF(5000, c(1,5,2,9,2), 10)

result <- tempfunc(data, train_test_ratio = .5, gamma = .7)

# uninstall ramdomForestBLB and install the normal randomForest package


library(randomForest)
library(MASS)
normal_rf <- randomForest(y~.-yexp, data = data[train.index, ], ntree = 500)
train.index <- sample(1:nrow(data), nrow(data)*0.5)
test <- data[-train.index,]
rf_result <- predict(normal_rf, test)
real_y_rf <- data[-train.index, "y"]

result <- cbind(result, rf_result, real_y_rf)

write.csv(result, file = '/Users/zihaoxu/Desktop/result.csv')

