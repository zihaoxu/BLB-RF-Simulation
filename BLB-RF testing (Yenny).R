library(randomForest)
library(dplyr)
library(ggplot2)
library(devtools) 
#install_github("swager/randomForestCI")
library(randomForestCI)
library(MASS)


#increase test data to 1000


##############################
#### FUNCTION TO #############
#### MAKE DATA FRAME #########
##############################
##############################

# input: number of samples, vector of beta coefficients. 
# output: a dataframe. 

makeDF <- function(numobs, b, constant){
  m <- length(b)
  x <- matrix(runif(m*numobs), nrow = numobs, ncol = m)
  y  = constant + x%*%b + rnorm(numobs)
  yexp = constant + x%*%b 
  df = data.frame(y, x, yexp)
  return(df)
}

##############################
#### FUNCTION TO #############
#### RUN TEST OBSERVATION THRU 
#### BLB RF ##################
##############################

# input: a training data frame, and test observation(s). 
# output: a vector of the predicted response, and the infinitismal jackknife variance

blbrf <- function(train, test, r, gamma, trees) {
  
  # (0) Set up parameters.
  x <- 1:nrow(train) #indices of training set
  n <- length(x) #size of training set
  s <- floor(n / n^gamma) #number of subsamples
  
  subsamps <- matrix(, nrow = nrow(test), ncol = 0)
  
  
  for(i in 1:s) {
    # (1) Draw a subsample and create vector to store all the resampled responses. 
    subsample <- sample(x, n^gamma, FALSE)
    x <- setdiff(x, subsample)
    resamps <- matrix(, nrow = nrow(test), ncol = 0)

    for(j in 1:r) {
      # (2) Resample from the subsample. (r = 100 for all runs)
      b <- length(subsample)
      probs <- rep(1/b, b)
      coeffs <- rmultinom(1, n, probs) #length is size of subsample
      # (3) Make a dataframe of the resample.
      resample <- train[0,]
      for(k in 1:b) {
        resample <- rbind(resample, sapply(train[subsample[k], ], rep, coeffs[k]))
      }
      
      # (4) Run a random forest. 
      rf <- randomForest(y~.-yexp, data = resample, ntree = trees)
      #pred <- predict(rf, test)
      
      # (5) Store response. 
      resamps <- cbind(resamps, predict(rf, test))
    }
    subsamps <- cbind(subsamps, apply(resamps, 1, mean))
  }
  
  y <- apply(subsamps, 1, mean)
  se <- apply(subsamps, 1, sd)
  yexp <- test[,length(test)]
  return(matrix(c(y,se, yexp), nrow = nrow(test), ncol = 3))
}


##############################
#### FUNCTION TO #############
#### MAKE PLOT################
##############################
##############################

# input: a training data frame, and a test data frame, and output from blbrf. 
# output: a nice.. plot

makeplot <- function(train, test, output, cluster = FALSE) {
  
  rf <- randomForest(y~.-yexp, data = train, ntree = 800, keep.inbag = TRUE, replace = TRUE)
  ij <- randomForestInfJack(rf, test, calibrate = TRUE)
  
  
  yexp <- output[,3]
  
  yhat <- c(output[,1], ij[,1])
  se <- c(output[,2], ij[,2])
  
  #duplicated row names?
  #df <- data.frame(c(y,y), yhat, se, blb = c(rep(1,nrow(test)), rep(0, nrow(test))))
  df <- data.frame(yexp, yhat, se, indicator = c(rep(1,nrow(test)), rep(0, nrow(test))))
  
  sampdf <- df[sample(1:nrow(df), nrow(df)/5),]
  
  if(!cluster){
      ggplot(sampdf, aes(x = yexp, y = yhat)) + 
      geom_errorbar(aes(ymin = yhat - 2*se, ymax = yhat + 2*se, colour = factor(indicator)), width = 0.05, size = .75) +
      geom_point(aes(colour = factor(indicator)), size = 1.0) + 
      geom_abline(intercept = 0, slope = 1, linetype = 2) +
      xlab("Truth: y = Bx") + 
      ylab("Prediction") +
      ggtitle("Random Forest vs Random Forest-BLB")  + scale_colour_discrete(name  ="Legend", labels=c("Random Forest", "BLB"))
  }else{
      ggplot(sampdf, aes(x = yexp, y = yhat)) + 
      geom_jitter(aes(colour = factor(indicator)), size = 1.0, position = position_jitter(width = 2, height = 2)) + 
      geom_errorbar(aes(ymin = yhat - 2*se, ymax = yhat + 2*se, colour = factor(indicator)), width = 0.05, size = .75) +
      geom_abline(intercept = 0, slope = 1, linetype = 2) +
      xlab("Truth: y = Bx") + 
      ylab("Prediction") +
      ggtitle("Random Forest vs Random Forest-BLB")  + scale_colour_discrete(name  ="Legend", labels=c("Random Forest", "BLB"))
    
  }
  
}