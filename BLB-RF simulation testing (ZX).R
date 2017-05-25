library(randomForest)
library(dplyr)
library(ggplot2)
library(devtools) 
#install_github("swager/randomForestCI")
library(randomForestCI)
library(MASS)


##############################
#### FUNCTION TO #############
#### Generate linear data ####
##############################
##############################

# input: number of observations, vector of beta coefficients and the constant. 
# output: a dataframe. 

makeLinearDF <- function(numobs, b, constant){
  set.seed(47)
  values <- c()
  m = length(b)
  for(i in 1:length(b)){
    values = c(values, rnorm(numobs, mean = runif(1, -100,100), sd = runif(1,0,2)))
  }
  x <- matrix(values, nrow = numobs, ncol = m)
  y  = constant + x%*%b + rnorm(numobs)
  yexp = constant + x%*%b 
  df = data.frame(y, x, yexp)
  return(df)
}

##############################
#### FUNCTION TO #############
#### Generate clustered data #
##############################
##############################

# input: number of observations, vector of centers for xs and range of x vaulues. 
# output: a dataframe of clustered data. 

makeClusteredDF <- function(numobs, centers, diff){
  set.seed(47)
  values <- c()
  yexp <- c()
  m = length(centers)
  for(i in 1:m){
    values <- c(values, runif(numobs, (centers[i]-diff), (centers[i]+diff)))
    yexp <- c(yexp, rep(i*5, numobs))
  }
  x <- matrix(values, nrow = numobs, ncol = m)
  y <- yexp+runif(numobs, -1,1)
  df <- data.frame(y, x, yexp)
  return(df)
}

##############################
#### FUNCTION TO #############
#### RUN TEST OBSERVATION THRU 
#### BLB RF ##################
##############################

# input: a training data frame, and test observation(s). 
# output: (for each test obs.) predicted response, se and the expected vaule

blbrf <- function(train, test, gamma, ntrees) {
  
  # (0) Set up parameters.
  x <- 1:nrow(train) #indices of training set
  n <- length(x) #size of training set
  s <- floor(n / n^gamma) #number of subsamples
  
  subsamps <- matrix(, nrow = nrow(test), ncol = 0)
  
  for(i in 1:s) {
    # (1) Draw a subsample and create vector to store all the resampled responses. 
    subsample <- sample(x, n^gamma, FALSE)
    x <- setdiff(x, subsample)
    resample <- train[0,]
    
    for (ggg in 1:s){
      resample <- rbind(resample, train[subsample,])
    }

    # (4) Run a random forest. 
    rf <- randomForest(y~.-yexp, data = resample, ntree = ntrees)
    
    # (5) Store response. 
    subsamps <- cbind(subsamps, predict(rf, test))
  }
  
  y <- apply(subsamps, 1, mean)
  se <- apply(subsamps, 1, sd) / sqrt(s)
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

points_w_se <- function(train, test, output, cluster = FALSE) {
  # Compute the prediction and se using randomForestInfJack
  rf <- randomForest(y~.-yexp, data = train, ntree = 500, keep.inbag = TRUE, replace = TRUE)
  ij <- randomForestInfJack(rf, test, calibrate = F)
  
  # Store the expected value, predicted vaule from RF and BLB-RF, and the se
  yexp <- output[,3]
  yhat <- c(output[,1], ij[,1])
  se <- c(output[,2], ij[,2])

  # Construct the data frame for plotting
  df <- data.frame(yexp, yhat, se, indicator = c(rep(1,nrow(test)), rep(0, nrow(test))))
  # take a subsample of the dataframe for better illustration
  sampdf <- df[sample(1:nrow(df), nrow(df)/20),]
  
  if(!cluster){
    ggplot(sampdf, aes(x = yexp, y = yhat)) + 
      geom_errorbar(aes(ymin = yhat - 2*se, ymax = yhat + 2*se, colour = factor(indicator)), width = 0.05, size = .45) +
      geom_point(aes(colour = factor(indicator)), size = 1.0) + 
      geom_abline(intercept = 0, slope = 1, linetype = 2) +
      xlab("Truth: y = Bx") + 
      ylab("Prediction") +
      ggtitle("Random Forest vs Random Forest-BLB")  + scale_colour_discrete(name  ="Legend", labels=c("Random Forest", "BLB"))
  }else{
    ggplot(sampdf, aes(x = jitter(yexp), y = yhat)) + 
      geom_point(aes(colour = factor(indicator)), size = 1.0) + 
      geom_errorbar(aes(ymin = yhat - 2*se, ymax = yhat + 2*se, colour = factor(indicator)), width = 0.05, size = .75) +
      geom_abline(intercept = 0, slope = 1, linetype = 2) +
      xlab("Truth: y = Cluster Mean") + 
      ylab("Prediction") +
      ggtitle("Random Forest vs Random Forest-BLB")  + scale_colour_discrete(name  ="Legend", labels=c("Random Forest", "BLB"))
  }
}


# Plot only the points w/out SE!!!!!

points_no_se <- function(train, test, output, cluster = FALSE, hist = FALSE) {
  # Compute the prediction and se using randomForestInfJack
  rf <- randomForest(y~.-yexp, data = train, ntree = 500, keep.inbag = TRUE, replace = TRUE)
  ij <- randomForestInfJack(rf, test, calibrate = T)
  
  rf <- randomForest(y~.-yexp, data = train, ntree = 500)
  yhat.rf <- predict(rf, newdata = test)
  
  # Store the expected value, predicted vaule from RF and BLB-RF, and the se
  yexp <- c(output[,3],output[,3],output[,3])
  yhat <- c(output[,1], ij[,1], yhat.rf)
  se <- c(output[,2], ij[,2], rep(0, nrow(test)))
  
  # Construct the data frame for plotting
  df <- data.frame(yexp, yhat, se, indicator = c(rep('BLB',nrow(test)), rep('JK', nrow(test)),rep('RF', nrow(test))), residual = (yhat - yexp))
  # take a subsample of the dataframe for better illustration
  sampdf <- df[sample(1:nrow(df), nrow(df)/10),]
  
  write.csv(df, file = "/Users/zihaoxu/Desktop/plt.csv")
  
  if(!cluster && !hist){
    #print(11)
      ggplot(sampdf, aes(x = yexp, y = yhat)) + 
      geom_point(aes(colour = factor(indicator)), size = 1.0) + 
      geom_abline(intercept = 0, slope = 1, linetype = 2) +
      xlab("Truth: y = Bx") + 
      ylab("Prediction") +
      ggtitle("Random Forest vs Random Forest-BLB")  + scale_colour_discrete(name  ="Legend", labels=c("Random Forest", "BLB"))
  }else if(cluster && !hist){
    ggplot(sampdf, aes(x = jitter(yexp), y = yhat)) + 
      geom_point(aes(colour = factor(indicator)), size = 1.0) + 
      geom_abline(intercept = 0, slope = 1, linetype = 2) +
      xlab("Truth: y = Cluster Mean") + 
      ylab("Prediction") +
      ggtitle("Random Forest vs Random Forest-BLB")  + scale_colour_discrete(name  ="Legend", labels=c("Random Forest", "BLB"))
  }else{
    ggplot(data = sampdf, aes(abs(residual)))+
      geom_histogram(data=subset(sampdf,indicator == 1),fill = "red", alpha = 0.2, bins = 15) +
      geom_histogram(data=subset(sampdf,indicator == 0),fill = "blue", alpha = 0.2,  bins = 15) +
      geom_freqpoly(data=subset(sampdf,indicator == 1), col = "red", alpha = 0.4)+
      geom_freqpoly(data=subset(sampdf,indicator == 0), col = "blue", alpha = 0.4)+
      xlab("Prediction Error Distribution") + 
      ylab("Count") +
      ggtitle("Prediction Error Dist. for RF vs RF-BLB")  + 
      scale_colour_manual("Legend", 
                          breaks = c("BLB-RF", "RF"),
                          values = c("red", "blue")) 
      
  }
}