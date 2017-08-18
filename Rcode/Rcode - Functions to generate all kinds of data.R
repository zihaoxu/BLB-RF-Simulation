
makeLinearDF <- function(numobs, b, constant){
  values <- c()
  m = length(b)
  for(i in 1:length(b)){
    values = c(values, runif(numobs,0,1))
  }
  x <- matrix(values, nrow = numobs, ncol = m)
  y  = constant + x%*%b + rnorm(numobs)
  yexp = constant + x%*%b 
  df = data.frame(y, x, yexp)
  return(df)
}



# makeClusteredDF borrowed from Benji Lu

#####
# generates data that is clustered. the response values of observations within a given cluster are drawn from the same normal distribution
#
# num.dim (double): number of predictor variables for the generated dataset
# sample.size (double): sample size for the generated dataset
# num.clusters (double): number of clusters into which the data will be divided. Each cluster will contain roughly, but not necessarily exactly, the same number of observations.
# NOTE: num.clusters must be greater than or equal to 2
# cluster.means (vector): a vector of length num.clusters specifying the mean response value for observations in each cluster
# NOTE: length of vector must equal num.clusters
# error.SD (double/vector): the standard deviation of the error term. If constant.SD is FALSE, this must be a vector of length num.clusters
# seed (double): seed state
# constant.SD (boolean): TRUE if the standard deviation of the error term is the same for each cluster, FALSE if the standard deviation of the error term is not the same for each cluster
# RETURNS: a generated sample in a sample.size x (num.dim + 1) dataframe
#
#####
makeClusteredDF <- function(num.dim, sample.size, num.clusters, cluster.means, error.SD, seed, constant.SD = TRUE) {
  
  # required package (run -install.packages("MixSim")- in the console to install if you haven't already)
  require(MixSim)
  
  # set seed
  # set.seed(seed)
  
  # use the MixSim package to generate a finite mixture model with Gaussian components with 0 average overlap and 0 maximum overlap of clusters desired
  repeat {
    mixModel <- MixSim(BarOmega = 0, MaxOmega = 0, K = num.clusters, p = num.dim)
    if (mixModel$fail == 0) break
  }
  
  # use the MixSim package to generate explanatory variable values for the observations, as well as a label for the cluster to which the observation belongs
  data.points <- simdataset(n = sample.size, Pi = mixModel$Pi, Mu = mixModel$Mu, S = mixModel$S)
  
  # if error standard deviation is the same for all clusters
  if (constant.SD) {
    # each observation's response value is its cluster's mean response plus an error term with fixed standard deviation
    data.points$y <- cluster.means[data.points$id] + rnorm(sample.size, 0, error.SD)
    data.points$yexp <- cluster.means[data.points$id]
  } else {
    # each observation's response value is its cluster's mean response plus an error term with cluster-specific standard deviation
    data.points$yexp <- cluster.means[data.points$id]
    data.points$y <- cluster.means[data.points$id] + rnorm(sample.size, 0, error.SD[data.points$id])
  }
  
  # save sample
  #write.csv(data.points, paste0("clustered", "Mean", paste0(cluster.means, collapse = "_"), "SD", paste0(error.SD, collapse = "_"), "Dataset.csv"), row.names = FALSE)
  
  # convert to dataframe and remove cluster label
  data.points <- as.data.frame(cbind(y = data.points$y, xvals = data.points$X, yexp = data.points$yexp))
  
  # return dataframe
  return(data.points)
}


########   50 * cos(pi*(x$X1 + x$X2))

makeCosineDF <- function(numobs, numberX = 5){
  xvalues <- c()
  for (i in 1:numberX){
    xvalues <- c(xvalues, runif(numobs, 0, 1))
  }
  x <- data.frame(matrix(xvalues, nrow = numobs, ncol = numberX))
  yexp <- 50 * cos(pi*(x$X1 + x$X2))
  df <- data.frame(y = yexp + rnorm(numobs, 0, 1), x, yexp)
  return(df)
}

