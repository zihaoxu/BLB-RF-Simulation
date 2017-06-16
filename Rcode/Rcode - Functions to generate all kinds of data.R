
makeLinearDF <- function(numobs, b, constant){
  values <- c()
  m = length(b)
  for(i in 1:length(b)){
    values = c(values, rnorm(numobs, mean = runif(1, 0, 10), sd = 1))
  }
  x <- matrix(values, nrow = numobs, ncol = m)
  y  = constant + x%*%b + rnorm(numobs)
  yexp = constant + x%*%b 
  df = data.frame(y, x, yexp)
  return(df)
}



makeClusteredDF <- function(numobs, centers, sd){
  values <- c()
  yexp <- c()
  m = length(centers)
  numobs <- floor(numobs/m)
  for(i in 1:m){
    values <- c(values, rnorm(m*numobs, centers[i], sd))
    yexp <- c(yexp, rep(i*runif(1,-50, 50), numobs))
  }
  x <- matrix(values, nrow = m*numobs, byrow = TRUE, ncol = m)
  y <- yexp+runif(numobs, -1,1)
  df <- data.frame(y, x, yexp)
  for(col in 2:m+1){
    print(as.integer(substr(colnames(df)[col], 2, 2)))
    if(as.integer(substr(colnames(df)[col], 2, 2))%%2==0){
      df[col] <- -df[col]
    }
  }
  return(df)
}


########   3 * cos(pi*(x$X1 + x$X2))

makeCosineDF <- function(numobs, numberX = 4){
  xvalues <- c()
  for (i in 1:numberX){
    xvalues <- c(xvalues, runif(numobs, 0, 1))
  }
  x <- data.frame(matrix(xvalues, nrow = numobs, ncol = numberX))
  yexp <- 3 * cos(pi*(x$X1 + x$X2))
  df <- data.frame(y = yexp + rnorm(numobs,0, .1), x, yexp)
  return(df)
}



makeXorDF <- function(numobs, numberX = 4){
  if(numberX%%2!=0){numberX-1}
  nloop = numberX/2
  xvalues <- c()
  for (i in 1:nloop){
    x1 <- runif(numobs,0,1)
    x2 <- runif(numobs,0,1)
    y <- xor(x1>.6, x2)
  }
}
