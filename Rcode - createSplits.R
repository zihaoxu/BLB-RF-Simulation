library(caret)
library(mlbench)
data(Sonar)

createSplits <- function(df, nfolds = 5){
  folds <- createFolds(df[,1], k = nfolds)
  split_up <- lapply(folds, function(ind, dat) dat[ind,], dat = df)
  return(split_up)
}


split_up <- createSplits(Sonar, 5)
unlist(lapply(split_up, nrow))

for(fold in split_up){
  print(mean(fold$V1))
}
