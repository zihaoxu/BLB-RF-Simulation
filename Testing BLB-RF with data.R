library(tree)
# Linear data generation
# logic: y = 10 + 1 * X1 + 2 * X2 + 3 * X3 + e  (standard normal)
set.seed(47)
data <- makeLinearDF(5000, c(1,5,2,9,2), 10)

######
b0 = 47
b1 = 10
b2 = 20
b3 = 30
X1 = runif(5000, b1, b1+10)
X2 = runif(5000, b2, b2+10)
X3 = runif(5000, b3, b3+10)
ep = runif(5000, -1, 1)
y = b0 + b1 * X1 + b2 * X2+ b3 * X3 + ep
yexp =  b0 + b1 * X1 + b2 * X2+ b3 * X3

data = data.frame(y, X1,X2, X3, yexp)

#######

train_index = sample(1:nrow(data), nrow(data)/2)
data.train = data[train_index,]
data.test = data[-train_index,]

result = blbrf(data.train, data.test, .7, 500)
points_no_se(data.train, data.test, output = result, )


##
tree.data = tree(y ~. -yexp, data, subset = train_index)
summary(tree.data)

plot(tree.data)
text(tree.data)

#####################   RESULTS:
# BLB has a larger variance but both looks fine


#-----------------------------------------------------------


# Clusting with simple logic:
# Logic: if X1/X2 is in between 0 and 10, then cluster 1
#        else if X1/X2 is in between 10 and 20, then cluster 2
#        else cluster 3

set.seed(47)
# cluster 1
X1 = runif(1000, 0, 10)
Y1 = runif(1000, 0, 10)
Z1 = runif(1000, 4, 6)
# cluster 2
X2 = runif(1000, 10, 20)
Y2 = runif(1000, 10, 20)
Z2 = runif(1000, 14, 16)
# cluster 3
X3 = runif(1000, 20, 30)
Y3 = runif(1000, 20, 30)
Z3 = runif(1000, 24, 26)

df = data.frame(y = c(Z1,Z2,Z3), x1 = c(X1, X2, X3), x2 = c(Y1, Y2, Y3),
                yexp = c(rep(5,1000), rep(15,1000), rep(25,1000)))
train.rows <- sample(1:nrow(df), nrow(df)/2)
y.train = df[train.rows, ]
y.test = df[-train.rows, ]

# Visulize the results
x <- blbrf(y.train, y.test, .7, 500)
points_no_se(y.train, y.test, output = x, cluster = TRUE)


#####################   RESULTS:
# Neither experience regression to the mean


#-----------------------------------------------------------




# Cluster with complex logic
# Logic: if x1 smaller than 10, cluster 1
#        else if x2 smaller than 20, cluster 2
#        else cluster 3
set.seed(47)
# cluster 1
X1 = runif(333, 0, 10)
Y1 = runif(333, 0, 50)
Z1 = 5 + runif(333, -5, 5)
# cluster 2
X2 = runif(333, 10, 20)
Y2 = runif(333, 0, 20)
Z2 = runif(333, 10, 20)
# cluster 3
X3 = runif(334, 10, 20)
Y3 = runif(334, 20, 50)
Z3 = runif(334, 20, 30)

df = data.frame(y = c(Z1,Z2,Z3), x1 = c(X1, X2, X3), x2 = c(Y1, Y2, Y3),
                yexp = c(rep(5,333), rep(15,333), rep(25,334)))
train.rows <- sample(1:nrow(df), nrow(df)/2)
y.train = df[train.rows, ]
y.test = df[-train.rows, ]

# Visulize the results
x <- blbrf(y.train, y.test, 10, .6, 800)
makeplot(y.train, y.test, x, cluster = TRUE)

#####################   RESULTS:
# When the true relationship is clustered and with a complex logic, 
# Ranodm forest roughly correctly assigns the mean value to each cluster 
# while BLB-RF is still experiencing regression to the mean when gamma is 
# small !!!! (cluster with lower mean has positive bias while cluster 
# with higher mean has negative bias....)
