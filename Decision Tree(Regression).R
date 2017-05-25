library(MASS)
set.seed(1)
train = sample(1:nrow(Boston), nrow(Boston)/2)
tree.boston = tree(medv ~., Boston, subset = train)
summary(tree.boston)
  
plot(tree.boston)
text(tree.boston, pretty = 0)

# now use cross validation to see size ~ error
cv.boston = cv.tree(tree.boston)
plot(cv.boston$size, cv.boston$dev, type = 'b')

yhat = predict(tree.boston, Boston[-train,])
boston.test = Boston[-train, "medv"]
plot(yhat, boston.test)
abline(0,1)
# MSE
mean((yhat - boston.test)^2)

# The square root of the MSE is therefore around 5.005, 
# indicating that this model leads to test predictions 
# that are within around $5, 005 of the true median 
# home value for the suburb.
