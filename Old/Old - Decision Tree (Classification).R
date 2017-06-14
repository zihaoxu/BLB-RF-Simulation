library(tree)
library(ISLR)
attach(Carseats)
High = ifelse(Sales<=8, "No", 'Yes')
Carseats = data.frame(Carseats, High)

tree.carseats = tree(High~.-Sales, Carseats)
summary(tree.carseats)

plot(tree.carseats)
text(tree.carseats)

set.seed(2)
train = sample(1:nrow(Carseats), 200)
Carseats.test = Carseats[-train,]
High.test = High[-train]
tree.carseats = tree(High~.-Sales, Carseats, subset = train)
# type = "class" tells R to give the actual No Yes answer
tree.pred = predict(tree.carseats, Carseats.test, type = "class")
table(tree.pred, High.test)


#Next, we consider whether pruning the tree might lead to improved results
cv.carseats = cv.tree(tree.carseats, FUN = prune.misclass)

par(mfrow = c(1,2))
plot(cv.carseats$size, cv.carseats$dev, type = 'b')
plot(cv.carseats$k, cv.carseats$dev, type = 'b')

# now create the pruned tree
prune.tree = prune.misclass(tree.carseats, best = 9)
plot(prune.tree)
text(prune.tree, pretty=0)

# test the new correctness

tree.pred = predict(prune.tree, Carseats.test, type = "class")
table(tree.pred, High.test)
print(paste("correct rate is:",(94+60)/200))



