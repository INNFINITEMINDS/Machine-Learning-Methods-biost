##
## Lab 6: Bootstrap, and tree-based methods
##

## Bootstrap
calc_statistic <- function(x){
  return(mean(x))
}

n <- 50
X <- rlnorm(n, meanlog=0.2, sdlog=2)

hist(X)
xlog <- log(X)
hist(xlog)

mu_star <- mean(X)
mu_star_star <- replicate(10000, calc_statistic(sample(X, replace=TRUE)))
U <- quantile(mu_star_star - mu_star, 0.975)
L <- quantile(mu_star_star - mu_star, 0.025)
CI <- c(mu_star + L, mu_star + U)

par(mfrow=c(1,2))
hist(mu_star_star, main='Regular Bootstrap')
abline(v=mu_star, col=4, lty=1, lwd=2)
abline(v=CI[1], col=2, lwd=2, lty=2)
abline(v=CI[2], col=2, lwd=2, lty=2)

## Percentile Bootstrap
calc_statistic <- function(x){
  return(mean(x))
}
mu_star <- mean(X)
mu_star_star <- replicate(10000, calc_statistic(sample(X, replace=TRUE)))
U <- quantile(mu_star_star, 0.975)
L <- quantile(mu_star_star, 0.025)
CI <- c(L,U)

hist(mu_star_star, main='Percentile Bootstrap')
abline(v=mu_star, col=4, lty=1, lwd=2)
abline(v=CI[1], col=2, lwd=2, lty=2)
abline(v=CI[2], col=2, lwd=2, lty=2)


## Classification Trees
rm(list=ls())

library(ISLR)
library(MASS)
library(tree)
library(randomForest)
library(gbm)

attach(Carseats)
High=ifelse(Sales<=8,"No","Yes")
Carseats=data.frame(Carseats,High)
#?tree
tree.carseats=tree(High~.-Sales,Carseats)
summary(tree.carseats)
plot(tree.carseats)
text(tree.carseats,pretty=0)
tree.carseats

set.seed(2)
train=sample(1:nrow(Carseats), 200)
Carseats.test=Carseats[-train,]
High.test=High[-train]
tree.carseats=tree(High~.-Sales,Carseats,subset=train)
tree.pred=predict(tree.carseats,Carseats.test,type="class")
table(tree.pred,High.test)
(86+57)/200

set.seed(3)
cv.carseats=cv.tree(tree.carseats,FUN=prune.misclass)
names(cv.carseats)
cv.carseats

#here deviance is similar to entropy, but with n_mk instead...
par(mfrow=c(1,2))
plot(cv.carseats$size,cv.carseats$dev,type="b")		#no of terminal nodes
plot(cv.carseats$k,cv.carseats$dev,type="b")		#cost complexity parameter
prune.carseats=prune.misclass(tree.carseats,best=9)	#best is found based on the CV 
plot(prune.carseats)
text(prune.carseats,pretty=0)
tree.pred=predict(prune.carseats,Carseats.test,type="class")
table(tree.pred,High.test)
(94+60)/200
prune.carseats=prune.misclass(tree.carseats,best=15)
plot(prune.carseats)
text(prune.carseats,pretty=0)
tree.pred=predict(prune.carseats,Carseats.test,type="class")
table(tree.pred,High.test)
(86+62)/200

## Regression Trees

set.seed(1)
train = sample(1:nrow(Boston), nrow(Boston)/2)
tree.boston=tree(medv~.,Boston,subset=train)
summary(tree.boston)
plot(tree.boston)
text(tree.boston,pretty=0)
cv.boston=cv.tree(tree.boston)
plot(cv.boston$size,cv.boston$dev,type='b')		#here the deviance is just RSS
prune.boston=prune.tree(tree.boston,best=5)
plot(prune.boston)
text(prune.boston,pretty=0)
yhat=predict(tree.boston,newdata=Boston[-train,])
boston.test=Boston[-train,"medv"]
plot(yhat,boston.test)
abline(0,1)
mean((yhat-boston.test)^2)

## Bagging and Random Forests
set.seed(1)
dim(Boston)
#?randomForest
bag.boston=randomForest(medv~.,data=Boston,subset=train,mtry=13,importance=TRUE)
bag.boston
yhat.bag = predict(bag.boston,newdata=Boston[-train,])
plot(yhat.bag, boston.test)
abline(0,1)
mean((yhat.bag-boston.test)^2)
bag.boston=randomForest(medv~.,data=Boston,subset=train,mtry=13,ntree=25)
yhat.bag = predict(bag.boston,newdata=Boston[-train,])
mean((yhat.bag-boston.test)^2)

set.seed(1)
rf.boston=randomForest(medv~.,data=Boston,subset=train,mtry=6,importance=TRUE)
yhat.rf = predict(rf.boston,newdata=Boston[-train,])
mean((yhat.rf-boston.test)^2)
importance(rf.boston)
varImpPlot(rf.boston)

## Boosting

set.seed(1)
boost.boston=gbm(medv~.,data=Boston[train,],distribution="gaussian",n.trees=5000,interaction.depth=4)
summary(boost.boston)

# partial dependence plots: 
# marginal effect of variables on response after integrating out  other variables
par(mfrow=c(1,2))
plot(boost.boston,i="rm")
plot(boost.boston,i="lstat")

yhat.boost=predict(boost.boston,newdata=Boston[-train,],n.trees=5000)
mean((yhat.boost-boston.test)^2)

boost.boston=gbm(medv~.,data=Boston[train,],distribution="gaussian",n.trees=5000,interaction.depth=4,shrinkage=0.2,verbose=F)
yhat.boost=predict(boost.boston,newdata=Boston[-train,],n.trees=5000)
mean((yhat.boost-boston.test)^2)

boost.boston=gbm(medv~.,data=Boston[train,],distribution="gaussian",n.trees=5000,interaction.depth=1,verbose=F)
yhat.boost=predict(boost.boston,newdata=Boston[-train,],n.trees=5000)
mean((yhat.boost-boston.test)^2)

boost.boston=gbm(medv~.,data=Boston[train,],distribution="gaussian",n.trees=5000,interaction.depth=2,verbose=F)
yhat.boost=predict(boost.boston,newdata=Boston[-train,],n.trees=5000)
mean((yhat.boost-boston.test)^2)




