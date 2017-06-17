############################################
## LAB 5: Classification, Cntd.
############################################
rm(list=ls())

library(ISLR)
library(MASS)
library(glmnet)
library(class)
library(e1071)
library(ROCR)

setwd('/Users/ashojaie/Dropbox/Teaching/UW/578C_ML4BBD/Spring2015/Rcodes')
load("Heart.rda")
attach(Heart)

names(Heart)
dim(Heart)
summary(Heart)
attach(Heart)
plot(trestbps)

# separate the data into training and test
set.seed(1)
train=sample(1:nrow(Heart), 197)
trndat=Heart[train,]
tstdat=Heart[-train,]
dim(trndat)
dim(tstdat)

# Logistic Regression
HD.tst=HD[-train]
glm.fit=glm(HD~.,data=Heart,family=binomial,subset=train)
glm.probs=predict(glm.fit,tstdat,type="response")
glm.pred=numeric(100)
glm.pred[glm.probs>.5]=1
table(glm.pred,HD.tst)
mean(glm.pred==HD.tst)

# penalized logistic regression
x=as.matrix(Heart[,1:13])
y=as.factor(Heart[,14])

#ridge
glm.ridge=glmnet(x[train,],y[train],family="binomial",alpha=0)
plot(glm.ridge,col=c(1:8), xvar="lambda")
legend('topright',colnames(x),lty=1,col=c(1:8),bty='n')
cv.ridge=cv.glmnet(x[train,],y[train],family="binomial",alpha=0)
plot(cv.ridge)
lambda.min=cv.ridge$lambda.min
ridge.probs=predict(glm.ridge,s=lambda.min,type="response",newx=x[-train,])
ridge.pred=rep(0,100)
ridge.pred[ridge.probs>.5]=1
table(ridge.pred,HD.tst)
mean(ridge.pred==HD.tst)

#lasso
glm.lasso=glmnet(x[train,],y[train],family="binomial",alpha=1)
plot(glm.lasso,col=c(1:8), xvar="lambda")
legend('bottomright',colnames(x),lty=1,col=c(1:8),bty='n')
cv.lasso=cv.glmnet(x[train,],y[train],family="binomial",alpha=1)
plot(cv.lasso)
lambda.min=cv.lasso$lambda.min
lasso.probs=predict(glm.lasso,s=lambda.min,type="response",newx=x[-train,])
lasso.pred=rep(0,100)
lasso.pred[ridge.probs>.5]=1
table(lasso.pred,HD.tst)
mean(lasso.pred==HD.tst)

# Linear Discriminant Analysis

library(MASS)
lda.fit=lda(HD~sex+cp+ca+thal,data=Heart,subset=train)
lda.fit
plot(lda.fit)
lda.pred=predict(lda.fit, tstdat)
lda.class=lda.pred$class
table(lda.class,HD.tst)
mean(lda.class==HD.tst)

# Quadratic Discriminant Analysis

qda.fit=qda(HD~sex+cp+ca+thal,data=Heart,subset=train)
qda.fit
qda.class=predict(qda.fit,tstdat)$class
table(qda.class,HD.tst)
mean(qda.class==HD.tst)

qda.fit=qda(HD~ca+thal,data=Heart,subset=train)		# no major vessels (0-3) colored by flourosopy, Thalium Stress Test
qda.fit
qda.class=predict(qda.fit,tstdat)$class
table(qda.class,HD.tst)
mean(qda.class==HD.tst)

# K-Nearest Neighbors

library(class)
train.X=cbind(ca,thal)[train,]
test.X=cbind(ca,thal)[-train,]
HD.trn=HD[train]
set.seed(1)
knn.pred=knn(train.X,test.X,as.factor(HD.trn),k=1)
table(knn.pred,HD.tst)
mean(knn.pred==HD.tst)
knn.pred=knn(train.X,test.X,HD.trn,k=10)
table(knn.pred,HD.tst)
mean(knn.pred==HD.tst)

# SVM

?svm

dat=Heart
dat$HD=as.factor(HD)
svmfit=svm(HD~.,data=dat[train,],kernel="radial",gamma=1,cost=1)
svm.pred=predict(svmfit,newdata=dat[-train,-14])
table(true=dat[-train,14], pred=svm.pred)
mean(dat[-train,14]==svm.pred)

set.seed(1)
tune.out=tune.svm(x=dat[train,-14],y=dat[train,14],kernel="radial",
	cost=c(0.1,1,10,100,1000),gamma=c(0.5,1,2,3,4))
summary(tune.out)
svmfit=svm(HD~.,data=dat[train,],kernel="radial",gamma=tune.out$best.parameters$gamma,cost=tune.out$best.parameters$cost)
svm.pred=predict(svmfit,newdata=dat[-train,-14])
table(true=dat[-train,14], pred=svm.pred)
mean(dat[-train,14]==svm.pred)

# A closer look at SVM

set.seed(1)
x=matrix(rnorm(200*2), ncol=2)
x[1:100,]=x[1:100,]+2
x[101:150,]=x[101:150,]-2
y=c(rep(1,150),rep(2,50))
plot(x, col=y)

dat=data.frame(x=x,y=as.factor(y))
train=sample(200,100)

svmfit=svm(y~.,data=dat[train,],kernel="radial",gamma=1,cost=1)
plot(svmfit, dat[train,])
summary(svmfit)
svmfit=svm(y~., data=dat[train,], kernel="radial",gamma=1,cost=1e5)
plot(svmfit,dat[train,])

set.seed(1)
tune.out=tune(svm, y~., data=dat[train,], kernel="radial", ranges=list(cost=c(0.1,1,10,100,1000),gamma=c(0.5,1,2,3,4)))
summary(tune.out)
table(true=dat[-train,"y"], pred=predict(tune.out$best.model,newx=dat[-train,]))

# ROC Curves

rocplot=function(pred, truth, ...){
   predob = prediction(pred, truth)
   perf = performance(predob, "tpr", "fpr")
   plot(perf,...)}

svmfit.opt=svm(y~., data=dat[train,], kernel="radial",gamma=2, cost=1,decision.values=T)
fitted=attributes(predict(svmfit.opt,dat[train,],decision.values=TRUE))$decision.values
par(mfrow=c(1,2))
rocplot(fitted,dat[train,"y"],main="Training Data")
svmfit.flex=svm(y~., data=dat[train,], kernel="radial",gamma=50, cost=1, decision.values=T)
fitted=attributes(predict(svmfit.flex,dat[train,],decision.values=T))$decision.values
rocplot(fitted,dat[train,"y"],add=T,col="red")
legend('bottomright',c('optimal model','flexible model'),lty=1,col=c(1,2),bty='n')
fitted=attributes(predict(svmfit.opt,dat[-train,],decision.values=T))$decision.values
rocplot(fitted,dat[-train,"y"],main="Test Data")
fitted=attributes(predict(svmfit.flex,dat[-train,],decision.values=T))$decision.values
rocplot(fitted,dat[-train,"y"],add=T,col="red")

############################################
