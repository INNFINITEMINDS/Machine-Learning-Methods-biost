############################################
## LAB 2: Subset Selection Methods
############################################
rm(list=ls())
library(ISLR)
library(leaps)
library(lars)

##
## Best Subset Selection
##

data(diabetes)
attach(diabetes)
dat=data.frame(x,y)
dat2=data.frame(x,x^2,y)

regfit.full=regsubsets(y~.,dat)
summary(regfit.full)
regfit.full=regsubsets(y~.,data=dat,nvmax=10)
reg.summary=summary(regfit.full)
names(reg.summary)
reg.summary$rsq

# plot the selection rules
par(mfrow=c(2,2))
plot(reg.summary$rss,xlab="Number of Variables",ylab="RSS",type="l")
plot(reg.summary$adjr2,xlab="Number of Variables",ylab="Adjusted RSq",type="l")
minindex=which.max(reg.summary$adjr2)
points(minindex,reg.summary$adjr2[minindex], col="red",cex=2,pch=20)
plot(reg.summary$cp,xlab="Number of Variables",ylab="Cp",type='l')
minindex=which.min(reg.summary$cp)
points(minindex,reg.summary$cp[minindex],col="red",cex=2,pch=20)
plot(reg.summary$bic,xlab="Number of Variables",ylab="BIC",type='l')
minindex=which.min(reg.summary$bic)
points(minindex,reg.summary$bic[minindex],col="red",cex=2,pch=20)

# plot the selection resutls
plot(regfit.full,scale="r2")
plot(regfit.full,scale="adjr2")
plot(regfit.full,scale="Cp")
plot(regfit.full,scale="bic")
coef(regfit.full,6)

##
## Forward and Backward Stepwise Selection
##
regfit.fwd=regsubsets(y~.,data=dat,nvmax=ncol(x),method="forward")
summary(regfit.fwd)
#regfit.bwd=regsubsets(y~.,data=dat,nvmax=ncol(x),method="backward")
#summary(regfit.bwd)
coef(regfit.full,5)
coef(regfit.fwd,5)
#coef(regfit.bwd,6)

##
## Choosing Among Models
##

# Validation set approach
set.seed(1)
train=sample(c(TRUE,FALSE), nrow(dat),rep=TRUE)
test=(!train)
regfit.best=regsubsets(y~.,data=dat[train,],nvmax=ncol(x))
test.mat=model.matrix(y~.,data=dat[test,])
val.errors=rep(NA,ncol(x))
for(i in 1:ncol(x)){
   coefi=coef(regfit.best,id=i)
   pred=test.mat[,names(coefi)]%*%coefi
   val.errors[i]=mean((dat$y[test]-pred)^2)
}
val.errors
par(mfrow=c(1,1))
plot(val.errors,type='b')
minindex=which.min(val.errors)
points(minindex,val.errors[minindex],col='red',cex=2,pch=20)

coef(regfit.best, minindex)
predict.regsubsets=function(object,newdata,id,...){
  form=as.formula(object$call[[2]])
  mat=model.matrix(form,newdata)
  coefi=coef(object,id=id)
  xvars=names(coefi)
  mat[,xvars]%*%coefi
  }
regfit.best=regsubsets(y~.,data=dat,nvmax=ncol(x))	#refit the model on the whole data
coef(regfit.best, minindex)							#get teh model with the best complexity

# Cross Validation
k=10		#no of CV-foles 
set.seed(1)
folds=sample(1:k,nrow(dat),replace=TRUE)
cv.errors=matrix(NA,k,ncol(x), dimnames=list(NULL, paste(1:ncol(x))))
for(j in 1:k){
  best.fit=regsubsets(y~.,data=dat[folds!=j,],nvmax=ncol(x))
  for(i in 1:ncol(x)){
    pred=predict(best.fit,dat[folds==j,],id=i)
    cv.errors[j,i]=mean( (dat$y[folds==j]-pred)^2)
  }
}
mean.cv.errors=apply(cv.errors,2,mean)
mean.cv.errors
par(mfrow=c(1,1))
plot(mean.cv.errors,type='b')
minindex=which.min(mean.cv.errors)
points(minindex,mean.cv.errors[minindex],col='red',cex=2,pch=20)

reg.best=regsubsets(y~.,data=dat, nvmax=ncol(x))	#refit the model on the whole data
coef(reg.best,minindex)								#get teh model with the best complexity

plot(mean.cv.errors,type='l',lwd=2,col='orange',xlab='no of variables',ylab='error',ylim=c(2700,4100))
lines(val.errors,col='blue',lwd=2)
legend('topright',c('10-fold CV','Validation set'),lwd=2,bty='n',lty=1,col=c('orange','blue'))
############################################
