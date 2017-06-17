############################################
## LAB 3: Regularization and Dimension Reduction Methods
############################################
rm(list=ls())

library(lars)
library(MASS)
library(glmnet)
library(pls)
library(ElemStatLearn)

data(diabetes)
data(prostate)

dim(prostate)
pairs(prostate[,1:9])

whichdat = 'diabetes'

if(whichdat == 'diabetes'){
	#for diabetes data
	x = diabetes$x
	y = diabetes$y
	dat = data.frame(x,y)
}else{
	#for prostate data
	x = as.matrix(prostate[,c(1:8)])
	y = prostate[,9]
	dat = prostate[,c(1:9)]
	colnames(dat)[9] = 'y'
}

##
## Ridge Regression
##
#fitting the model over a grid of lambda values
grid=10^seq(5,-2,length=100)
ridge.mod=glmnet(x,y,alpha=0,lambda=grid)
plot(ridge.mod,xvar="lambda",col=c(1:ncol(x)))
legend('topright',colnames(x),lty=1,col=c(1:ncol(x)),bty='n')

#checking the model for two values of lambda
dim(coef(ridge.mod))

ridge.mod$lambda[50]
coef(ridge.mod)[,50]
sqrt(sum(coef(ridge.mod)[-1,50]^2))

ridge.mod$lambda[60]
coef(ridge.mod)[,60]
sqrt(sum(coef(ridge.mod)[-1,60]^2))

#using the predict function to get the coefficients for a given lambda
predict(ridge.mod,s=50,type="coefficients")[1:9,]		#s=lambda

#checking the performance on the test data
set.seed(1)
train=sample(1:nrow(x), nrow(x)/2)
test=(-train)
y.test=y[test]

ridge.mod=glmnet(x[train,],y[train],alpha=0,lambda=grid, thresh=1e-12)

#the ridge model for lambda=4
ridge.pred=predict(ridge.mod,s=4,newx=x[test,])
mean((ridge.pred-y.test)^2)

#a model with only intercept
ridge.pred=predict(ridge.mod,s=1e10,newx=x[test,])
mean((ridge.pred-y.test)^2)
mean((mean(y[train])-y.test)^2)

#the LS model (no regularization)
ridge.pred=predict(ridge.mod,s=0,newx=x[test,],exact=T)
mean((ridge.pred-y.test)^2)
predict(ridge.mod,s=0,exact=T,type="coefficients")[1:9,]

set.seed(1)
cv.out=cv.glmnet(x[train,],y[train],alpha=0)
plot(cv.out)
bestlam=cv.out$lambda.min
bestlam

#how does the best lambda work?
ridge.pred=predict(ridge.mod,s=bestlam,newx=x[test,])
mean((ridge.pred-y.test)^2)

out=glmnet(x,y,alpha=0)
predict(out,type="coefficients",s=bestlam)[1:9,]

##
## The Lasso
##
lasso.mod=glmnet(x[train,],y[train],alpha=1,lambda=grid)
plot(lasso.mod,xvar="lambda",col=c(1:ncol(x)),lwd=c(1,2,3))
legend('topright',colnames(x),col=c(1:ncol(x)),bty='n',lwd=c(1,2,3))

par(mfrow=c(1,2))
plot(ridge.mod,xvar="lambda",col=c(1:ncol(x)))
legend('topright',colnames(x),lty=1,col=c(1:ncol(x)),bty='n')
plot(lasso.mod,xvar="lambda",col=c(1:ncol(x)))

set.seed(1)
cv.out=cv.glmnet(x[train,],y[train],alpha=1)
plot(cv.out)
bestlam=cv.out$lambda.min

lasso.pred=predict(lasso.mod,s=bestlam,newx=x[test,])
mean((lasso.pred-y.test)^2)

out=glmnet(x,y,alpha=1,lambda=grid)
lasso.coef=predict(out,type="coefficients",s=bestlam)[1:9,]
lasso.coef
lasso.coef[lasso.coef!=0]

##
## Principal Components Regression
##
set.seed(2)

#PCR with 10-fold CV (to choose the no of components)
pcr.fit=pcr(y~.,data=dat,scale=TRUE,validation="CV")	#NOTE: pcr reports RMSE
summary(pcr.fit)
validationplot(pcr.fit,val.type="MSEP")

set.seed(1)
pcr.fit=pcr(y~.,data=dat,subset=train,scale=TRUE,validation="CV")
validationplot(pcr.fit,val.type="MSEP")
#which.min(RMSEP(pcr.fit)$val[1,,])

pcr.pred=predict(pcr.fit,x[test,],ncomp=7)
mean((pcr.pred-y.test)^2)
pcr.fit=pcr(y~x,scale=TRUE,ncomp=7)
summary(pcr.fit)

##
## Partial Least Squares (not covered in lecture)
##
set.seed(1)
pls.fit=plsr(y~.,data=dat,subset=train,scale=TRUE,validation="CV")
summary(pls.fit)

validationplot(pls.fit,val.type="MSEP")
which.min(RMSEP(pls.fit)$val[1,,])

pls.pred=predict(pls.fit,x[test,],ncomp=3)
mean((pls.pred-y.test)^2)
pls.fit=plsr(y~.,data=dat,scale=TRUE,ncomp=3)
summary(pls.fit)

############################################
