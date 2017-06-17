###########
## LAB 1 ##
###########

##
##Linear Regression
##
library(MASS)
library(ISLR)
library(lars)

data(diabetes)
attach(diabetes)
dim(x)
colnames(x)

# Simple Linear Regression

#plot associations with y, for numerical variabels
par(mfrow=c(3,3), mar=c(4,4,1,1))
for(i in 3:ncol(x)){
	plot(x[,i], y, xlab=colnames(x)[i])
}

bmi=x[,3]
hist(bmi)

lm.fit=lm(y~bmi)
lm.fit
summary(lm.fit)
names(lm.fit)
coef(lm.fit)
confint(lm.fit)

predict(lm.fit,data.frame(bmi=(c(-0.05,0,0.05))), interval="confidence")
predict(lm.fit,data.frame(bmi=(c(-0.05,0,0.05))), interval="prediction")

plot(bmi,y,pch=20,col='gray')
abline(lm.fit,lwd=3,col="blue")

par(mfrow=c(2,2))
plot(lm.fit)

# Multiple Linear Regression
pairs(x)

lm.fit=lm(y~x)
lm.fit
summary(lm.fit)
names(lm.fit)
coef(lm.fit)
confint(lm.fit)

# Interaction Terms
lm.fit=lm(y~x2)
lm.fit
summary(lm.fit)
names(lm.fit)
coef(lm.fit)
confint(lm.fit)

##
##Training/Test Error & and Bias-Var Tradeoff
##

# The Validation Set Approach
set.seed(1)
train=sample(nrow(x),nrow(x)/2)

lm.fit=lm(y~bmi,subset=train)
mean(lm.fit$residuals^2)				#training error
mean((y-predict(lm.fit,bmi))[-train]^2)	#test error

lm.fit2=lm(y~poly(bmi,2),subset=train)
mean(lm.fit2$residuals^2)				#training error
mean((y-predict(lm.fit2,bmi))[-train]^2)	#test error

lm.fit3=lm(y~poly(bmi,3),subset=train)
mean(lm.fit3$residuals^2)				#training error
mean((y-predict(lm.fit3,bmi))[-train]^2)	#test error

set.seed(2)
train=sample(nrow(x),nrow(x)/2)

lm.fit=lm(y~bmi,subset=train)
mean(lm.fit$residuals^2)				#training error
mean((y-predict(lm.fit,bmi))[-train]^2)	#test error

lm.fit2=lm(y~poly(bmi,2),subset=train)
mean(lm.fit2$residuals^2)				#training error
mean((y-predict(lm.fit2,bmi))[-train]^2)	#test error

lm.fit3=lm(y~poly(bmi,3),subset=train)
mean(lm.fit3$residuals^2)				#training error
mean((y-predict(lm.fit3,bmi))[-train]^2)	#test error

##
## practice with writing functions in R
##

# This function estimates the train and test errors for simple regressions with polynomials of deg=1:3
# for a given seed, and returns these values in a 2x3 matrix
myfn <- function(xx,yy,seed){
	set.seed(seed)
	train=sample(length(yy),length(yy)/2)
	out=matrix(,2,3)
	
	for(j in 1:3){
		fit=lm(yy~poly(xx,j),subset=train)
		out[1,j]=mean(fit$residuals^2)				#training error
		out[2,j]=mean((yy-predict(fit,xx))[-train]^2)	#test error
	}
	
	return(out)
}

err.1 = myfn(bmi,y,1)
err.2 = myfn(bmi,y,2)

matplot(t(log(err.1)), axes=F, ylab='log(err)', xlab='poly deg', type='l', ylim=c(log(3540),log(4300)),
	col=c(4,2), lty=1)
axis(2); axis(1, at=1:3, labels=1:3); box()
legend('topleft', c('train','test'), lty=1, col=c(4,2), bty='n')

matlines(t(log(err.2)), col=c(4,2), lty=1)


##
## Exercise: Plot the errors for 100 train/test pairs
##


##
## Leave-One-Out Cross-Validation
##
dat=data.frame(bmi,y)
glm.fit=glm(y~bmi, data=dat)
coef(glm.fit)
library(boot)
cv.err=cv.glm(data=dat, glmfit=glm.fit)		#not specifying K=LOOCV
cv.err$delta

cv.error=numeric(5)
for (i in 1:5){
 glm.fit=glm(y~poly(bmi,i),data=dat)
 cv.error[i]=cv.glm(dat,glm.fit)$delta[1]
 }
cv.error

##
## k-Fold Cross-Validation
##
set.seed(10)
cv.error.10=numeric(10)
train.err.10=numeric(10)
for (i in 1:10){
 glm.fit=glm(y~poly(bmi,i),data=dat)
 cv.error.10[i]=cv.glm(dat,glm.fit,K=10)$delta[1]
 train.err.10[i]=mean(glm.fit$residuals^2)
 }
cv.error.10

# plot LOOCV and 10-fold CV error estimates
plot(cv.error, type='l', xlab='deg of polynomial', ylim=c(3920,4000), lwd=2)
lines(cv.error.10[1:5], col='blue', lwd=2)
legend('topleft',c('LOOCV','10-fold CV'), col=c(1,4), lwd=2, bty='n')

# plot train and test error (10-CV)
plot(train.err.10, type='l', xlab='deg of polynomial', ylim=c(3790,4900), lwd=2, ylab='error')
lines(cv.error.10, col='blue', lwd=2)
legend('topleft',c('train error','10-fold CV'), col=c(1,4), lwd=2, bty='n')

# look at the distribution of CV-based MSE 
dat1 = data.frame(x,y)
dat2 = data.frame(x2,y)
cverr12 = matrix(,100,2)
for(i in 1:100){
	set.seed(i)
	#x
	glm.fit=glm(y~.,data=dat1)
	cverr12[i,1]=cv.glm(dat1,glm.fit,K=10)$delta[1]
	#x2
	glm.fit=glm(y~.,data=dat2)
	cverr12[i,2]=cv.glm(dat2,glm.fit,K=10)$delta[1]
}
boxplot(cverr12, col=c(4,2), names=c('x','x2'), ylab='error')
