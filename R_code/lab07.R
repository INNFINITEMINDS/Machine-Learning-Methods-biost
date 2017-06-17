############################################
## Lab 7: High-dimensional inference
############################################

M = 10000
n = 100

##
## First, no effects at all
##
y = as.factor(c(rep("normal",n/2),rep("cancer",n/2)))
ttst.p <- function(x){
	t.test(x[y == "cancer"], x[y == "normal"])$p.value
}
ttst.s <- function(x){
	t.test(x[y == "cancer"], x[y == "normal"])$statistic
}

set.seed(1)
expdat <- matrix(rnorm(n*M), nrow=M)		#NOTE: nothing's significant
pvals <- apply(expdat, 1, ttst.p)
stats <- apply(expdat, 1, ttst.s)

min(pvals)

hist(pvals, freq=F, main="Histogram of p-values", xlab='p-values', breaks=20)
abline(h=1, col=2, lwd=2, lty=2)

hist(stats, freq=F, breaks=100, main='t-statistic Histogram', xlab='t-statistic')
curve(dnorm(x, mean=0, sd=1), add=TRUE, col=2, lty=2, lwd=2)

plot(c(1:M)/M,sort(pvals),xlab='expected',ylab='observed',
	main='U[0,1] QQ-plot')
abline(0,1,col=2,lty=2,lwd=2)
		
indx <- which.min(pvals)
t.test(expdat[indx,y=='cancer'], expdat[indx,y=='normal'])

maxstat <- function(m){
	expdat <- matrix(rnorm(n*m),nrow=m)
	tststat <- apply(expdat, 1, ttst.s)
	return(max(abs(tststat)))
}

nrej <- function(m){
	expdat <- matrix(rnorm(n*m),nrow=m)
	tststat <- apply(expdat, 1, ttst.p)
	return(sum(tststat<0.05))
}

set.seed(2)
#max.stats = replicate(100, maxstat(1000))
#mean(max.stats)
#hist(max.stats)
no.rej = replicate(100, nrej(1000))
mean(no.rej)
hist(no.rej)





##
## What happens if there are some effects?
##
M1 = 10000
set.seed(3)
shift = c(runif(M1/2,0.1,0.5), runif(M1/2,-0.5,-0.1))
shift = matrix(shift, nrow=M1, ncol=n/2, byrow=F)
expdat2 = expdat
expdat2[1:M1,1:(n/2)] = expdat2[1:M1,1:(n/2)] + shift

pvals2 <- apply(expdat2, 1, ttst.p)
stats2 <- apply(expdat2, 1, ttst.s)

min(pvals2)

hist(pvals2, freq=F, main="Histogram of p-values", xlab='p-values', breaks=100)
abline(h=1, col=2, lwd=2, lty=2)

hist(stats2, freq=F, ylim=c(0,.4), breaks=100, main='t-statistic Histogram', xlab='t-statistic')
curve(dnorm(x, mean=0, sd=1), add=TRUE, col=2, lty=2, lwd=2)

plot(c(1:M)/M,sort(pvals2),xlab='expected',ylab='observed',
	main='U[0,1] QQ-plot')
abline(0,1,col=2,lty=2,lwd=2)

##
## FDR in R
##
?p.adjust

# everything null
x <- matrix(rnorm(1000*50),ncol=50)
y <- sample(c(0,1),50,rep=TRUE)
ps <- NULL
for(i in 1:1000) ps <- c(ps, t.test(x[i,y==0],x[i,y==1])$p.value)
cat("Around 5% of p-values are below 0.05:", 
mean(ps<.05),fill=TRUE)
fdrs.bh <- p.adjust(ps, method="BH")
plot(ps,fdrs.bh, ylim=c(0,1))
abline(v=0.05, col=2)
abline(h=0.05, col=4)

plot(fdrs.bh, ylim=c(0,1))
abline(h=0.8, col=2)
abline(v=100, col=2)

# some non-null
x <- matrix(rnorm(1000*50),ncol=50)
y <- sample(c(0,1),50,rep=TRUE)
x[1:100,y==0] <- x[1:100,y==0] + 1
ps <- NULL
for(i in 1:1000) ps <- c(ps, t.test(x[i,y==0],x[i,y==1])$p.value)
cat("Way more than 5% of p-values are below 0.05:", 
mean(ps<.05),fill=TRUE)
fdrs.bh <- p.adjust(ps, method="BH")
plot(ps,fdrs.bh)
plot(fdrs.bh)
abline(h=0.2, col=2)
abline(v=100, col=2)

cat("Number of Tests with FDR below 0.4:", sum(fdrs.bh<0.4), fill=TRUE)
cat("Compute the BH FDR Directly:", max(which(sort(ps,decreasing=FALSE) < .4*(1:1000)/1000)), fill=TRUE)

plot(sort(ps,decreasing=FALSE),ylab="P-Values",cex=0.7)
abline(a=0, b=0.2/1000,col=2, lwd=2, lty=2)
abline(h=0.2/1000, col=4, lwd=2, lty=4)
legend('topleft',c('Bonferroni','FDR'),lty=c(4,2),col=c(4,2),lwd=2,bty='n')

##
##Analysis of cancer immortality data
##
#### Used the liposarcoma samples from:
#### http://www.ncbi.nlm.nih.gov/geo/query/acc.cgi?acc=GSE14533

#### The data is cancer samples with one of 2 different mechanisms for replacing Telomeres:
#### Without replacing Telomeres, fast replicating cells will quickly die off
#### Telo --> Telomerase is the standard mechanism
#### Alt ---> Is the alternative mechanism (poorly understood)

#### Can we find genes which are over/under expressed in Telo vs Alt? (ideally over expressed in Alt)

#### Grabbing the data (GEO didn't work, so I did it by hand)

expression.list <- list()
dataset <- c(363613 + 0:17)
id <- 26946 + 0:17
for(i in 1:18){
    site <- paste("http://www.ncbi.nlm.nih.gov/geo/query/acc.cgi?view=data&acc=GSM",dataset[i]
    ,"&id=",id[i], "&db=GeoDb_blob35", sep = "")
    expression.list[[i]] <- read.table(site, skip = 22, sep = "\t",
    na.strings = "", fill = TRUE, as.is = TRUE,
    col.names = c("probe_id","value"))
    cat(i)
}

###### These next two steps may be unnecessary (the expression platforms were the same) but better safe than sorry!

#### Taking the intersection of the probe-names across samples
use.names <- expression.list[[1]][,1]
for(i in length(expression.list)){
    use.names <- intersect(use.names, expression.list[[i]][,1])
}

#### Matching up the expression values by probe across samples
exp.matrix <- matrix(0,nrow = length(use.names), ncol = length(expression.list))
for(i in 1:length(expression.list)){
    match.ind <- match(use.names, expression.list[[i]][,1])
    exp.matrix[,i] <- expression.list[[i]][match.ind,2]
}

### Storing the response (you can see this from the website)
immort <- c(rep("alt",10),rep("telo",8))

## need to impute the missing values, for now drop them
indx <- (apply(is.na(exp.matrix), 1, sum) > 0)
sum(indx)
X <- exp.matrix[!indx,]

### Running our tests!
p.vals <- apply(X,1,function(x){t.test(x[immort=="alt"], x[immort=="telo"])$p.val})
t.stats <- apply(X,1,function(x){t.test(x[immort=="alt"], x[immort=="telo"])$statistic})

hist(p.vals, freq = FALSE, xlab = "p-value", main = "p-value Histogram", breaks = 100)
abline(h = 1, col = 2, lty = 2, lwd = 5)

hist(t.stats, freq = FALSE, breaks = 100, xlab="t-statistic", main = "t-statistic Histogram")
lines(seq(-4,4,by = 0.01), dt(seq(-4,4,by = 0.01),15), col = 2, lty = 2, lwd = 5)

plot(1:2000,sort(p.adjust(p.vals, method = "BH"), decreasing = FALSE)[1:2000], xlab = "number rejected",ylab ="FDR control", type = 'l', lwd = 3)





