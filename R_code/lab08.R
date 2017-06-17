############################################
## LAB 8: Dimension Reduction Methods
############################################

rm(list=ls())
library(MASS)
library(ISLR)
library(cluster)

##
## PCA for USArrests
##
data(USArrests)

pdf('arrests-1.pdf')
pairs(USArrests, col=4, cex=0.7, upper.panel=panel.smooth)
dev.off()

states <- row.names(USArrests)
states[1:5]

apply(USArrests, 2, mean)
apply(USArrests, 2, var)

pc.out <- prcomp(USArrests, scale=TRUE)
print(pc.out$rot)

pc.out <- prcomp(USArrests, scale=TRUE, retx=TRUE)
names(pc.out)

#plotting the observations and variables in the PC space
plot(pc.out$x[,1], pc.out$x[,2], type="n", xlab="1st PC", ylab="2nd PC")
text(pc.out$x[,1], pc.out$x[,2], labels=states, cex=0.7, col=4)

plot(pc.out$rot, type="n", xlim=c(-0.6, -0.2))
text(pc.out$rot, names(USArrests), col=4)

#biplot
biplot(pc.out)

#scree plots & PVE
screeplot(pc.out)

par(mar=c(2.5,4.5,.5,.5))
plot(pc.out, col='orange', main='', type='l')
axis(1, at=c(0.7, 1.8, 3.2, 4.5), labels=paste('PC', 1:4))

print(pc.out$sdev^2)
print((pc.out$sdev^2)/sum(pc.out$sdev^2))

par(mfrow=c(1,2), mar=c(4.5,4.5,.5,.5))
plot((pc.out$sdev^2)/sum(pc.out$sdev^2), xlab="PC", ylab="PVE", 
	col=4, ylim=c(0, 1), type='b')
plot(cumsum(pc.out$sdev^2)/sum(pc.out$sdev^2), xlab="PC", col=4, 
	ylab="Cumulative PVE", ylim=c(0, 1), type='b')

##
##MDS on the arrests data
##

dist.1 <- daisy(USArrests)
dist.2 <- daisy(USArrests, metric="manhattan")

mds.1 <- cmdscale(dist.1)
mds.2 <- cmdscale(dist.2)
mds.3 <- isoMDS(dist.1)

#plotting the MDS resutls
par(mfrow=c(1,3), mar=c(4.5,4.5,2.5,.5))
x <- mds.1[,1]
y <- mds.1[,2]
plot(x, y, xlab = "Coordinate 1", ylab = "Coordinate 2",
	xlim = range(x)*1.2, type = "n", main='Euclidean Dist')
text(x, y, labels = rownames(USArrests), col='blue', cex=0.7)

x <- mds.2[,1]
y <- mds.2[,2]
plot(x, y, xlab = "Coordinate 1", ylab = "Coordinate 2",
	xlim = range(x)*1.2, type = "n", main='Manhattan Dist')
text(x, y, labels = rownames(USArrests), col='blue', cex=0.7)


x <- mds.3$points[,1]
y <- mds.3$points[,2]
plot(x, y, xlab = "Coordinate 1", ylab = "Coordinate 2",
	xlim = range(x)*1.2, type = "n", main='Non-Metric MDS')
text(x, y, labels = rownames(USArrests), col='blue', cex=0.7)


##
## Analysis of NCI60 data
##

## PCA

data(NCI60)
dat <- NCI60$data
#dim(dat)

labs <- NCI60$labs
#table(labs)

##remove subtypes with only one member
indx <- names(table(labs))[table(labs) == 1]
dat <- dat[!(labs %in% indx),]
#dim(dat)
labs2 <- labs[!(labs %in% indx)]

#dat <- scale(dat)

dat2 <- t(dat)
#dim(dat2)

##select subset of data and order the samples
ord <- order(labs2)
dat <- dat[ord,]
labs2 <- labs2[ord]
dat2 <- dat2[,ord]

pc.nci <- prcomp(dat, scale=TRUE, retx=TRUE)
#summary(pc.nci)

pc.nci <- prcomp(dat2, scale=TRUE, retx=TRUE)
#summary(pc.nci)

#scree plot
par(mfrow=c(1,2), mar=c(4.5, 4.5, 1, .5))
plot((pc.nci$sdev^2)/sum(pc.nci$sdev^2), xlab="PC", ylab="PVE", 
	col=4, ylim=c(0, 1), type='b', cex=0.5)
#plot(cumsum(pc.nci$sdev^2)/sum(pc.nci$sdev^2), xlab="PC", col=4, 
#	ylab="Cumulative PVE", ylim=c(0, 1), type='b')
screeplot(pc.nci, type='l', col='blue', main='') #main='NCI60 data', 

#plotting the observations in the PC space
mycol <- as.integer(as.factor(labs2))
par(mfrow=c(1,3), mar=c(4.5,4.5,.5,.5))
plot(pc.nci$x[,1], pc.nci$x[,2], type="p", xlab="1st PC", ylab="2nd PC",
    cex=1.2, col=mycol, pch=mycol)
legend('topright', unique(labs2), col=unique(mycol), pch=unique(mycol), cex=0.6)
plot(pc.nci$x[,1], pc.nci$x[,3], type="p", xlab="1st PC", ylab="3rd PC",
    cex=1.2, col=mycol, pch=mycol)
plot(pc.nci$x[,2], pc.nci$x[,3], type="p", xlab="2nd PC", ylab="3rd PC",
    cex=1.2, col=mycol, pch=mycol)

## MDS

dist.1 <- daisy(dat)
dist.2 <- daisy(dat, metric="manhattan")

mds.1 <- cmdscale(dist.1)
mds.2 <- cmdscale(dist.2)
mds.3 <- isoMDS(dist.1)

par(mfrow=c(1,3), mar=c(4.5,4.5,2.5,.5))
x <- mds.1[,1]
y <- mds.1[,2]
plot(x, y, xlab = "Coordinate 1", ylab = "Coordinate 2",
	xlim = range(x)*1.2, type = "p", main='Euclidean Dist', pch=mycol,col=mycol)
legend('topright', unique(labs2), col=unique(mycol), pch=unique(mycol), cex=0.6)

x <- mds.2[,1]
y <- mds.2[,2]
plot(x, y, xlab = "Coordinate 1", ylab = "Coordinate 2",
	xlim = range(x)*1.2, type = "p", main='Manhattan Dist', pch=mycol,col=mycol)

x <- mds.3$points[,1]
y <- mds.3$points[,2]
plot(x, y, xlab = "Coordinate 1", ylab = "Coordinate 2",
	xlim = range(x)*1.2, type = "p", main='Non-Metric MDS', pch=mycol,col=mycol)
