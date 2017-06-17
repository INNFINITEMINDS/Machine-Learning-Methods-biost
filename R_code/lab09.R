############################################
## LAB 9: Clustering Methods
############################################

library(MASS)
library(ISLR)
library(cluster)
library(sparcl)

##
## load the NCI60 data
##
data(NCI60)
dat <- NCI60$data
#dim(dat)

labs <- NCI60$labs
#table(labs)

##remove subtypes with only one member
indx <- names(table(labs))[table(labs) == 1]
dat <- dat[!(labs %in% indx),]
labs <- labs[!(labs %in% indx)]
#table(labs)

dat <- scale(dat, center=F, scale=T)

##define the distance
d <- dist(dat)

pdf('hclustex.pdf', height=6, width=16)
par(mfrow=c(1,3))
plot(hclust(d), labels=labs, col="green", main="Complete Linkage", hang=.1,
	xlab="", sub="", ylab="", cex.main=1.5, cex.lab=0.6)
plot(hclust(d, method="average"), labels=labs, col="orange", hang=.1,
	main="Average Linkage", xlab="", sub="", ylab="", cex.main=1.5, cex.lab=0.8)
plot(hclust(d, method="single"), labels=labs, col="blue", hang=.1,
	main="Single Linkage", xlab="", sub="", ylab="", cex.main=1.5, cex.lab=0.8)
dev.off()

pdf('hclustex.pdf', height=4, width=9)
par(mfrow=c(1,3), mar=c(0,2,4,0))
plot(hclust(d), labels=FALSE, col="brown", main="Complete Linkage", hang=1.5,
	xlab="", sub="", ylab="", cex.main=1.5, cex.lab=0.6)
plot(hclust(d, method="average"), labels=FALSE, col="orange", hang=1.5,
	main="Average Linkage", xlab="", sub="", ylab="", cex.main=1.5, cex.lab=0.8)
plot(hclust(d, method="single"), labels=FALSE, col="SkyBlue", hang=1.5,
	main="Single Linkage", xlab="", sub="", ylab="", cex.main=1.5, cex.lab=0.8)
dev.off()

# getting the clusters
library(sparcl)

hca = hclust(d)

# by giving the number of clusters
hct = cutree(hca, k=9)
ColorDendrogram(hca, y=hct, branchlength=23, labels=labs)

# from the tree height
hct = cutree(hca, h=130)
ColorDendrogram(hca, y=hct, branchlength=23, labels=labs)

par(mfrow=c(1,1))
# getting the clusters by mouse click
plot(hca, labels=labs, hang=-.1, xlab="", sub="", ylab="", cex.main=1.5, cex.lab=0.6)
(hcs = identify(hca))

?rect.hclust


##
##K-means clustering
##
# a 2-dimensional example
x <- rbind(matrix(rnorm(100, sd = 0.5), ncol = 2),
           matrix(rnorm(100, mean = 1, sd = 0.5), ncol = 2))
colnames(x) <- c("x", "y")

par(mfrow=c(1,2))
par(mar=c(4.5,4.5,1,1))
plot(x, col = c(rep(1,50),rep(2,50)))
points(matrix(c(1,0,1,0),2,2), col = 1:2, pch = 8, cex=2)

cl <- kmeans(x, 2, nstart=10)
plot(x, col = cl$cluster)
points(cl$centers, col = cl$cluster, pch = 8, cex=2)

cl <- kmeans(x, matrix(c(1,0,-1,0),2,2), nstart=1)
plot(x, col = cl$cluster)
points(cl$centers, col = cl$cluster, pch = 8, cex=2)



K <- 10
wss <- numeric(K)
bss <- numeric(K)
tss <- numeric(K)
for(k in 1:K){
	cl <- kmeans(x, k, nstart = 25)
	tss[k] <- cl$totss
	wss[k] <- cl$tot.withinss
	bss[k] <- cl$betweenss
}

par(mar=c(4.5,4.5,1,1))
plot(1:K, wss, type='b', lty=2, lwd=2, col=2, ylab='Sum of Squares', 
	xlab='Number of clusters', ylim=c(min(c(bss, wss)), max(c(bss, wss))))
#lines(1:K, bss, lty=4, lwd=2, col=4)
legend('right', c('WSS'), col=c(2), lwd=2, lty=c(2), bty='n', pch=1)
arrows(x0=3,y0=30,x1=2.1,y1=19, col=3, lwd=2)
text(x=4,y=33,'Optimal Number of Clusters', col=3)

par(mar=c(4.5,4.5,1,1))
plot(1:K, wss, type='b', lty=2, lwd=2, col=2, ylab='Sum of Squares', 
	xlab='Number of clusters', ylim=c(min(c(bss, wss)), max(c(bss, wss))))
lines(1:K, bss, type='b', lty=4, lwd=2, col=4, pch=2)
legend('right', c('WSS','BSS'), col=c(2,4), lwd=2, lty=c(2,4), pch=c(1,2), bty='n')

##
## K-means clustering for the NCI data
##
K <- 10
wss <- numeric(K)
bss <- numeric(K)
tss <- numeric(K)
for(k in 1:K){
	cl <- kmeans(dat, k, nstart = 25)
	tss[k] <- cl$totss
	wss[k] <- cl$tot.withinss
	bss[k] <- cl$betweenss
}

par(mar=c(4.5,4.5,1,1))
plot(1:K, wss, type='b', lty=1, lwd=2, col=2, ylab='Sum of Squares', 
	xlab='Number of clusters')

K <- 20
wss <- numeric(K)
bss <- numeric(K)
tss <- numeric(K)
for(k in 1:K){
	cl <- kmeans(dat, k, nstart = 25)
	tss[k] <- cl$totss
	wss[k] <- cl$tot.withinss
	bss[k] <- cl$betweenss
}

par(mar=c(4.5,4.5,2,1))
plot(1:K, wss, type='b', lty=1, lwd=2, col=2, ylab='Sum of Squares', 
	xlab='Number of clusters')

