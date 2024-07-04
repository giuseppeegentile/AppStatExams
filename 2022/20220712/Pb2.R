library(mvtnorm)
library(MVN)
library(rgl)
library(car)
library(dbscan)
library(cluster)
library(fields)
library(MVN)
library(heplots)
library(car)

## HIERARCHICAL CLUSTERING--------------------------------
data <- read.table("demogorgons.txt", header=TRUE)
head(data)

plot(data)

distance <- dist(data, method='euclidean')


hclust.a <- hclust(distance, method='average')

# plot of the dendrograms
plot(hclust.a)

clusters <- cutree(hclust.a, k=2) # euclidean-complete:

# plot of 2 clusters
plot(data, col=ifelse(clusters==1,"red","blue"),pch=19)

values <- data
groups <- factor(clusters)

fit <- manova(as.matrix(values) ~ groups)
summary.manova(fit)
#assumptions
mvn(data[which(groups==1),])$multivariateNormality
mvn(data[which(groups==2),])$multivariateNormality
# very gaussian
cov1 <- cov(data[which(groups==1),])
cov2 <- cov(data[which(groups==2),])
cov1/cov2

summary(boxM(data,groups))
# assumptions verified


# yes there is clear evidence


centroid1 <- colMeans(data[which(clusters==1),])
centroid2 <- colMeans(data[which(clusters==2),])
centroid1
centroid2

cov1 <- cov(data[which(clusters==1),])
cov2 <- cov(data[which(clusters==2),])

n1 <- dim(data[which(clusters==1),])[1]
n2 <- dim(data[which(clusters==2),])[1]
p <-2

alpha <- 0.05
cfr.fisher1 <- ((n1 - 1) * p / (n1 - p)) * qf(1 - alpha, p, n1 - p)
cfr.fisher2 <- ((n2 - 1) * p / (n2 - p)) * qf(1 - alpha, p, n2 - p)

plot(data)
points(centroid1[1],centroid1[2],col="red",pch=19)
points(centroid2[1],centroid2[2],col="blue",pch=19)
ellipse(centroid1, cov1/n1, sqrt(cfr.fisher1), col = 'red', lty = 2, lwd=2, center.cex=1)
ellipse(centroid2, cov2/n2, sqrt(cfr.fisher1), col = 'blue', lty = 2, lwd=2, center.cex=1)


