library(mvtnorm)
library(MVN)
library(rgl)
library(car)
library(dbscan)
library(cluster)
library(fields)

data <- read.table("beers.txt", header=TRUE)
head(data)

plot(data)

data <- scale(data)
plot(data)


distance <- dist(data, method='euclidean')

clust.s <- hclust(distance, method="single")
clust.c <- hclust(distance, method="complete")

par(mfrow=c(1,2))
plot(clust.s)
plot(clust.c)
par(mfrow=c(1,1))

# i would choose the complete linkage. Singel linkage here exhibits clear signs of chain effect
# in the complete linkage dendrogram instead we have clear distinction between 2 clusters

clusters <- cutree(clust.c,k=2)
clusters

plot(data, col=ifelse(clusters==1,"red","blue"),pch=19)

# size, centroids and cophenetic distance
size1 <- length(clusters==1)
size1
size2 <- length(clusters==2)
size2

centroid1 <- colMeans(data[which(clusters==1),])
centroid2 <- colMeans(data[which(clusters==2),])

points(centroid1[1], centroid1[2], col="yellow",pch=19)
points(centroid2[1], centroid2[2], col="black", pch=19)


cop <- cophenetic(clust.c)
coeff <- cor(distance,cop)
coeff
