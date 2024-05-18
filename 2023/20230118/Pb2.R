library(mvtnorm)
library(MVN)
library(rgl)
library(car)
library(dbscan)
library(cluster)
library(fields)

setwd("C:/Users/Giuseppe/Documents/.magistrale/secondo anno/AppStat/Exams/2023/20230118/20230118")
data <- read.table("beers.txt",header=T)

head(data)
plot(data, pch=19)

data <- scale(data)
data <- as.data.frame(data)
plot(data, pch=19)

# Fix the ordering of the data
misc <- sample(dim(data)[1])
data <- data[misc,]
data.e <- dist(data, method='euclidean')


data.es <- hclust(data.e, method='single')
data.ec <- hclust(data.e, method='complete')



par(mfrow=c(1,2))
plot(data.es, main='euclidean-single', hang=-0.1, xlab='', labels=F, cex=0.6, sub='')
plot(data.ec, main='euclidean-complete', hang=-0.1, xlab='', labels=F, cex=0.6, sub='')
par(mfrow=c(1,1))

# b)
# with single linkage we have the chain effect (Adding single points to clusters iteratively).
# Also, is not clear the clustering  structure.
# The chaining occours because merge based on the single point distance of the other cluster
# when all points have comparable distances, is not an appropriate linkage

# With complete, instead, is clear that there are 2 clusters, since there is high separation
# between merges
plot(data.ec, main='euclidean-complete', hang=-0.1, xlab='', labels=F, cex=0.6, sub='')
rect.hclust(data.ec, k=2)




# Cut the dendrogram: generate labels for each point
{
  cluster.ec <- cutree(data.ec, k=2) # euclidean-complete:
  
}

# If true labels NOT provided: show the labels created by the cluster and numbers
{
  table(cluster.ec)
}


# Plot the clusters
{
  plot(data, col=ifelse(cluster.ec==1,'red','blue'), pch=19, main='Complete linkage')
  
  
  # instead with single, we'd have a single point as other cluster: 
  cluster.es <- cutree(data.es, k=2) # euclidean-complete:
  plot(data, col=ifelse(cluster.es==1,'red','blue'), pch=19, main='Complete linkage')
}
# compute the cophenetic matrices 
{
  coph.ec <- cophenetic(data.ec)
}

# compute cophenetic coefficients
{
  ec <- cor(data.e, coph.ec)
  ec
}

# Confusion matrix with ordered structure
{
  image(as.matrix(data.e)[data.ec$order,data.ec$order], asp=1, main='reordered euclidean complete' )
}


# Size of clusters
length(which(cluster.ec ==1))
length(which(cluster.ec ==2))


# Centroid of the clusters
colMeans(data[cluster.ec == 1, ])
colMeans(data[cluster.ec == 2, ])


plot(data, col=ifelse(cluster.ec==1,'red','blue'), pch=20, main='Complete linkage')
points(colMeans(data[cluster.ec == 1, ])[1],colMeans(data[cluster.ec == 1, ])[2], pch=17,col="black")
points(colMeans(data[cluster.ec == 2, ])[1],colMeans(data[cluster.ec == 2, ])[2], pch=17,col="black")




