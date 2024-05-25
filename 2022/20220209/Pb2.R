setwd("C:/Users/Giuseppe/Documents/.magistrale/secondo anno/AppStat/Exams/2022/20220209/20220209")
data <- read.table("streaming.txt",header = T)

head(data)
dim(data)
plot(data, pch=19)

n <- dim(data)[1]
p <- dim(data)[2]

library(mvtnorm)
library(MVN)
library(rgl)
library(car)
library(dbscan)
library(cluster)
library(fields)



# stabilize the ordering of the data
misc <- sample(dim(data)[1])
data <- data[misc,]




data.e <- dist(data, method='euclidean')
data.es <- hclust(data.e, method='single')


plot(data.es, main='euclidean-single', hang=-0.1, xlab='', labels=F, cex=0.6, sub='')
cluster.es <- cutree(data.es, k=3) # euclidean-single
rect.hclust(data.es, k=3)
# single linkage fail to identify the tirhd small cluster, since look at 
# point to point distance

# Plot the clusters 
{
  plot(data, col=cluster.es +1, pch=19, main='Simple linkage')

}

# Size of clusters
{
  length(which(cluster.es ==1))
  length(which(cluster.es ==2))
  length(which(cluster.es ==3))
}


# Centroid of the clusters
{
  colMeans(data[cluster.es == 1, ])
  colMeans(data[cluster.es == 2, ])
  
  
  plot(data, col=cluster.es+1, pch=20, main='Complete linkage')
  points(colMeans(data[cluster.es == 1, ])[1],colMeans(data[cluster.es == 1, ])[2], pch=17,col="black")
  points(colMeans(data[cluster.es == 2, ])[1],colMeans(data[cluster.es == 2, ])[2], pch=17,col="black")

}
# compute the cophenetic matrices 
{
  coph.es <- cophenetic(data.es)
}
# compute cophenetic coefficients: the higher the better
{
  es <- cor(data.e, coph.es)
  es
}


# c)
data1 <- data[cluster.es==1,]
data2 <- data[cluster.es==2,]
data3 <- data[cluster.es==3,]
alpha   <- .05
g = 3 # number of clusters
k <- g*p # add *2 if also variance 
# data1
{
  D <- data1
  D.mean   <- sapply(D, mean) 
  # if D.mean and delta.0 incompatible -> D.mean   <- colMeans(D)
  D.cov    <- cov(D)
  D.invcov <- solve(D.cov)
  n <- dim(D)[1]
  cfr.t <- qt(1-alpha/(2*k), n-1) # Student quantile
  
  # Bonferroni confidence intervals in the direction of DBOD and DSS
  IC.BF.feature_1 <- c(D.mean[1] - cfr.t*sqrt(D.cov[1,1]/n),
                       D.mean[1],
                       D.mean[1] + cfr.t*sqrt(D.cov[1,1]/n))
  IC.BF.feature_2 <- c(D.mean[2] - cfr.t*sqrt(D.cov[2,2]/n),
                       D.mean[2],
                       D.mean[2] + cfr.t*sqrt(D.cov[2,2]/n))
  # Bonferroni region defined by the cartesian product of the Bf intervals
  Bf1 <- rbind(IC.BF.feature_1, IC.BF.feature_2)
  dimnames(Bf1)[[2]] <- c('inf','center','sup')
  Bf1
}

# data2
{
  D <- data2
  D.mean   <- sapply(D, mean) 
  # if D.mean and delta.0 incompatible -> D.mean   <- colMeans(D)
  D.cov    <- cov(D)
  D.invcov <- solve(D.cov)
  n <- dim(D)[1]
  cfr.t <- qt(1-alpha/(2*k), n-1) # Student quantile
  # Bonferroni confidence intervals in the direction of DBOD and DSS
  IC.BF.feature_1 <- c(D.mean[1] - cfr.t*sqrt(D.cov[1,1]/n),
                       D.mean[1],
                       D.mean[1] + cfr.t*sqrt(D.cov[1,1]/n))
  IC.BF.feature_2 <- c(D.mean[2] - cfr.t*sqrt(D.cov[2,2]/n),
                       D.mean[2],
                       D.mean[2] + cfr.t*sqrt(D.cov[2,2]/n))
  # Bonferroni region defined by the cartesian product of the Bf intervals
  Bf2 <- rbind(IC.BF.feature_1, IC.BF.feature_2)
  dimnames(Bf2)[[2]] <- c('inf','center','sup')
  Bf2
}

# data3
{
  D <- data3
  n <- dim(D)[1]
  D.mean   <- sapply(D, mean) 
  # if D.mean and delta.0 incompatible -> D.mean   <- colMeans(D)
  D.cov    <- cov(D)
  D.invcov <- solve(D.cov)
  cfr.t <- qt(1-alpha/(2*k), n-1) # Student quantile
  
  # Bonferroni confidence intervals in the direction of DBOD and DSS
  IC.BF.feature_1 <- c(D.mean[1] - cfr.t*sqrt(D.cov[1,1]/n),
                       D.mean[1],
                       D.mean[1] + cfr.t*sqrt(D.cov[1,1]/n))
  IC.BF.feature_2 <- c(D.mean[2] - cfr.t*sqrt(D.cov[2,2]/n),
                       D.mean[2],
                       D.mean[2] + cfr.t*sqrt(D.cov[2,2]/n))
  # Bonferroni region defined by the cartesian product of the Bf intervals
  Bf3 <- rbind(IC.BF.feature_1, IC.BF.feature_2)
  dimnames(Bf2)[[2]] <- c('inf','center','sup')
  Bf3
}
Bf1
Bf2
Bf3

res1= mvn(data1)
res1$multivariateNormality

res2= mvn(data2)
res2$multivariateNormality


