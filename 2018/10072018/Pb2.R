setwd("C:/Users/Giuseppe/Documents/.magistrale/secondo anno/AppStat/Exams/2018/20180710/20180710")

library(mvtnorm)
library(MVN)
library(rgl)
library(car)
library(dbscan)
library(cluster)
library(fields)

data <- read.table('Running.txt', header = T)

head(data)
dim(data)

# stabilize the ordering of the data
misc <- sample(dim(data)[1])
data <- data[misc,]





# Dissimilarity matrix with a metric (euclidean,...)
{
  data.e <- dist(data, method='euclidean')
  
  
  
}

# Linkage:
{
  data.es <- hclust(data.e, method='single')
}

plot(data[,1:2])
# plot dendrograms
{
  # Is there chaining effect for single linkage? Adding single points to clusters iteratively
  
  
  # The clearer the clustering structure in the data, the higher separations between merges
  # how many cluster do you want? fixed for k=2
  # At parity of heigth, consider how balanced the clusters are
  
  plot(data.es, main='euclidean-single', hang=-0.1, xlab='', labels=F, cex=0.6, sub='')
  rect.hclust(data.es, k=2)
  par(mfrow=c(1,1))
}

# Cut the dendrogram
{
  cluster.es <- cutree(data.es, k=2) # euclidean-single
}
plot(data,col=as.factor(cluster.es),pch=19)

# problem of chaining effect, a cluster is a single point!



# a better strategy for this dataset may be average or ward, let's try with average

# indeed
data.ea <- hclust(data.e, method='average')
plot(data.ea, main='euclidean-average', hang=-0.1, xlab='', labels=F, cex=0.6, sub='')
rect.hclust(data.ea, k=2)
par(mfrow=c(1,1))
cluster.ea <- cutree(data.ea, k=2)



# Size of clusters
{
  length(which(cluster.ea ==1))# 49
  length(which(cluster.ea ==2))# 31
}
# more balanced


# Centroid of the clusters
{
  colMeans(data[cluster.ea == 1, ]) # 40.35102          23.96327 
  colMeans(data[cluster.ea == 2, ]) # 27.03742          15.18903
  
  plot(data, col=cluster.ea +1, pch=19, main='Complete linkage')
  points(colMeans(data[cluster.ea == 1, ])[1],colMeans(data[cluster.ea == 1, ])[2], pch=17,col="black")
  points(colMeans(data[cluster.ea == 2, ])[1],colMeans(data[cluster.ea == 2, ])[2], pch=17,col="black")
  
}




# Bonferroni for running cluster -> those with times lower -> cluster2
{
  data1 <- data[cluster.ea==1,]
  data2 <- data[cluster.ea==2,]
  alpha   <- .05
  g =  1# number of clusters (3 here)
  p = dim(data)[2]
    k <- g*p*2 # = 4
 

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
    ICvar2 <- cbind(inf=diag(D.cov)*(n-1) / qchisq(1 - alpha/(2*k), n-1),
                   center=diag(D.cov),
                   sup=diag(D.cov)*(n-1) / qchisq(alpha/(2*k), n-1))
    
    ICvar2
  }
  
  Bf2
  ICvar2  
  res2= mvn(data2)
  res2$multivariateNormality
}





