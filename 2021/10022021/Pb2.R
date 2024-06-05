setwd("C:/Users/Giuseppe/Documents/.magistrale/secondo anno/AppStat/Exams/2021/20210210/20210210")

library(mvtnorm)
library(MVN)
library(rgl)
library(car)
library(dbscan)
library(cluster)
library(fields)
data <- read.table("rice.txt",header=T)
head(data)
dim(data)
plot(data, pch=19)

# stabilize the ordering of the data
misc <- sample(dim(data)[1])
data <- data[misc,]
data.e <- dist(data, method='euclidean')
data.ec <- hclust(data.e, method='complete')


# plot dendrograms
{
  plot(data.ec, main='euclidean-complete', hang=-0.1, xlab='', labels=F, cex=0.6, sub='')
  rect.hclust(data.ec, k=2)
}

# Cut the dendrogram: generate labels for each point
{
  cluster.ec <- cutree(data.ec, k=2) 
}
# complete linkage merge based on the farest pair of points of the 2 clusters
#               will try always to get spherical clusters
# The issue with this linkage is that will merge also clusters that are separated, but less
# wrt to the other cluster
# Here clusters are not well balanced, and have different shapes
# (see the biggest cloud on top and the other smaller two)
# Plot the clusters 
{
  plot(data, col=cluster.ec +1, pch=19, main='Complete linkage')
}

# A better strategy is to use average linkage: merge based on the average distances of the 2 clusters

data.ea <- hclust(data.e, method='average')
plot(data.ea, main='euclidean-average', hang=-0.1, xlab='', labels=F, cex=0.6, sub='')
rect.hclust(data.ea, k=3)
cluster.ea <- cutree(data.ea, k=3) 
plot(data, col=cluster.ea +1, pch=19, main='Average linkage')

# Size of clusters
{
  length(which(cluster.ea ==1)) #42
  length(which(cluster.ea ==2)) # 21
  length(which(cluster.ea ==3)) #18
}
# Centroid of the clusters
{
  colMeans(data[cluster.ea == 1, ])
  colMeans(data[cluster.ea == 2, ])
  colMeans(data[cluster.ea == 3, ])
 # major_axis eccentricity 
 # 0.6205476    0.9041190 
 # 0.7118571    0.7036667 
 # 0.4312778    0.7709444 
  
  plot(data, col=cluster.ea +1, pch=19, main='Complete linkage')
  points(colMeans(data[cluster.ea == 1, ])[1],colMeans(data[cluster.ea == 1, ])[2], pch=17,col="black")
  points(colMeans(data[cluster.ea == 2, ])[1],colMeans(data[cluster.ea == 2, ])[2], pch=17,col="black")
  points(colMeans(data[cluster.ea == 3, ])[1],colMeans(data[cluster.ea == 3, ])[2], pch=17,col="black")
  
}


# Bonferroni for each cluster
{
  
  data1 <- data[cluster.ea==1,]
  data2 <- data[cluster.ea==2,]
  data3 <- data[cluster.ea==3,]
  alpha   <- .05
  g = 3 # number of clusters
  k <- 2*g # add *2 if also variance 
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
    ICvar <- cbind(inf=diag(D.cov)*(n-1) / qchisq(1 - alpha/(2*k), n-1),
                   center=diag(D.cov),
                   sup=diag(D.cov)*(n-1) / qchisq(alpha/(2*k), n-1))
    # Bonferroni region defined by the cartesian product of the Bf intervals
    Bf1 <- rbind(IC.BF.feature_1,ICvar[1,])
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
    ICvar <- cbind(inf=diag(D.cov)*(n-1) / qchisq(1 - alpha/(2*k), n-1),
                   center=diag(D.cov),
                   sup=diag(D.cov)*(n-1) / qchisq(alpha/(2*k), n-1))
    # Bonferroni region defined by the cartesian product of the Bf intervals
    Bf2 <- rbind(IC.BF.feature_1, ICvar[1,])
    dimnames(Bf2)[[2]] <- c('inf','center','sup')
    Bf2
  }
  # data3
  {
    D <- data3
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
    ICvar <- cbind(inf=diag(D.cov)*(n-1) / qchisq(1 - alpha/(2*k), n-1),
                   center=diag(D.cov),
                   sup=diag(D.cov)*(n-1) / qchisq(alpha/(2*k), n-1))
    # Bonferroni region defined by the cartesian product of the Bf intervals
    Bf3 <- rbind(IC.BF.feature_1, ICvar[1,])
    dimnames(Bf2)[[2]] <- c('inf','center','sup')
    Bf3
  }
  
  Bf1
  Bf2
  Bf3
  # >   Bf1
  # inf    center        sup
  # IC.BF.feature_1 0.578068914 0.6205476 0.66302632
  # var             0.005872386 0.0098604 0.01919633
  # >   Bf2
  # inf      center         sup
  # IC.BF.feature_1 0.6898297338 0.711857143 0.733884552
  # var             0.0005855364 0.001189229 0.003286645
  # >   Bf3
  # major_axis  major_axis  major_axis
  # IC.BF.feature_1 0.4083724058 0.431277778 0.454183150
  # var             0.0004965239 0.001060565 0.003261042
  
  res1= mvn(data1)
  res1$multivariateNormality
  
  res2= mvn(data2)
  res2$multivariateNormality
  
  res3= mvn(data3)
  res3$multivariateNormality
}


