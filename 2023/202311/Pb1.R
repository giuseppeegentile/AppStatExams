setwd("C:/Users/Giuseppe/Documents/.magistrale/secondo anno/AppStat/Exams/2023/20231107/20231107")
options(rgl.printRglwidget = TRUE)

library(mvtnorm)
library(MVN)
library(rgl)
library(car)
library(dbscan)
library(cluster)
library(fields)
data <- read.table("kapok.txt",header=T)
head(data)
dim(data)
plot(data, pch=19)



# stabilize the ordering of the data
misc <- sample(dim(data)[1])
data <- data[misc,]
data.e <- dist(data, method='euclidean')

# Linkage:
{

  data.ew <- hclust(data.e, method='ward.D2') # merge clusters that don't increase too much variability
  
}


# plot dendrograms
{
  plot(data.ew, main='euclidean-ward', hang=-0.1, xlab='', labels=F, cex=0.6, sub='')
  rect.hclust(data.ew, k=3)
  par(mfrow=c(1,1))
}



# Cut the dendrogram: generate labels for each point
{

  cluster.ew <- cutree(data.ew, k=3) # euclidean-ward
}
plot(data,pch=19,col=cluster.ew)

# clusters with high mean of both height and diameter are more old, therefore related to past fires
# the smaller the centroids, the closer in time the fire is




# Bonferroni for each cluster
{
  data1 <- data[cluster.ew==1,]
  data2 <- data[cluster.ew==2,]
  data3 <- data[cluster.ew==3,]
  alpha   <- .1
  g =  3# number of clusters (3 here)
  p <- dim(data)[2]
    k <- g*p*2 # variance asked too
  # add accorgingly to how many intervals he asks
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
    ICvar <- cbind(inf=diag(D.cov)*(n-1) / qchisq(1 - alpha/(2*k), n-1),
                   center=diag(D.cov),
                   sup=diag(D.cov)*(n-1) / qchisq(alpha/(2*k), n-1))
    
    ICvar
    Bf1 <- rbind(IC.BF.feature_1, IC.BF.feature_2, ICvar)
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
    ICvar <- cbind(inf=diag(D.cov)*(n-1) / qchisq(1 - alpha/(2*k), n-1),
                   center=diag(D.cov),
                   sup=diag(D.cov)*(n-1) / qchisq(alpha/(2*k), n-1))
    
    ICvar
    # Bonferroni region defined by the cartesian product of the Bf intervals
    Bf2 <- rbind(IC.BF.feature_1, IC.BF.feature_2, ICvar)
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
    IC.BF.feature_2 <- c(D.mean[2] - cfr.t*sqrt(D.cov[2,2]/n),
                         D.mean[2],
                         D.mean[2] + cfr.t*sqrt(D.cov[2,2]/n))
    ICvar <- cbind(inf=diag(D.cov)*(n-1) / qchisq(1 - alpha/(2*k), n-1),
                   center=diag(D.cov),
                   sup=diag(D.cov)*(n-1) / qchisq(alpha/(2*k), n-1))
    
    ICvar
    # Bonferroni region defined by the cartesian product of the Bf intervals
    Bf3 <- rbind(IC.BF.feature_1, IC.BF.feature_2,ICvar)
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
  
  res3= mvn(data3)
  res3$multivariateNormality
}

