library(car)
options(rgl.printRglwidget = TRUE)

library(mvtnorm)
library(MVN)
library(rgl)
library(car)
library(dbscan)
library(cluster)
library(fields)
setwd("C:/Users/Giuseppe/Documents/.magistrale/secondo anno/AppStat/Exams/2024/20240117/20240117")
data <- read.table("sharks.txt",header=T)
plot(data,pch=19)
#very likely 4 clusters

# stabilize the ordering of the data
misc <- sample(dim(data)[1])
data <- data[misc,]

data.e <- dist(data, method='euclidean')
data.ea <- hclust(data.e, method='average')


par(mfrow=c(1,2))
plot(data.ea, main='euclidean-average', hang=-0.1, xlab='', labels=F, cex=0.6, sub='')
# 2 or 4 clusters are appropriate
rect.hclust(data.ea, k=4)

plot(data.ea, main='euclidean-average', hang=-0.1, xlab='', labels=F, cex=0.6, sub='')
rect.hclust(data.ea, k=2)

# due to the initial representation on the plot i prefer to use 4 clusters


cluster.ea <- cutree(data.ea, k=4) # euclidean-average

plot(data, col=cluster.ea +1, pch=19, main='Average linkage')

# Size of clusters
{
  length(which(cluster.ea ==1))
  length(which(cluster.ea ==2))
  length(which(cluster.ea ==3))
  length(which(cluster.ea ==4))
}
# they are balanced

# Centroid of the clusters
{
  colMeans(data[cluster.ea == 1, ])
  colMeans(data[cluster.ea == 2, ])
  colMeans(data[cluster.ea == 3, ])
  colMeans(data[cluster.ea == 4, ])
  
  
  plot(data, col=cluster.ea+1, pch=20, main='Complete linkage')
  points(colMeans(data[cluster.ea == 1, ])[1],colMeans(data[cluster.ea == 1, ])[2], pch=17,col="black")
  points(colMeans(data[cluster.ea == 2, ])[1],colMeans(data[cluster.ea == 2, ])[2], pch=17,col="black")
  points(colMeans(data[cluster.ea == 3, ])[1],colMeans(data[cluster.ea == 3, ])[2], pch=17,col="black")
  points(colMeans(data[cluster.ea == 4, ])[1],colMeans(data[cluster.ea == 4, ])[2], pch=17,col="black")
  
}


data1 <- data[cluster.ea ==1, ]
data2 <- data[cluster.ea ==2, ]
data3 <- data[cluster.ea ==3, ]
data4 <- data[cluster.ea ==4, ]


#cluster 1
{
  {
    p <- dim(data1)[2]
    n <- dim(data1)[1]
    D.mean <- sapply(data1,mean)  
    D.cov <- cov(data1)
  }
  
  
  alpha = 0.1
  k <- 2 # only mean body circ. and variance
  cfr.t <- qt(1-alpha/(2*k), n-1) # Student quantile
  
  
  # Bonferroni confidence intervals in the direction of DBOD and DSS
  IC.BF.feature_1 <- c(D.mean[1] - cfr.t*sqrt(D.cov[1,1]/n),
                       D.mean[1],
                       D.mean[1] + cfr.t*sqrt(D.cov[1,1]/n))
  
  # Bonferroni region defined by the cartesian product of the Bf intervals
  Bf <- rbind(IC.BF.feature_1)
  dimnames(Bf)[[2]] <- c('inf','center','sup')
  Bf
  ICvar <- cbind(inf=(D.cov[1,1])*(n-1) / qchisq(1 - alpha/(2*k), n-1),
                 center=(D.cov[1,1]),
                 sup=(D.cov[1,1])*(n-1) / qchisq(alpha/(2*k), n-1))
  
  ICvar
}
#cluster 2
{
  {
    p <- dim(data2)[2]
    n <- dim(data2)[1]
    D.mean <- sapply(data2,mean)  
    D.cov <- cov(data2)
  }
  
  
  alpha = 0.1
  k <- 2 # only mean body circ. and variance
  cfr.t <- qt(1-alpha/(2*k), n-1) # Student quantile
  
  
  # Bonferroni confidence intervals in the direction of DBOD and DSS
  IC.BF.feature_1 <- c(D.mean[1] - cfr.t*sqrt(D.cov[1,1]/n),
                       D.mean[1],
                       D.mean[1] + cfr.t*sqrt(D.cov[1,1]/n))
  
  # Bonferroni region defined by the cartesian product of the Bf intervals
  Bf <- rbind(IC.BF.feature_1)
  dimnames(Bf)[[2]] <- c('inf','center','sup')
  Bf
  ICvar <- cbind(inf=(D.cov[1,1])*(n-1) / qchisq(1 - alpha/(2*k), n-1),
                 center=(D.cov[1,1]),
                 sup=(D.cov[1,1])*(n-1) / qchisq(alpha/(2*k), n-1))
  
  ICvar
}

#cluster 3
{
  {
    p <- dim(data3)[2]
    n <- dim(data3)[1]
    D.mean <- sapply(data3,mean)  
    D.cov <- cov(data3)
  }
  
  
  alpha = 0.1
  k <- 2 # only mean body circ. and variance
  cfr.t <- qt(1-alpha/(2*k), n-1) # Student quantile
  
  
  # Bonferroni confidence intervals in the direction of DBOD and DSS
  IC.BF.feature_1 <- c(D.mean[1] - cfr.t*sqrt(D.cov[1,1]/n),
                       D.mean[1],
                       D.mean[1] + cfr.t*sqrt(D.cov[1,1]/n))
  
  # Bonferroni region defined by the cartesian product of the Bf intervals
  Bf <- rbind(IC.BF.feature_1)
  dimnames(Bf)[[2]] <- c('inf','center','sup')
  Bf
  ICvar <- cbind(inf=(D.cov[1,1])*(n-1) / qchisq(1 - alpha/(2*k), n-1),
                 center=(D.cov[1,1]),
                 sup=(D.cov[1,1])*(n-1) / qchisq(alpha/(2*k), n-1))
  
  ICvar
}

#cluster 4
{
  {
    p <- dim(data4)[2]
    n <- dim(data4)[1]
    D.mean <- sapply(data4,mean)  
    D.cov <- cov(data4)
  }
  
  
  alpha = 0.1
  k <- 2 # only mean body circ. and variance
  cfr.t <- qt(1-alpha/(2*k), n-1) # Student quantile
  
  
  # Bonferroni confidence intervals in the direction of DBOD and DSS
  IC.BF.feature_1 <- c(D.mean[1] - cfr.t*sqrt(D.cov[1,1]/n),
                       D.mean[1],
                       D.mean[1] + cfr.t*sqrt(D.cov[1,1]/n))

  # Bonferroni region defined by the cartesian product of the Bf intervals
  Bf <- rbind(IC.BF.feature_1)
  dimnames(Bf)[[2]] <- c('inf','center','sup')
  Bf
  ICvar <- cbind(inf=(D.cov[1,1])*(n-1) / qchisq(1 - alpha/(2*k), n-1),
                 center=(D.cov[1,1]),
                 sup=(D.cov[1,1])*(n-1) / qchisq(alpha/(2*k), n-1))
  
  ICvar
}

 

