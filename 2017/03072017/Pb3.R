setwd("C:/Users/Giuseppe/Documents/.magistrale/secondo anno/AppStat/Exams/2017/20170703/20170703")


library(mvtnorm)
library(MVN)
library(rgl)
library(car)
library(dbscan)
library(cluster)
library(fields)
data <- read.table("geisha.txt",header=T)
head(data)
dim(data)
plot(data,pch=19)


# stabilize the ordering of the data
misc <- sample(dim(data)[1])
data <- data[misc,]
data.e <- dist(data, method='euclidean')
data.es <- hclust(data.e, method='single')
  


# plot dendrograms
{
  plot(data.es, main='euclidean-single', hang=-0.1, xlab='', labels=F, cex=0.6, sub='')
  par(mfrow=c(1,1))
  
  plot(data.es, main='euclidean-single', hang=-0.1, xlab='', labels=F, cex=0.6, sub='')
  rect.hclust(data.es, k=2)
  
  cluster.es <- cutree(data.es, 2)
}

# Size of clusters
{
  length(which(cluster.es ==1))
  length(which(cluster.es ==2))
}


# Centroid of the clusters
{
  colMeans(data[cluster.es == 1, ])
  colMeans(data[cluster.es == 2, ])
  
  plot(data, col=cluster.es +1, pch=19, main='Single linkage')
  points(colMeans(data[cluster.es == 1, ])[1],colMeans(data[cluster.es == 1, ])[2], pch=17,col="black")
  points(colMeans(data[cluster.es== 2, ])[1],colMeans(data[cluster.es == 2, ])[2], pch=17,col="black")
  
}

# chaining effect due to single linkage -> use ward
data.ew <- hclust(data.e, method='ward.D2')
plot(data.ew, main='euclidean-ward', hang=-0.1, xlab='', labels=F, cex=0.6, sub='')
par(mfrow=c(1,1))

plot(data.ew, main='euclidean-ward', hang=-0.1, xlab='', labels=F, cex=0.6, sub='')
rect.hclust(data.ew, k=2)

cluster.ew <- cutree(data.ew, 2)


# Size of clusters
{
  length(which(cluster.ew ==1))
  length(which(cluster.ew ==2))
} # way better now


# Centroid of the clusters
{
  colMeans(data[cluster.ew == 1, ])
  colMeans(data[cluster.ew == 2, ])
  
  plot(data, col=cluster.ew +1, pch=19, main='Ward linkage')
  points(colMeans(data[cluster.ew == 1, ])[1],colMeans(data[cluster.ew == 1, ])[2], pch=17,col="black")
  points(colMeans(data[cluster.ew== 2, ])[1],colMeans(data[cluster.ew == 2, ])[2], pch=17,col="black")
}

# successful are group 1 (from  assingment)
data1 <- data[cluster.ew ==1,]
data2 <- data[cluster.ew ==2,]

alpha   <- .1
g = 2 
  
k <- 4 # add *2 if also variance and if asks each feature
# add accorgingly to how many intervals he asks

# Bonferroni for successful tours
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
# Bonferroni for mean difference between successful and not successfult tours
# Bonferroni for the difference of the means
{
  n1 <- dim(data1)[1]
  n2 <- dim(data2)[1]
  cfr.t <- qt(1-alpha/(2*k),n1+n2-2)
  
  data1.mean   <- sapply(data1, mean) 
  data2.mean   <- sapply(data2, mean) 
  data1.cov <- cov(data1)
  data2.cov <- cov(data2)
  Sp        <- ((n1 - 1) * data1.cov + (n2 - 1) * data2.cov) / (n1 + n2 - 2)
  Bf1 <- cbind(inf    = data1.mean[1]-data2.mean[1] - cfr.t*sqrt(Sp[1,1]*(1/n1+1/n2)),
               center = data1.mean[1]-data2.mean[1],
               sup    = data1.mean[1]-data2.mean[1] + cfr.t*sqrt(Sp[1,1]*(1/n1+1/n2)))
  Bf2 <- cbind(inf    = data1.mean[2]-data2.mean[2] - cfr.t*sqrt(Sp[2,2]*(1/n1+1/n2)),
               center = data1.mean[2]-data2.mean[2],
               sup    = data1.mean[2]-data2.mean[2] + cfr.t*sqrt(Sp[2,2]*(1/n1+1/n2)))
  Bf <- rbind(Bf1, Bf2)
  dimnames(Bf)[[2]] <- c('inf','center', 'sup')    
  Bf
}

# successful tours last more and starts in earlier hours than unsuccessful ones




