setwd("C:/Users/Giuseppe/Documents/.magistrale/secondo anno/AppStat/Exams/2019/20190117/20190117")
options(rgl.printRglwidget = TRUE)

library(mvtnorm)
library(MVN)
library(rgl)
library(car)
library(dbscan)
library(cluster)
library(fields)
data <- read.table("diamonds.txt",header=T)
head(data)
dim(data)


boxplot(data)
data <- scale(data)
data <- as.data.frame(data)
boxplot(data)
# we can assume 2 clusters
plot(data,pch=19)


# stabilize the ordering of the data
misc <- sample(dim(data)[1])
data <- data[misc,]
data.e <- dist(data, method='euclidean')
data.ew <- hclust(data.e, method='ward.D2')


# plot dendrograms
{
  plot(data.ew, main='euclidean-ward', hang=-0.1, xlab='', labels=F, cex=0.6, sub='')
  par(mfrow=c(1,1))
  
  # Is there chaining effect for single linkage? Adding single points to clusters iteratively
  
  
  # The clearer the clustering structure in the data, the higher separations between merges
  # how many cluster do you want? fixed for k=2
  # At parity of heigth, consider how balanced the clusters are
  

  plot(data.ew, main='euclidean-ward', hang=-0.1, xlab='', labels=F, cex=0.6, sub='')
  rect.hclust(data.ew, k=2)
  par(mfrow=c(1,1))
}

# Cut the dendrogram: generate labels for each point
{
  cluster.ew <- cutree(data.ew, k=2) # euclidean-ward
}
plot(data,col=cluster.ew+1,pch=19)

# 297 of a type and 38 of another:
{
  table(cluster.ew)
}
38*100/dim(data)[1]
297*100/dim(data)[1]


data1 <- data[cluster.ew==1,]
data2 <- data[cluster.ew==2,]
alpha   <- .05
g = 2 # number of clusters 
p <- dim(data)[2]

k <- p*g*(g-1)/2 # difference of only two groups
# or g*(g-1)/2


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









