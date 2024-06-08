library(mvtnorm)
library(MVN)
library(rgl)
library(car)
library(dbscan)
library(cluster)
library(fields)

data <- read.table("kapok.txt", header=TRUE)

plot(data)
# we see a division in 3 or 4 clusters

distance <-dist(data, method="euclidean") 

data.ew <- hclust(distance, method="ward.D2")

plot(data.ew)
# i would say that there are 3 clusters

clusters <- cutree(data.ew, k=3)

plot(data, col=clusters)

# not convinced, let's try 4
clusters <- cutree(data.ew, k=4)
plot(data, col=clusters)
# more convinced

size1<-length(which(clusters==1))
size2<-length(which(clusters==2))
size3<-length(which(clusters==3))
size4<-length(which(clusters==4))

sizes <- c(size1,size2,size3,size4)
sizes

centroid1 <- colMeans(data[which(clusters==1),])
centroid2 <- colMeans(data[which(clusters==2),])
centroid3 <- colMeans(data[which(clusters==3),])
centroid4 <- colMeans(data[which(clusters==4),])

# assumptions of bonferroni:
# gaussianity of the samples

data1 <- data[which(clusters==1),]
data2 <- data[which(clusters==2),]
data3 <- data[which(clusters==3),]
data4 <- data[which(clusters==4),]

ps <- c(mvn(data1)$multivariateNormality$'p value',
        mvn(data2)$multivariateNormality$'p value',
        mvn(data3)$multivariateNormality$'p value',
        mvn(data4)$multivariateNormality$'p value')
ps

# we cannot assume gaussianity for the 4th cluster.
# We still calculate all the confidence intervals
alpha <- 0.1
k <- p*4 + p*4 # 2 components in the mean, 4 groups, plus 2 variances per groups
n1 <- size1
n2 <- size2
n3 <- size3
n4 <- size4
S1 <- cov(data1)
S2 <- cov(data2)
S3 <- cov(data3)
S4 <- cov(data4)

p <- 2

quantile1 <- qt(1-alpha/(2*k), n1-1) 
quantile2 <- qt(1-alpha/(2*k), n2-1) 
quantile3 <- qt(1-alpha/(2*k), n3-1) 
quantile4 <- qt(1-alpha/(2*k), n4-1) 

BC1.m <- cbind(centroid1 - quantile1*sqrt(diag(S1)/n1),
           centroid1 + quantile1*sqrt(diag(S2)/n1))
BC1.m
BC2.m <- cbind(centroid2 - quantile2*sqrt(diag(S2)/n2),
         centroid2 + quantile2*sqrt(diag(S2)/n2))
BC2.m
BC3.m <- cbind(centroid3 - quantile3*sqrt(diag(S3)/n3),
         centroid3 + quantile3*sqrt(diag(S3)/n3))
BC3.m
BC4.m <- cbind(centroid2 - quantile2*sqrt(diag(S4)/n4),
         centroid2 + quantile2*sqrt(diag(S4)/n4))
BC4.m

BC1.var <- cbind((n1-1)*diag(S1)/qchisq(1-alpha/(2*k), n1-1),
                 (n1-1)*diag(S1)/qchisq(alpha/(2*k), n1-1) )
BC1.var
BC2.var<- cbind((n2-1)*diag(S2)/qchisq(1-alpha/(2*k), n2-1),
                (n2-1)*diag(S2)/qchisq(alpha/(2*k), n2-1) )
BC2.var
BC3.var<- cbind((n3-1)*diag(S3)/qchisq(1-alpha/(2*k), n3-1),
                (n3-1)*diag(S3)/qchisq(alpha/(2*k), n3-1) )
BC3.var
BC4.var<- cbind((n4-1)*diag(S4)/qchisq(1-alpha/(2*k), n4-1),
                (n4-1)*diag(S4)/qchisq(alpha/(2*k), n4-1) )
BC4.var




