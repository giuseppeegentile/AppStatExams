library(mvtnorm)
library(MVN)
library(rgl)
library(car)
library(dbscan)
library(cluster)
library(fields)

data <- read.table("sharks.txt", header = TRUE)
head(data)

plot(data)
# we see a clear division in 4 clusters

# the exam ask to perform a hierarchical clustering with euclidean distance and complete linkage
dist.e <- dist(data, method = 'euclidean')

sharks.ea <- hclust(dist.e, method='average')

plot(sharks.ea)
rect.hclust(sharks.ea, k=4)

# we see 4 clusters creating rapidly.
# We could argue for 2, but from the visualization of the data i would argue they are 4.

cluster.ea <- cutree(sharks.ea,k=4)
plot(data, col=cluster.ea +1)
# calculate the centroids, and the size of the clusters
cluster1.size <- length(which(cluster.ea==1))
cluster2.size <- length(which(cluster.ea==2))
cluster3.size <- length(which(cluster.ea==3))
cluster4.size <- length(which(cluster.ea==4))

centroid1 <- sapply(data[cluster.ea==1,],mean)
centroid2 <- sapply(data[cluster.ea==2,],mean)
centroid3 <- sapply(data[cluster.ea==3,],mean)
centroid4 <- sapply(data[cluster.ea==4,],mean)


plot(data, col = cluster.ea +1)
points(centroid1[1],centroid1[2],col=2, pch=3)
points(centroid2[1],centroid2[2],col=3, pch=3)
points(centroid3[1],centroid3[2],col=4, pch=3)
points(centroid4[1],centroid4[2],col=5, pch=3)

# Bonferroni confidence intervals for the body circumference
# 4 groups
alpha <- 0.10
k <- 4*2
n1 <- dim(data[cluster.ea==1,])[1]
n2 <- dim(data[cluster.ea==2,])[1]
n3 <- dim(data[cluster.ea==3,])[1]
n4 <- dim(data[cluster.ea==4,])[1]


bodycirc <- data[,2]

quantile1 <- qt(1-alpha/(2*k), n1-1)
quantile2 <- qt(1-alpha/(2*k), n2-1)
quantile3 <- qt(1-alpha/(2*k), n3-1)
quantile4 <- qt(1-alpha/(2*k), n4-1)

# gaussianity assumption
shapiro.test(bodycirc[cluster.ea==1])
shapiro.test(bodycirc[cluster.ea==2])
shapiro.test(bodycirc[cluster.ea==3])
shapiro.test(bodycirc[cluster.ea==4])

m1 <- mean(bodycirc[cluster.ea==1])
m2 <- mean(bodycirc[cluster.ea==2])
m3 <- mean(bodycirc[cluster.ea==3])
m4 <- mean(bodycirc[cluster.ea==4])

var1 <- var(bodycirc[cluster.ea==1])
var2 <- var(bodycirc[cluster.ea==2])
var3 <- var(bodycirc[cluster.ea==3])
var4 <- var(bodycirc[cluster.ea==4])

BC1.mean <- c(m1 - quantile1 * sqrt(var1/n1),
              m1 + quantile1 * sqrt(var1/n1))
BC2.mean <- c(m2 - quantile2 * sqrt(var2/n2),
              m2 + quantile2 * sqrt(var2/n2))
BC3.mean <- c(m3 - quantile3 * sqrt(var3/n3),
              m3 + quantile3 * sqrt(var3/n3))
BC4.mean <- c(m4 - quantile4 * sqrt(var4/n4),
              m4 + quantile4 * sqrt(var4/n4))

BC1.var <- c(var1/(n1-1)/ qchisq(1 - alpha/(2*k), n1-1),
             var1/(n1-1)/ qchisq(alpha/(2*k), n1-1))
BC2.var <- c(var2/(n2-1)/ qchisq(1 - alpha/(2*k), n2-1),
             var2/(n2-1)/ qchisq(alpha/(2*k), n2-1))
BC3.var <- c(var3/(n3-1)/ qchisq(1 - alpha/(2*k), n3-1),
             var3/(n3-1)/ qchisq(alpha/(2*k), n3-1))
BC4.var <- c(var4/(n4-1)/ qchisq(1 - alpha/(2*k), n4-1),
             var4/(n4-1)/ qchisq(alpha/(2*k), n4-1))

BC.mean <- rbind(BC1.mean, BC2.mean, BC3.mean, BC4.mean)
BC.mean


BC.var <- rbind(BC1.var, BC2.var, BC3.var, BC4.var)
BC.var


