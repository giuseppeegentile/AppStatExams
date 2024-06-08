###-----------------------------------###
### Problem 1: Kapok trees (20231107) ###
###-----------------------------------###

rm(list = ls())
graphics.off()

options(rgl.printRglwidget = TRUE)

library(mvtnorm)
library(MVN)
library(rgl)
library(car)
library(dbscan)
library(cluster)
library(fields)

data <- read.table('kapok.txt', header = T)
head(data)

plot(data)


# a) ----------------------------------------------------------------------

HC <- hclust(dist(data, method='euclidean'), method = 'ward.D2')

plot(HC, hang=-0.1, sub='', labels=F, xlab='')

rect.hclust(HC, k=4)

cluster <- cutree(HC, k=4)
table(cluster)

plot(data, col=cluster+1, pch=16, lwd=2)

means <- NULL
for(k in 1:4)
{
  means = rbind(means, sapply(data[cluster==k,],mean))
}

means
points(means, pch=17, cex=1.5)


# Extra -------------------------------------------------------------------


## Cophenetic Coefficient --------------------------------------------------

data.e <- dist(data, method='euclidean')
coph.ew <- cophenetic(HC)

cor(data.e, coph.ew)


# Choice of k (W(k)/SS_tot Plot) ------------------------------------------

w <- NULL
w_i <- NULL
b <- NULL
b_i <- NULL
mean <- colMeans(data)

for(k in 1:10){
  cluster <- cutree(HC, k = k)
  w_i <- NULL
  b_i <- NULL
  means <- NULL
  for(i in 1:length(unique(cluster)))
  {
    means <-  rbind(means, sapply(data[cluster == i, ], mean))
  }
  mean_bar <- colMeans(means)
  for(i in 1:length(unique(cluster)))
  {
    cluster_i_centered_rel <- data[cluster == i, ]
    cluster_i_centered_rel[, 1] <- data[cluster == i, 1] - means[i, 1]
    cluster_i_centered_rel[, 2] <- data[cluster == i, 2] - means[i, 2]
    mean_i_centered <- means[i, ]
    mean_i_centered[1] <- means[i, 1] - mean_bar[1]
    mean_i_centered[2] <- means[i, 2] - mean_bar[2]
    w_i <- c(w_i, sum(cluster_i_centered_rel^2))
    b_i <- c(b_i, table(cluster)[i] * sum(mean_i_centered^2)) 
    # there's something wrong here with b_i (don't use it)
  }
  w <- c(w, sum(w_i))
  b <- c(b, sum(b_i))
}

data_centered <- data
data_centered[, 1] <- data[, 1] - mean[1]
data_centered[, 2] <- data[, 2] - mean[2]

t <- sum(data_centered^2)

matplot(1:10, w/t, pch='', xlab='clusters', ylab='within/tot', main='Choice of k', ylim=c(0,1))
lines(1:10, w/t, type='b', lwd=2)