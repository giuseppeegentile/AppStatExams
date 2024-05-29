###------------------------------------------------------------###
### Problem 1: Blueland Sharks in the Abyssal Haven (20240117) ###
###------------------------------------------------------------###


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

data <- read.table('sharks.txt', header = T)
head(data)

plot(data)


# a) ----------------------------------------------------------------------

HC <- hclust(dist(data, method='euclidean'), method = 'average')

plot(HC, hang=-0.1, sub='', labels=F, xlab='')

rect.hclust(HC, k=4)

cluster <- cutree(HC, k=4)
table(cluster)

plot(data , col=cluster+1, asp=1, pch=16, lwd=2)

means <- NULL
for(k in 1:4)
{
  means = rbind(means, sapply(data[cluster==k,],mean))
}
  
points(means, pch=17, cex=1.5)
