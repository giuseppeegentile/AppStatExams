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


# b) ----------------------------------------------------------------------

vars <- NULL
for(k in 1:4)
{
  vars = rbind(vars, sd(data[cluster==k,1])^2)
}

n <- dim(data)[1]
k <- 2
alpha <- 0.1
cfr.t <- qt(1-alpha/(2*k), n-1) 

BF <- NULL
for (i in 1:4)
{
  BF_i <- rbind(c(means[i] - cfr.t*sqrt(vars[i]/n),
                  means[i],
                  means[i] + cfr.t*sqrt(vars[i]/n)),
                c(vars[i]*(n-1) / qchisq(1 - alpha/(2*k), n-1),
                  vars[i],
                  vars[i]*(n-1) / qchisq(alpha/(2*k), n-1)))
  
  dimnames(BF_i)[[1]] <- c(paste("m", i, sep=''), paste("v", i, sep=''))
  BF = rbind(BF, BF_i)
}

dimnames(BF)[[2]] <- c('inf','center','sup')
BF
