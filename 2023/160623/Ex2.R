# Marco Scarpelli

# Exam 16/06/2023

# Exercise 2

library(MASS)
library(car)
library(rgl)
library(leaps)
library(tree)
library(corrplot)
library(glmnet)
library(mvnormtest)
library(MVN)
library(heplots)
library(ggplot2)
library(mvtnorm)
library(nlme)
library(lme4)
library(insight)
library(nlmeU)
library(lattice)
library(class)
library(dbscan)
library(cluster)

# ATTENZIONE: questa libreria interferisce con biplot
# library(plot.matrix) 

rm(list=ls())
graphics.off()

set.seed(1)

df.orig <- read.table('molecules.txt', header=T)

df <- as.dist(df.orig)

head(df)

##########################################
# Point A + B
plot(df, main='', pch=19, cex=.3) # plot if p=1 or 2, pairs otherwise

image(1:90, 1:90, as.matrix(df), main = "Dataframe", asp = 1, xlab = "i", ylab = "j", ylim = rev(range(1:90)))

# We need an unsupervised method, so no KNN.
#   DBScan can exclude outliers more easily.
#   Furthermore, we don't know the exact number of
#     clusters in advance, and judging from the plot,
#     hierarchical methods' dendrograms will be bad.
# We could use multidimensional scaling to scale down the data.

hclust.output <- hclust(df, method = "average") # complete, single, average, ward.D2
plot(hclust.output, main = "Dendrogram", 
     hang = -0.1, xlab = "", labels = F, cex = 0.6, sub = "")

# 3 clusters seems fine

k=3
  
rect.hclust(hclust.output, k = k)

# cut the cluster with k clusters
clusters <- cutree(hclust.output, k = k)
names(hclust.output) # Rende i possibili field a cui possiamo accedere

table(clusters)

ordered_index <- order(clusters)
matrix_reordered <- as.matrix(df)[ordered_index, ordered_index]
image(1:90, 1:90, matrix_reordered, asp=1, main='', pch=19, ylim = rev(range(1:90))) # plot if p=1 or 2, pairs otherwise

##########################################
# Point C
minPts <- 3
kNNdistplot(df, minPts = minPts)
# Elbow rule: eps = 0.1
abline(h = 0.1, col = "red", lty = 2)

myEps <- .1

dbs <- dbscan(df, eps = myEps, minPts = minPts)
dbs
# 2 clusters and 6 noise points
# 0  1  2 
# 6 56 28 

##########################################
# Point D

minPts <- 10
myEps <- .15
dbs <- dbscan(df, eps = myEps, minPts = minPts)
dbs
# The clustering contains 3 cluster(s) and 9 noise points.
# 0  1  2  3 
# 9 23 22 36 

# Silhouette score.
clustered_index.DBScan <- which(dbs$cluster != 0) # Index of non noise points
clustered_points.DBScan <- df.orig[clustered_index.DBScan, ] # only clustered points
clustered_labels.DBScan <- dbs$cluster[clustered_index.DBScan] # corresponding labels

sil_score <- function(labels, dist) {
  # Compute the average of the silhouette widths
  sil <- silhouette(labels, dist)
  sil_widths <- sil[,"sil_width"]
  mean(sil_widths)
}

sil_score(clustered_labels.DBScan, dist(clustered_points.DBScan)) # 0.4767451

sil_score(clusters, df) # 0.6587052

##########################################
# Point E
reduced.df.orig <- cmdscale(df.orig, k=2)
reduced.df <- cmdscale(df, k=2)

par(mfrow=c(1,2))
plot(reduced.df, col = dbs$cluster+1, asp=1, axes=FALSE, main="MDS of data (DBScan)")
plot(reduced.df, col = clusters, asp=1, axes=FALSE, main="MDS of data (Hierarchical)")
par(mfrow=c(1,1))

plot(df, dist(reduced.df), asp=1, axes=FALSE, main="MDS of data (Hierarchical)")

Stressk <- NULL
kMax <- 10

for(k in 1:kMax)
{
  location.k <- cmdscale(df, k)
  Stress <- (sum( (as.vector(df) - as.vector(dist(location.k)))^2)  /
               sum( as.vector(location.k)^2))^(1/2)
  Stressk <- c(Stressk, Stress) 
}
Stressk
plot(1:kMax,Stressk,xlab='k',ylab='Stress',lwd=2)

image(1:90, 1:90, asp=1, abs(as.matrix(dist(reduced.df)) - as.matrix(df.orig)), axes = F, xlab = '', ylab ='')
axis(1, at = 1:90, labels = colnames(df), las = 2, cex = 0.75)
axis(2, at = 1:90, labels = colnames(df), las = 1, cex = 0.75)
box()
