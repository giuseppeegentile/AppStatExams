# Marco Scarpelli

# Exam 18/01/2023

# Exercise 1

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

# ATTENZIONE: questa libreria interferisce con biplot
# library(plot.matrix) 


rm(list=ls())
graphics.off()

df_nonScaled<-read.table('beers.txt',header = T)

head(df_nonScaled)

df <- scale(df_nonScaled)

head(df)

n<-dim(df)[1]
p<-dim(df)[2]

plot(df, main='', pch=19) # plot if p=1 or 2, pairs otherwise

##########################################
# Point A
dist.matrix <- dist(df, method='euclidean') # euclidean, manhattan, camberra

par(mfrow=c(1,1)) # Se vogliamo plottare più metriche una a fianco all'altra
image(1:n, 1:n, as.matrix(dist.matrix), main = "Dissimilarity matrix", asp = 1, xlab = "i", ylab = "j", ylim = rev(range(1:n)))
par(mfrow=c(1,1))

hclust.output.single <- hclust(dist.matrix, method = "single") # complete, single, average, ward.D2
hclust.output.complete <- hclust(dist.matrix, method = "complete") # complete, single, average, ward.D2

par(mfrow=c(1,2))
plot(hclust.output.single, main = "Single linkage", 
     hang = -0.1, xlab = "", labels = F, cex = 0.6, sub = "")
plot(hclust.output.complete, main = "Complete linkage", 
     hang = -0.1, xlab = "", labels = F, cex = 0.6, sub = "")


##########################################
# Point B

# We can see that the single linkage method tends to
#   suffer from the chaining effect.
# We choose 2 clusters and complete linkage.
k=2
rect.hclust(hclust.output.complete, k = k)
par(mfrow=c(1,1))

# cut the cluster with k clusters
clusters <- cutree(hclust.output.complete, k = k)

# Table with output
table(clusters)

##########################################
# Point C

plot(df, col = clusters, pch = 19)
legend('topright', legend=c(levels(factor(clusters))), col = c(levels(factor(clusters))), lty=1)

# cophenetic matrix
coph.matrix <- cophenetic(hclust.output.complete)
coph.coeff <- cor(dist.matrix, coph.matrix)
coph.coeff # 0.7919285

# Cluster sizes:
table(clusters)
#  1  2 
# 70 55 

# Method 2: for loop
lengths = {}
means = {}
for (i in 1:k) { #one for each combination tested (nrows of matrix A)
  lengths <- c(lengths, length(which(clusters == i))) # Cluster sizes (just use table(clusters))
  means <- rbind(means, colMeans(df[which(clusters == i),])) # Centroids (means)
}
# Centroids (i.e. means)
means
#         alcohol        ibu
# [1,] -0.6909623 -0.8190009
# [2,]  0.8794066  1.0423647