# Marco Scarpelli

# Exam 2024/01/17

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
library(dbscan)
library(cluster)

# ATTENZIONE: questa libreria interferisce con biplot
# library(plot.matrix) 


rm(list=ls())
graphics.off()

df <- read.table('sharks.txt', header=T)

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

hclust.output <- hclust(dist.matrix, method = "average") # complete, single, average, ward.D2
plot(hclust.output, main = "Dendrogram", 
     hang = -0.1, xlab = "", labels = F, cex = 0.6, sub = "")

# 4 clusters seem appropriate, also judging from the data plot.
k <- 4
rect.hclust(hclust.output, k = k)

# cut the cluster with k clusters
clusters <- cutree(hclust.output, k = k)
table(clusters)
# Cluster sizes:
#  1  2  3  4 
# 28 35 43 24 

# plot clusters 
plot(df, col = clusters, pch = 19)
legend('topright', legend=c(levels(factor(clusters))), col = c(levels(factor(clusters))), lty=1)

# Method 2: for loop
lengths = {}
means = {}
for (i in 1:k) { #one for each combination tested (nrows of matrix A)
  lengths <- c(lengths, length(which(clusters == i))) # Cluster sizes (just use table(clusters))
  means <- rbind(means, colMeans(df[which(clusters == i),])) # Centroids (means)
}
# Centroids (i.e. means)
means
#      circumference   length
# [1,]      1.001071 1.468214
# [2,]      2.214286 3.332857
# [3,]      3.062791 5.064186
# [4,]      3.907500 6.619583

# There are 4 sharks clusters, with increasing length and circumference. Since
#   these are probably related with age, we could say that there are 4 pretty distinct
#   age groups within the shark population.
# ... I'm too tired to say something meaningful about the correlation of this fact
#   with earthquakes.

##########################################
# Point B

i1 <- which(clusters == 1)
i2 <- which(clusters == 2)
i3 <- which(clusters == 3)
i4 <- which(clusters == 4)

# Assumption: Multivariate normality (within each cluster)
mvn(df[i1,])$multivariateNormality # p=0.3691683
mvn(df[i2,])$multivariateNormality # p=0.9305703
mvn(df[i3,])$multivariateNormality # p=0.8971202
mvn(df[i4,])$multivariateNormality # p=0.351516

# Each one is multivariate normal.

alpha<-.1
num_comparisons<-k*p*2 # For each cluster (4) compare, for 2 variables (p), mean and variance (2) 

means <- sapply(df,mean)

lengths = {}
means = {}
for (i in 1:k) { #one for each combination tested (nrows of matrix A)
  lengths <- c(lengths, length(which(clusters == i))) # Cluster sizes (just use table(clusters))
  means <- rbind(means, colMeans(df[which(clusters == i),])) # Centroids (means)
}

cfr.ts = {}
Bf.IC.means = {}
Bf.IC.vars = {}
for (i in 1:k) {
  cfr.ts <- c(cfr.ts, qt(1-alpha/(2*num_comparisons),lengths[i]-1))
  Bf.IC.means <- rbind(
    Bf.IC.means, cbind(
      paste("Cluster", i, sep=" "),
      round(
        cbind(
          inf = means[i,] - cfr.ts[i]*sqrt(diag(cov(df[which(clusters == i),]))/lengths[i]),
          center = means[i,], 
          sup = means[i,] + cfr.ts[i]*sqrt(diag(cov(df[which(clusters == i),]))/lengths[i])
        ),
        3)
    )
  )
  
  Bf.IC.vars<-rbind(
    Bf.IC.vars, cbind(
      paste("Cluster", i, sep=" "),
      round(
        cbind(
          inf = diag(cov(df[which(clusters == i),]))*(lengths[i]-1) / qchisq(1 - alpha/(2*num_comparisons), lengths[i]-1),
          center = diag(cov(df[which(clusters == i),])),
          sup = diag(cov(df[which(clusters == i),]))*(lengths[i]-1) / qchisq(alpha/(2*num_comparisons), lengths[i]-1)
        ),
        3)
    )
  )
}
# Means
Bf.IC.means
#                               inf     center  sup    
# circumference "Cluster 1" "0.916" "1.001" "1.086"
# length        "Cluster 1" "1.338" "1.468" "1.598"
# circumference "Cluster 2" "2.126" "2.214" "2.303"
# length        "Cluster 2" "3.22"  "3.333" "3.446"
# circumference "Cluster 3" "2.986" "3.063" "3.14" 
# length        "Cluster 3" "4.951" "5.064" "5.177"
# circumference "Cluster 4" "3.761" "3.908" "4.054"
# length        "Cluster 4" "6.425" "6.62"  "6.815"

# Variance
Bf.IC.vars
#                               inf     center  sup    
# circumference "Cluster 1" "0.012" "0.023" "0.055"
# length        "Cluster 1" "0.028" "0.054" "0.13" 
# circumference "Cluster 2" "0.018" "0.032" "0.07" 
# length        "Cluster 2" "0.029" "0.053" "0.114"
# circumference "Cluster 3" "0.018" "0.031" "0.061"
# length        "Cluster 3" "0.039" "0.066" "0.131"
# circumference "Cluster 4" "0.028" "0.057" "0.15" 
# length        "Cluster 4" "0.051" "0.101" "0.266"

