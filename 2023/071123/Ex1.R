# NOME COGNOME

# Exam 07/11/2023

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

df <- read.table('kapok.txt', header=T)

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

hclust.output <- hclust(dist.matrix, method = "ward.D2") # complete, single, average, ward.D2
plot(hclust.output, main = "Dendrogram", 
     hang = -0.1, xlab = "", labels = F, cex = 0.6, sub = "")

# 3 or 4 clusters from the dendrogram even though it's definitely
#   5 judging from the plot.
# NOTE: Using k=5 creates clusters which pass the multivariate
#   normality test, so tweak the following as needed.
k <- 5
rect.hclust(hclust.output, k = k)

# cut the cluster with k clusters
clusters <- cutree(hclust.output, k = k)
table(clusters)

# plot clusters 
plot(df, col = clusters, pch = 19)
legend('topright', legend=c(levels(factor(clusters))), col = c(levels(factor(clusters))), lty=1)

# Interpretation is the same as the Alaskan Forests exam.

##########################################
# Point B

i1 <- which(clusters == 1)
i2 <- which(clusters == 2)
i3 <- which(clusters == 3)
i4 <- which(clusters == 4)
i5 <- which(clusters == 5)

# Assumption: Multivariate normality (within each cluster)
mvn(df[i1,])$multivariateNormality # p=3.994804e-12
mvn(df[i2,])$multivariateNormality # p=0.5651105
mvn(df[i3,])$multivariateNormality # p=0.0003658513

# At this point one should probably re-do the exercise with 5 clusters.

# Using 5 clusters, each one is multivariate normal.
mvn(df[i4,])$multivariateNormality
mvn(df[i5,])$multivariateNormality



alpha<-.1
num_comparisons<-k*p*2 #numero di confronti

means <- sapply(df,mean)

lengths = {}
means = {}
for (i in 1:k) { #one for each combination tested (nrows of matrix A)
  lengths <- c(lengths, length(which(clusters == i))) # Cluster sizes (just use table(clusters))
  means <- rbind(means, colMeans(df[which(clusters == i),])) # Centroids (means)
}
# Centroids (i.e. means)
means

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
#                      inf      center   sup     
# height   "Cluster 1" "66.418" "67.434" "68.45" 
# diameter "Cluster 1" "7.014"  "7.174"  "7.334" 
# height   "Cluster 2" "53.364" "53.745" "54.125"
# diameter "Cluster 2" "5.903"  "6.112"  "6.32"  
# height   "Cluster 3" "85.362" "87.185" "89.008"
# diameter "Cluster 3" "8.444"  "9.315"  "10.187"

# Variance
Bf.IC.vars
#                      inf      center   sup     
# height   "Cluster 1" "17.385" "22.982" "31.556"
# diameter "Cluster 1" "0.43"   "0.569"  "0.781" 
# height   "Cluster 2" "1.376"  "1.954"  "2.951" 
# diameter "Cluster 2" "0.415"  "0.589"  "0.889" 
# height   "Cluster 3" "9.111"  "15.776" "32.342"
# diameter "Cluster 3" "2.082"  "3.604"  "7.389" 

