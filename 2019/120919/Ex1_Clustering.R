# Marco Scarpelli

# Exam 12/09/2019+

# Exercise 1

library(MASS)
library(car)
library(rgl)   #3D plots
library(leaps) #best subset selection
library(tree)  #decision trees
library(corrplot) #correlation
library(glmnet)
library(mvnormtest)
library(MVN)
library(heplots)
library(ggplot2)
library(mvtnorm)

rm(list=ls())
graphics.off()

df <- read.table('sequoia.txt', header=T)

head(df)

n<-dim(df)[1]
p<-dim(df)[2]

##########################################
# Point A

plot(df, main='', pch=19) # plot if p=1 or 2, pairs otherwise

dist.matrix <- dist(df, method='euclidean') # euclidean, manhattan, camberra

par(mfrow=c(1,1)) # Se vogliamo plottare più metriche una a fianco all'altra
image(1:n, 1:n, as.matrix(dist.matrix), main = "Dissimilarity matrix", asp = 1, xlab = "i", ylab = "j", ylim = rev(range(1:n)))
par(mfrow=c(1,1))

hclust.output <- hclust(dist.matrix, method = "ward.D2") # complete, single, average, ward.D2

hclust.output$merge  # order of aggregation of statistical units / clusters
hclust.output$height # distance at which we have aggregations
hclust.output$order  # ordering that allows to avoid intersections in the dendrogram

plot(hclust.output, main = "Dendrogram", 
     hang = -0.1, xlab = "", labels = F, cex = 0.6, sub = "")

# From the plot there seem to be 5 clusters, while looking at the dendrogram only I would say 3 or 4.
k <- 5

rect.hclust(hclust.output, k = k)

# cut the cluster with k clusters
clusters <- cutree(hclust.output, k = k)

# Table with output
table(clusters)

# plot clusters 
plot(df, col = clusters, pch = 19)
legend('bottomright', legend=c(levels(factor(clusters))), col = c(levels(factor(clusters))), lty=1)

# Size of clusters
table(clusters)

lengths = {}
means = {}
for (i in 1:k) { #one for each combination tested (nrows of matrix A)
  lengths <- c(lengths, length(which(clusters == i)))
  means <- rbind(means, colMeans(df[which(clusters == i),])) # Centroids (means)
}

# Centroids (i.e. means)
means

# In my opinion, the secular trees are geographically-clustered: when a large fire occurs, it will 
#   raze entire portions of the forest and trees will regrow there. As trees age, they become 
#   taller and thicker; since they grow very slowly, it is safe to assume that trees born
#   in a zone that was razed by a fire will present roughly the same height and thickness,
#   so we can identify 5 separate zones where the trees have grown at a similar pace.
# We could identify the cluster with the highest thickness and height with trees that survived
#   all fires, or at least outlived the others by a lot. The other clusters may indicate
#   zones where there was a fire; these fires were not too far apart in time since the
#   centroids of the clusters are not too different.

##########################################
# Point B
alpha<-0.1
num_comparisons <- 10

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

# Variance
Bf.IC.vars

      