###---------------------------------------------------------------------###
### Problem 2: Similarities between soil pollutant molecules (20230616) ###
###---------------------------------------------------------------------###

library(dbscan)
library(cluster)

rm(list = ls())
graphics.off()

dist <- read.table('molecules.txt', header = T)

n <- dim(dist)[1]

dist <- as.dist(dist)
image(1:n, 1:n, as.matrix(dist), asp = 1, xlab = 'i', ylab = 'j')

# a) ----------------------------------------------------------------------

# We are given a distance matrix (euclidean), therefore we could go with 
# both hierarchical clustering or DBSCAN


# b) ----------------------------------------------------------------------

HC <- hclust(dist, method = 'average')

plot(HC, main = paste(HC$dist.method, "-" , HC$method, sep = ''), hang = -0.1, sub = '', labels = F, xlab = '')

k <- 3 # number of clusters
rect.hclust(HC, k = k)

cluster <- cutree(HC, k = k)
table(cluster)


# c) ----------------------------------------------------------------------

minPts <- 3

kNNdistplot(dist, minPts = minPts)
abline(h = 0.10, col = "red", lty = 2)
threshold <- 0.10

dbs <- dbscan(dist, eps = threshold, minPts = minPts)
dbs

table(dbs$cluster)

clustered_index <- which(dbs$cluster != 0) # Index of non noise points
clustered_labels <- dbs$cluster[clustered_index] # corresponding labels

dist.new <- read.table('molecules.txt', header = T)
dist.new <- dist.new[clustered_index, clustered_index]

sil <- silhouette(clustered_labels, dist.new)
summary(sil)

sil_score <- function(labels, dist) {
  # Compute the average of the silhouette widths
  sil <- silhouette(labels, dist)
  sil_widths <- sil[, "sil_width"]
  mean(sil_widths)
}

sil_score(clustered_labels, dist.new)
# 0.5183827 -> good


# d) ----------------------------------------------------------------------

minPts <- 10

threshold <- 0.15

dbs <- dbscan(dist, eps = threshold, minPts = minPts)
dbs

table(dbs$cluster)

clustered_index <- which(dbs$cluster != 0) # Index of non noise points
clustered_labels <- dbs$cluster[clustered_index] # corresponding labels

dist.new <- read.table('molecules.txt', header = T)
dist.new <- dist.new[clustered_index, clustered_index]

sil <- silhouette(clustered_labels, dist.new)
summary(sil)

sil_score(clustered_labels, dist.new)
# 0.5618077 -> better than before

# Hierarchical's Silhouette

sil <- silhouette(cluster, dist)
summary(sil)

sil_score(cluster, dist)
# 0.6587052 -> best one


# e) ----------------------------------------------------------------------

mds <- cmdscale(dist, k = 2)
mds

plot(mds[, 1], mds[, 2], type = 'n', asp = 1, axes = FALSE, main = "MDS", xlab = '', ylab = '')
text(mds[, 1], mds[, 2], labels = colnames(as.matrix(dist)), cex = 0.75, pos = 3)

# Plot of the resulting clusterings

plot(mds[, 1], mds[, 2], col = dbs$cluster + 1, asp = 1, pch = 16, lwd = 2) # best DBSCAN
plot(mds[, 1], mds[, 2], col = cluster + 1, asp = 1, pch = 16, lwd = 2) # hierarchical

plot(dist, dist(mds))
# The 2D plot tends to overestimate the shorter true distances
