###------------###
### CLUSTERING ###----------------------------------------------------------
###------------###

### Try It Out! ###
rm(list = ls())
graphics.off()
par(mfrow=c(1,1))
cat("\014")
data <- read.table('datasets/data_clustering.txt', header = T)
### Try It Out! ###

n <- dim(data)[1]

plot(data, pch = 19)


#### Hierarchical Clustering ----

# Dissimilarity Matrix (available metrics: euclidean, manhattan, canberra)

dist <- dist(data, method = 'euclidean')
image(1:n, 1:n, as.matrix(dist), asp = 1, xlab = 'i', ylab = 'j')

# Dissimilarity Between Clusters (available methods: single, complete, average, ward.D2)

HC <- hclust(dist, method = 'ward.D2')

names(HC)
HC$merge  # order of aggregation of statistical units / clusters
HC$height # distance at which we have aggregations
HC$order  # ordering that allows to avoid intersections in the dendrogram

# Dendrogram

plot(HC, main = paste(HC$dist.method, "-" , HC$method, sep = ''), hang = -0.1, sub = '', labels = F, xlab = '')

k <- 2 # number of clusters
rect.hclust(HC, k = k)

# Clustering

cluster <- cutree(HC, k = k)
table(cluster)

plot(data, col = cluster + 1, pch = 16, lwd = 2)

means <- NULL
for(i in 1:length(unique(cluster)))
{
  means = rbind(means, sapply(data[cluster == i,], mean))
}

means
points(means, col = sort(unique(cluster)) + 1, pch = '*', cex = 4)

# Cophenetic Coefficient

coph <- cophenetic(HC)
cor(dist, coph)

par(mfrow=c(1,2))
image(as.matrix(dist), main = HC$dist.method)
image(as.matrix(coph), main = HC$method)
par(mfrow=c(1,1))


##### Choice of k (W(k)/SS_tot Plot) -----

w <- NULL
w_i <- NULL
mean <- colMeans(data)

for(k in 1:10)
{
  cluster <- cutree(HC, k = k)
  w_i <- NULL
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
    w_i <- c(w_i, sum(cluster_i_centered_rel^2))
  }
  w <- c(w, sum(w_i))
}

data_centered <- data
data_centered[, 1] <- data[, 1] - mean[1]
data_centered[, 2] <- data[, 2] - mean[2]

t <- sum(data_centered^2)

matplot(1:10, w/t, pch = '', xlab = 'clusters', ylab = 'within/tot', main = 'Choice of k', ylim = c(0, 1))
lines(1:10, w/t, type = 'b', lwd = 2)


#### K-Means ----

k <- 2
result.k <- kmeans(data, centers = k) # Centers: fixed number of clusters

names(result.k)

result.k$cluster      # labels of clusters
result.k$centers      # centers of the clusters
result.k$totss        # tot. sum of squares
result.k$withinss     # sum of squares within clusters
result.k$tot.withinss # sum(sum of squares within cluster)
result.k$betweenss    # sum of squares between clusters
result.k$size         # dimension of the clusters

plot(data, col = result.k$cluster + 1, pch = 16, lwd = 2)
points(result.k$centers, col = sort(unique(result.k$cluster)) + 1, pch = '*', cex = 4)


##### Choice of k (W(k)/SS_tot Plot) -----

b <- NULL
w <- NULL
for(k in 1:10)
{
  result.k <- kmeans(data, k)
  w <- c(w, sum(result.k$wit))
  b <- c(b, result.k$bet)
}

matplot(1:10, w/(w+b), pch = '', xlab = 'clusters', ylab = 'within/tot', main = 'Choice of k', ylim = c(0, 1))
lines(1:10, w/(w+b), type = 'b', lwd = 2)


#### DBSCAN ----

# Choice of hyperparameters for DBSCAN
# Rule of thumb, minPts should be at least p + 1
# Can be eventually increased
minPts <- 3

# How to choose eps from minPts?
# Plot of the distances to the minPts-1 nearest neighbor
kNNdistplot(data, minPts = minPts) #!library(dbscan)
# Set a good threshold
abline(h = 2.75, col = "red", lty = 2)
threshold <- 2.75

# Run the dbscan
dbs <- dbscan(data, eps = threshold, minPts = minPts)
dbs

table(dbs$cluster)

# Plot of the resulting clustering
plot(data, col = dbs$cluster + 1, pch = 16, lwd = 2)


##### Silhouette Scores -----

# How to tune the algorithm and find the "best" eps and minPts?
# Silhouette score (from the package "cluster")
help(silhouette) #!library(cluster)

# Let's compute the silhouette score on the clustering performed before
# WARNING (specific to DBSCAN): We need to remove the noise points as they do
# not belong to a cluster, before computing the silhouette score
clustered_index <- which(dbs$cluster != 0) # Index of non noise points
clustered_points <- data[clustered_index, ] # only clustered points
clustered_labels <- dbs$cluster[clustered_index] # corresponding labels

# Silhouette coefficient is calculated for each unit i in the following way:
# sil(i) = (b(i)-a(i)) / max{a(i),b(i)} [it ranges from -1 to 1]
# where: - a(i) is the average distance of i to a point of the SAME cluster
#        - b(i) is the average distance of i to a point of the NEAREST (different) cluster
sil <- silhouette(clustered_labels, dist(clustered_points))
summary(sil)

sil_score <- function(labels, dist) {
  # Compute the average of the silhouette widths
  sil <- silhouette(labels, dist)
  sil_widths <- sil[, "sil_width"]
  mean(sil_widths)
}

sil_score(clustered_labels, dist(clustered_points))

# Grid Search
minPts_grid <- 1:20
eps_grid <- seq(threshold/5, threshold*3, length = 20)

max_share_noise <- 0.2

dbscan_perf <- function(minPts, eps) {
  # Compute the silhouette score resulting from dbscan clustering
  dbs <- dbscan(data, eps, minPts) # Run dbscan
  
  clustered_index <- which(dbs$cluster != 0) # Index of non noise points
  clustered_points <- data[clustered_index, ] # only clustered points
  clustered_labels <- dbs$cluster[clustered_index] # corresponding labels
  nb_clusters <- length(unique(clustered_labels))
  
  if ((nb_clusters > 1 & nb_clusters < n) & (length(which(dbs$cluster == 0))/n < max_share_noise)) { 
    # Silhouette score is defined only if 2 <= nb_clusters <= n-1
    sil_score(clustered_labels, dist(clustered_points))
  }
  
  else {
    # otherwise we return 0 which would be the approx. value of the silhouette
    # score if the clusters were completely overlapping
    0
  }
}

# We compute the silhouette score for all combinations of minPts and eps
perf_grid <- outer(minPts_grid, eps_grid, FUN = Vectorize(dbscan_perf))
dimnames(perf_grid) <- list(minPts_grid, eps_grid)

# Histogram of the Silhouette scores
hist(perf_grid, breaks = 20, xlab = "Silhouette score", xlim = c(-1, 1), main = NULL)

max_score <- max(perf_grid)
min_score <- min(perf_grid)
max_abs <- max(abs(max_score), abs(min_score))

image.plot(x = eps_grid, y = minPts_grid, z = perf_grid, xlab = "eps", ylab = "minPts",
           main = 'Silhouette score', col = hcl.colors(64, palette = 'Blue-Red'),
           breaks = c(seq(-max_abs, 0, length = 33)[-33], seq(0, max_abs, length = 33))) #!library(fields)
par(mfrow=c(1,1))

# Retrieve best parameter values
max_score <- max(perf_grid)
argmax_score <- which(perf_grid == max_score, arr.ind = TRUE)
best_eps <- eps_grid[argmax_score[2]]
best_minPts <- minPts_grid[argmax_score[1]]
best_eps
best_minPts
max_score

# Run the dbscan
dbs <- dbscan(data, best_eps, best_minPts)
dbs

plot(data, col = dbs$cluster + 1, pch = 16, lwd = 2)


#### Multidimensional Scaling ----

# Given the distances (dissimilarities) among n statistical units, look for 
# the k-dimensional representation (k small) of the n statistical units
# such that the distances (dissimilarities) among the representations
# of the n units are as close as possible to the original distances
# (dissimilarities) among the n units.

help(eurodist)
dist <- eurodist

mds <- cmdscale(dist, k = 2)
mds

plot(mds[, 1], mds[, 2], type = 'n', asp = 1, axes = FALSE, main = "MDS", xlab = '', ylab = '')
text(mds[, 1], mds[, 2], labels = colnames(as.matrix(dist)), cex = 0.75, pos = 3)

# Compare the original matrix d_ij = d(x_i,x_j) and delta_ij = d(y_i,y_j) 
plot(dist, dist(mds))

# Visualize the most different distances
par(cex = 0.75, mar = c(10, 10, 2, 2))
image(1:21, 1:21, asp = 1, abs(as.matrix(dist(mds)) - as.matrix(dist)), axes = F, xlab = '', ylab ='')
axis(1, at = 1:21, labels = colnames(as.matrix(dist)), las = 2, cex = 0.75)
axis(2, at = 1:21, labels = colnames(as.matrix(dist)), las = 1, cex = 0.75)
box()

# Rome-Athens
as.matrix(dist)[19,1]
as.matrix(dist(mds))[19,1]

# Cologne-Geneve
as.matrix(dist)[6,8]
as.matrix(dist(mds))[6,8]


# Compute the "stress": the higher it is, the worse
# the matching between original distances and their
# geometrical representation through MDS
Stressk <- NULL
for(k in 1:4)
{
  mds.k <- cmdscale(dist, k)
  Stress <- (sum((as.vector(dist) - as.vector(dist(mds.k)))^2) / sum(as.vector(mds.k)^2))^(1/2)
  Stressk <- c(Stressk, Stress) 
}

plot(1:4, Stressk, xlab = 'k', ylab = 'Stress', lwd = 2)
lines(1:4, Stressk, type = 'b', lwd = 2)