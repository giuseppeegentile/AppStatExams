library(mvtnorm)
library(MVN)
library(rgl)
library(car)
library(dbscan)
library(cluster)
library(fields)

data <- read.table("molecules.txt", header=TRUE)
head(data)

# we have already a distance matrix, since the devised distance is specific for this problem, i would
# use a hierarchical clustering method

# as dist should suffice
struct <- hclust(as.dist(data), method="average")
plot(struct)
# from the dendrogram seems that 3 is the best k

cluster <- cutree(struct, k=3)
table(cluster)

# DBSCAN
minPts <- 3

#knnDistPlot  should work also with the distance matrix
kNNdistplot(as.dist(data),minPts=minPts)
abline(h=0.1)

# Grid Search
n <- dim(data)[1]
minPts_grid <- 3
eps_grid <- seq(0.001, 0.5, by = 0.05)

max_share_noise <- 0.2

sil_score <- function(labels, dist) {
  # Compute the average of the silhouette widths
  sil <- silhouette(labels, dist)
  sil_widths <- sil[,"sil_width"]
  mean(sil_widths)
}

dbscan_perf <- function(minPts, eps) {
  # Compute the silhouette score resulting from dbscan clustering
  dbs <- dbscan(as.dist(data), eps, minPts) # Run dbscan
  
  noise <- which(dbs$cluster == 0) # Index of  noise points
  clustered_labels <- dbs$cluster[-noise] # labels of non noise
  nb_clusters <- length(unique(clustered_labels))
  
  if ((nb_clusters > 1 & nb_clusters < n) & (length(which(dbs$cluster == 0))/n < max_share_noise)) { 
    # Silhouette score is defined only if 2 <= nb_clusters <= n-1
    sil_score(clustered_labels, as.dist(data[-noise,-noise]))
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


max_score <- max(perf_grid)
argmax_score <- which(perf_grid == max_score, arr.ind = TRUE)
best_eps <- eps_grid[argmax_score[2]]
best_minPts <- minPts_grid[argmax_score[1]]
best_eps
best_minPts
max_score

# both the approaches suggest 0.10 as epsilon
db.clusters <- dbscan(as.dist(data),eps = 0.10, minPts = 3)
db.clusters

# we find 2 clusters, and 6 noise points
6/90 # 6% of noise points

# DBSCAN with minPts=10 and eps=0.15
db.clusters2 <- dbscan(as.dist(data),eps=0.15, minPts=10)
db.clusters2

# we find 3 clusters (1 23, 2 22, 3 36), and 9 noise points
# to compare these two result we can use the silhouette score
# score for hierarchical method
score1 <- sil_score(cluster, as.dist(data))

# score for last dbscan
noise2 <- which(db.clusters2$cluster==0)

label2 <- db.clusters2$cluster[-noise2]
score2 <-sil_score(label2, as.dist(data[-noise2, -noise2]))

score1
score2


# we may prefer the cluster with highest silhouette score -> hierarchical clustering
# on average points belonging to a cluster are closer to the cluster itself than to other clusters

# multidimensional scaling
scaling <- cmdscale(as.dist(data),k=2)
scaling

plot(scaling[,1], scaling[,2],col=(db.clusters2$cluster)+1, asp=1, xlab='',ylab='',pch=19)

as.matrix(abs(as.matrix(dist(scaling)) - as.matrix(data)))


plot(as.dist(data), dist(scaling))

# it tends to overestimate the distances