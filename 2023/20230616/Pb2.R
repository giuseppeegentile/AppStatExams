
library(mvtnorm)
library(MVN)
library(rgl)
library(car)
library(dbscan)
library(cluster)
library(fields)
setwd("C:/Users/Giuseppe/Documents/.magistrale/secondo anno/AppStat/Exams/2023/20230616/20230616")
data <- read.table("molecules.txt",header=T)
head(data)
dim(data)

data.e <- as.dist(data)

# stabilize the ordering of the data
misc <- sample(dim(data)[1])
data <- data[misc,]


#I'd use hierarchical clusterings method since is directly provided the distances between 
# entries, so we won't need to find the optimal distance policy (that is the hardest thing to do)
# but only the linkage one. 
# If the distance between molecules is well defined by graph disance (prior knowledge of the 
# problem) -> I'd use hierarchical.


# Linkage:
data.ea <- hclust(data.e, method='average')


plot(data.ea, main='euclidean-average', hang=-0.1, xlab='', labels=F, cex=0.6, sub='')

# The optimal clustering structure appears with 3 clusters
rect.hclust(data.ea, k=3)

cluster.ea <- cutree(data.ea, k=3) # euclidean-average

# Size of clusters
{
  length(which(cluster.ea ==1))
  length(which(cluster.ea ==2))
  length(which(cluster.ea ==3))
}

# Are indeed balanced


# compute the cophenetic coeff, not requested, but to validate point a
coph.ea <- cophenetic(data.ea)
ea <- cor(data.e, coph.ea)
ea
# 0.78 is quite good



# c)
minPts <- 3


kNNdistplot(data.e, minPts = minPts) # elbow method
abline(h = 0.1, col = "red", lty = 5)
# -> we choose 0.1

eps = 0.1

# Run dbscan and plot
{
  dbs <- dbscan(data.e, eps = 0.1, minPts = 3)
  dbs
  # There are 6 noise points
  # There are 2 clusters, unbalanced: one with 28 and the other with 56
  # -> not satisfacted, however, DBS is very sensitive to parameters
}
# Measure the quality of the cluster structure
{
  clustered_index <- which(dbs$cluster != 0) # Take only non noise points
  clustered_points <- data[clustered_index, ] # only clustered points
  clustered_labels <- dbs$cluster[clustered_index] # corresponding labels
  
  sil <- silhouette(clustered_labels, dist(clustered_points))
  summary(sil)
  
  # average of average cluster's sil score
  sil_score <- function(labels, dist) {
    # Compute the average of the silhouette widths
    sil <- silhouette(labels, dist)
    sil_widths <- sil[,"sil_width"]
    mean(sil_widths)
  }
  
  sil_score(clustered_labels, dist(clustered_points))
  # 0.00352856 
}
# sil score was better with hierarchical

# d)
dbs <- dbscan(data.e, eps = 0.15, minPts = 10)
dbs
# we get now 3 clusters and 9 noise points
# Cluster slightly more balanced

# We can calculate again the sil scores, to evaluate the best method

# Measure the quality of the cluster structure
{
  clustered_index <- which(dbs$cluster != 0) # Take only non noise points
  clustered_points <- data[clustered_index, ] # only clustered points
  clustered_labels <- dbs$cluster[clustered_index] # corresponding labels
  
  sil <- silhouette(clustered_labels, dist(clustered_points))
  summary(sil)
  
  # average of average cluster's sil score
  sil_score <- function(labels, dist) {
    # Compute the average of the silhouette widths
    sil <- silhouette(labels, dist)
    sil_widths <- sil[,"sil_width"]
    mean(sil_widths)
  }
  
  sil_score(clustered_labels, dist(clustered_points))
  # -0.05540497 
}

# we got negative score (even if low) so is definitely worst than the parameters used at point
# c.
# we prefer still the hierarchical clustering 


# e )
n=dim(data)[1] #entries

data.mds <- cmdscale(data.e, k=3)
data.mds
plot(data.mds[,1], data.mds[,2], type='n', asp=1, axes=FALSE, main="MDS Data")
text(data.mds[,1], data.mds[,2], cex = 0.75, pos = 3)

# compare the original matrix d_ij = d(x_i,x_j) and delta_ij = d(y_i,y_j) 
plot(data.e, dist(data.mds))
abline(coef = c(0,1), col="red")
# Many points are failed to be represented in the 2d plot
# In particular, all the failed points are above the diagonal, that means that the "true distance"
# that is the data.e is overestimated. 
# for instance, for the entry that is represented on the 2d plot with distance 1.5, the true
# distance is indeed 1.0


