library(mvtnorm)
library(MVN)
library(rgl)
library(car)
library(dbscan)
library(cluster)
library(fields)

## HIERARCHICAL CLUSTERING--------------------------------
data <- read.table("data.txt", header=TRUE)
head(data)

plot(data)

distance <- dist(data, method='euclidean')
distance <- dist(data, method='manhattan')
distance <- dist(data, method='canberra')

hclust.s <- hclust(distance, method='single')
hclust.a <- hclust(distance, method='average')
hclust.c <- hclust(distance, method='complete')
hclust.w <- hclust(distance, method='ward.D2')

# plot of the dendrograms
par(mfrow=c(2,2))
plot(hclust.s)
plot(hclust.a)
plot(hclust.c)
plot(hclust.w)
# plot of the rectagular region
rect.hclust(hclust.s, k=2)
rect.hclust(hclust.a, k=2)
rect.hclust(hclust.c, k=2)
rect.hclust(hclust.w, k=2)

clusters <- cutree(hclust.s, k=2) 
clusters <- cutree(hclust.a, k=2) 
clusters <- cutree(hclust.c, k=2) 

# plot if 2 clusters
plot(data, col=ifelse(clusters==1,"red","blue"),pch=19)

# size of the clusters
size1 <- length(clusters==1)
size1
size2 <- length(clusters==2)
size2

# centroids
centroid1 <- colMeans(data[which(clusters==1),])
centroid2 <- colMeans(data[which(clusters==2),])

# cophenetic matrix
cop <- cophenetic(hclust.c)
# cophenetic coefficient
coeff <- cor(distance,cop)
coeff

# K-MEANS ----------------------------------------------------------------------------------
plot(data)

result.k <- kmeans(data, centers=2) # Centers: fixed number of clusters

names(result.k)

result.k$cluster      # labels of clusters
result.k$centers      # centers of the clusters
result.k$totss        # tot. sum of squares
result.k$withinss     # sum of squares within clusters
result.k$tot.withinss # sum(sum of squares within cluster)
result.k$betweenss    # sum of squares between clusters
result.k$size         # dimension of the clusters

plot(data, col = result.k$cluster+1)

# How to choose k:
# 1) evaluate the variability between the groups with respect to 
#   the variability withing the groups
# 2) evaluate the result of hierarchical clustering (not recommended,
#    quite computationally expensive)

# (we've just seen method 2) and suggested k = 2)

# method 1)
b <- NULL
w <- NULL
for(k in 1:10){
  result.k <- kmeans(data, k)
  w <- c(w, sum(result.k$wit))
  b <- c(b, result.k$bet)
}

matplot(1:10, w/(w+b), pch='', xlab='clusters', ylab='within/tot', main='Choice of k', ylim=c(0,1))
lines(1:10, w/(w+b), type='b', lwd=2)
# if you don't see an elbow it can indicate that you do not have a clear clustering structure
# and then you try



# DBSCAN --------------------------------------------------------------------------------------
# YOU CAN USE DBSCAN ALSO ON DISTANCE MATRIX
# optimal choice per non convex datasets
# Choice of hyperparameters for DBSCAN
# Rule of thumb, minPts should be at least p + 1 = 3 here
# Can be eventually increased
minPts <- 3

# How to choose eps from minPts?
# Plot of the distances to the minPts-1 nearest neighbor
# sort of measure of density
# at the elbow point you start to have point far from their 2NN
kNNdistplot(data, minPts = minPts)
# Taking eps = 0.05 seems to be a good threshold
abline(h = 0.05, col = "red", lty = 2)
# all the points below the line will be conisdered core points with minPts=3

# Run the dbscan
dbs <- dbscan(data, eps = 0.05, minPts = 3)
dbs

# plot of the resulting
plot(data, col = dbs$cluster + 1L, pch=19)

# to choose epsilon and minPts we use a quality measure of the clustering structure -> silhouette
help(silhouette)

clustered_index <- which(dbs$cluster != 0) # Index of non noise points
clustered_points <- data[clustered_index] # only clustered points
clustered_labels <- dbs$cluster[clustered_index] # corresponding labels


sil_score <- function(labels, dist) {
  # Compute the average of the silhouette widths
  sil <- silhouette(labels, dist)
  sil_widths <- sil[,"sil_width"]
  mean(sil_widths)
}

# we get a minus 0 silhouette score even if our cluter structure it's not bad
sil_score(clustered_labels, dist(clustered_points))

# Grid Search
minPts_grid <- 1:20
eps_grid <- seq(0.01, 0.2, by = 0.01)

max_share_noise <- 0.2

dbscan_perf <- function(minPts, eps) {
  # Compute the silhouette score resulting from dbscan clustering
  dbs <- dbscan(data, eps, minPts) # Run dbscan
  
  clustered_index <- which(dbs$cluster != 0) # Index of non noise points
  clustered_points <- x[clustered_index] # only clustered points
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

# Retrieve the best parameters
max_score <- max(perf_grid)
argmax_score <- which(perf_grid == max_score, arr.ind = TRUE)
best_eps <- eps_grid[argmax_score[2]]
best_minPts <- minPts_grid[argmax_score[1]]
best_eps
best_minPts
max_score

#MULTIDIMENSIONAL SCALING----------------------------------------------------------
# the distance matrix
distance

# k dimension of space in which you want to embed your data
mdscale <- cmdscale(distance, k=2)
mdscale

# I have to set asp=1 (equal scales on the two axes)
# to correctly represent Euclidean distances
plot(mdscale[,1], mdscale[,2], type='n', asp=1, xlab='',ylab='')
text(mdscale[,1], mdscale[,2], labels=colnames(as.matrix(eurodist)), cex = 0.75, pos = 3)
# use points if text does not matter

# compare the original matrix d_ij = d(x_i,x_j) and delta_ij = d(y_i,y_j) 
plot(eurodist, dist(mdscale))

# Compute the "stress": the higher it is, the worse
# the matching between original distances and their
# geometrical representation through MDS
Stressk <- NULL
for(k in 1:4)
{
  mdscale.k <- cmdscale(distance, k)
  Stress <- (sum( (as.vector(distance) - as.vector(dist(mdscale.k)))^2)  /
               sum( as.vector(mdscale.k)^2))^(1/2)
  Stressk <- c(Stressk, Stress) 
}

plot(1:4,Stressk,xlab='k',ylab='Stress',lwd=2)
# the stress increases for k>2 because of numerical problems 
# (the representation of minimal stress is not always found)

# AFTER THE CLUSTERING------------------------------------------------------------------
# You have two populations -> (M)ANOVA 2-way
###Test to prove the existence of a difference in the mean value of the two clustters-----

p  <- dim(data)[2]
n1 <- table(clusters)[1]
n2 <- table(clusters)[2]

# Verify gaussianity
mvn(data[clusters=='1',])$multivariateNormality
mvn(data[clusters=='2',])$multivariateNormality

# Test for independent Gaussian populations
t1.mean <- sapply(data[clusters=='1',],mean)
t2.mean <- sapply(data[clusters=='2',],mean)
t1.cov  <-  cov(data[clusters=='1',])
t2.cov  <-  cov(data[clusters=='2',])
Sp      <- ((n1-1)*t1.cov + (n2-1)*t2.cov)/(n1+n2-2)

# Test: H0: mu.1-mu.2==0 vs H1: mu.1-mu.2!=0

delta.0 <- c(0,0,0,0,0) # change it according to the number of variables p
alpha <- 0.1
Spinv   <- solve(Sp)
T2 <- n1*n2/(n1+n2) * (t1.mean-t2.mean-delta.0) %*% Spinv %*% (t1.mean-t2.mean-delta.0)
cfr.fisher <- (p*(n1+n2-2)/(n1+n2-1-p))*qf(1-alpha, p, n1+n2-1-p)

P <- 1 - pf(T2/(p*(n1+n2-2)/(n1+n2-1-p)), p, n1+n2-1-p)
P

### Bonferroni intervals for the components of the mean----------------------------------
k <- p
alpha <- 0.1
IC <- cbind(t2.mean-t1.mean - sqrt(diag(Sp)*(1/n1+1/n2)) * qt(1 - alpha/(k*2), n1+n2-2),
            t2.mean-t1.mean,
            t2.mean-t1.mean + sqrt(diag(Sp)*(1/n1+1/n2)) * qt(1 - alpha/(k*2), n1+n2-2))
IC

###Confidence region for the RELATIVE position of the center of the two clusters-----------------
# Characterize the ellipse:
# Directions of the axes
eigen(Sp)$vector

# Radius
r <- sqrt(cfr.fisher)

# Length of the semi-axes
r*sqrt(eigen(Sp)$values*(1/n1+1/n2))

# plot
par(mfrow=c(1,2))
plot(satellite , col=clusters+1, asp=1, pch=16, main='Original data and groups')
plot(satellite, pch='', asp=1, 
     main='Elliptic region for the mean diff. (red - green)')
# confidence region and sample mean in blue
ellipse(center=t1.mean-t2.mean, shape=Sp*(1/n1+1/n2), radius=sqrt(cfr.fisher), 
        lwd=2, col='blue')
par(mfrow=c(1,1))

###Confidence region containing 1-alpha% of the observations of ONE group------------------
onegroup<-stones[which(cl.ew==2),]
n <- dim(onegroup)[1]
p <-dim(onegroup)[2]

plot(data)
points(onegroup, pch=19)

# Verify gaussian assumptions
mvn(onegroup)$multivariateNormality

M <- sapply(onegroup, mean)
S <- cov(onegroup)
alpha <- 0.01
cfr.chisq <- qchisq(1-alpha,p)

# Characterize the ellipse:
# Axes directions:
eigen(S)$vectors
# Center:
M
# Radius of the ellipse:
r <- sqrt(cfr.chisq)
# Length of the semi-axes:
r*sqrt(eigen(S)$values)  

plot(onegroup, asp = 1, col='gold', pch=19, xlim=c(-10,50))
points(M[1], M[2], pch = 4, cex = 1.5, lwd = 2)

ellipse(center=M, shape=S, radius=sqrt(cfr.chisq), col = 'black', lty = 2, center.pch = 4)
points(stones[which(cl.ew==1),])

###Bonferroni for different linear combinations of the difference of the mean--------------

g1 <- data[cluster==1,]
g2 <- data[cluster==2,]
g1
g2

# Test: H0: mu.1-mu.2==0 vs H1: mu.1-mu.2!=0
p  <- 2
n1 <- dim(g1)[1]
n2 <- dim(g2)[1]
alpha <- 0.10

mean1 <- sapply(g1,mean)
mean2 <- sapply(g2,mean)
cov1  <-  cov(g1)
cov2  <-  cov(g2)
Sp      <- ((n1-1)*cov1 + (n2-1)*cov2)/(n1+n2-2)

delta.0 <- c(0,0)
Spinv   <- solve(Sp)

T2 <- n1*n2/(n1+n2) * (mean1-mean2-delta.0) %*% Spinv %*% (mean1-mean2-delta.0)

cfr.fisher <- (p*(n1+n2-2)/(n1+n2-1-p))*qf(1-alpha,p,n1+n2-1-p)

pvalue <- 1 - pf(T2/(p*(n1+n2-2)/(n1+n2-1-p)), p, n1+n2-1-p)
pvalue

### Bonferronis
# In this case:
#     - The difference of the mean of the variable 1
#     - The difference of the mean of the variable 2
#     - The difference of the mean of the sum of the variables 1 and 2
#     - The difference of the mean of the difference of variable 1 and
#       2

dm <- (mean1-mean2)
A  <- rbind(c(1,0), c(0,1), c(1,1), c(1,-1))
k  <- dim(A)[1]

A.s2 <- diag(A%*%Sp%*%t(A))
A.dm <- A%*%(mean1-mean2)

Bonf <- cbind(inf=A.dm - qt(1-(alpha/(2*k)), n1+n2-2) * sqrt( A.s2*(1/n1+1/n2) ), 
              center=A.dm, 
              sup=A.dm + qt(1-(alpha/(2*k)), n1+n2-2) * sqrt( A.s2*(1/n1+1/n2) ))
Bonf

### question (d)
mvn(g1)$multivariateNormality
mvn(g2)$multivariateNormality
# Verifying assumptions
cov1
cov2
