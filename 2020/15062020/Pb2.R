setwd("C:/Users/Giuseppe/Documents/.magistrale/secondo anno/AppStat/Exams/2020/20200615/20200615")
data <- read.table("stoneflakes.txt",header=T)

library(mvtnorm)
library(MVN)
library(rgl)
library(car)
library(dbscan)
library(cluster)
library(fields)

plot(data,pch=19)

# stabilize the ordering of the data
misc <- sample(dim(data)[1])
data <- data[misc,]



data.e <- dist(data, method='euclidean')
data.ew <- hclust(data.e, method='ward.D2')

# plot dendrograms
{
  
  plot(data.ew, main='euclidean-ward', hang=-0.1, xlab='', labels=F, cex=0.6, sub='')
  par(mfrow=c(1,1))
  
  # Is there chaining effect for single linkage? Adding single points to clusters iteratively
  
  
  # The clearer the clustering structure in the data, the higher separations between merges
  # how many cluster do you want? fixed for k=2
  # At parity of heigth, consider how balanced the clusters are

  plot(data.ew, main='euclidean-ward', hang=-0.1, xlab='', labels=F, cex=0.6, sub='')
  rect.hclust(data.ew, k=3)
  par(mfrow=c(1,1))
}

# Cut the dendrogram: generate labels for each point
{
  cluster.ew <- cutree(data.ew, k=3) # euclidean-ward
}
# MANOVA after clustering: add the grouping to data, then do MANOVA as usual
data$label <- cluster.ew





p = dim(data)[2] - 1  # -1: remove the grouping structure if exists, otherwise just 
# the number of features

# Preprocess: depending on where is the label
{
  # rename features with X1,X2,X3,X4
  v = 'X1'
  for(i in 2:p){
    v = c(v, paste('X',i,sep=''))
  }
  #rename as label the last column (may be in the first column)
  colnames(data) = c(v, 'label')
  head(data)
  # Consider only features: see where they start and where ends!!
  data.feats = data[,1:p]       
  
}

n     = length(data$label)    # total number of observations
ng    = table(data$label)     # number of observations in each group
treat = levels(factor(data$label))   # levels of the treatment (setosa, ecc..ecc..)
g     = length(treat)         # number of levels (i.e. of groups) (number of treat)



# Explore dataset and get index of treatments
{
  colors <- rainbow(length(treat))
  color_map <- setNames(colors, treat)
  plot(data.feats,pch=19,col = color_map[data$label])
  
  # g indexes to get the treatments (or classes)
  i1 = which(data$label == treat[1])
  i2 = which(data$label == treat[2])
  i3 = which(data$label == treat[3])
  ### ...... continue/remove if needed
  n1 = length(i1)
  n2 = length(i2)
  n3 = length(i3)
  ### ...... continue/remove if needed
  n = n1 + n2 + n3  ### ...... continue/remove if needed
}



# 1) normality
{
  Ps = NULL
  for(i in 1:g){
    mvn.test <- mvn(data.feats[get(paste('i',i, sep='')), 1:p])
    Ps <- c(Ps, mvn.test$multivariateNormality$`p value`)
  }
  Ps
}

# 2) same covariance structure (homoschedasticity) (= same covariance matrix Sigma)
{
  S  = cov(data.feats)
  S1 = cov(data.feats[i1,])
  S2 = cov(data.feats[i2,])
  S3 = cov(data.feats[i3,])
  
  
  # Quantitatively:
  round(S1, digits=1)
  round(S2, digits=1)
  round(S3, digits=1)
  
  # heat map of covariances
  par(mfrow=c(1,3))
  image(S1, col=heat.colors(100),main='Cov. S1', asp=1, axes = FALSE, breaks = quantile(rbind(S1,S2,S3), (0:100)/100, na.rm=TRUE))
  image(S2, col=heat.colors(100),main='Cov. S2', asp=1, axes = FALSE, breaks = quantile(rbind(S1,S2,S3), (0:100)/100, na.rm=TRUE))
  image(S3, col=heat.colors(100),main='Cov. S3', asp=1, axes = FALSE, breaks = quantile(rbind(S1,S2,S3), (0:100)/100, na.rm=TRUE))
}


fit = manova(as.matrix(data.feats) ~ data$label )
summary.manova(fit)
# Exact tests (for Wilks) if p<=2 or g<=3
# -> yes, there is statistical evidence 


# estimates of the parameters
fit$coefficients
# Sigma
W <- summary.manova(fit)$SS$Residuals
W



# CI of the group effects (tau_i - tau_j): every pair of feature for every pair of groups
alpha = 0.1
k <- p*g*(g-1)/2
qT <- qt(1-alpha/(2*k), n-g)

W <- summary.manova(fit)$SS$Residuals

m  <- sapply(data.feats, mean)         # estimates mu 
m1 <- sapply(data.feats[i1,], mean)    # estimates mu.1 = mu + tau.1
m2 <- sapply(data.feats[i2,], mean)    # estimates mu.2 = mu + tau.2
m3 <- sapply(data.feats[i3,], mean)    # estimates mu.3 = mu + tau.3

inf12 <- m1 - m2 - qT * sqrt(diag(W)/(n-g) * (1/n1+1/n2))
sup12 <- m1 - m2 + qT * sqrt(diag(W)/(n-g) * (1/n1+1/n2))
inf13 <- m1 - m3 - qT * sqrt(diag(W)/(n-g) * (1/n1+1/n3))
sup13 <- m1 - m3 + qT * sqrt(diag(W)/(n-g) * (1/n1+1/n3))
#inf14 <- m1 - m4 - qT * sqrt(diag(W)/(n-g) * (1/n1+1/n4))
#sup14 <- m1 - m4 + qT * sqrt(diag(W)/(n-g) * (1/n1+1/n4))
inf23 <- m2 - m3 - qT * sqrt(diag(W)/(n-g) * (1/n2+1/n3))
sup23 <- m2 - m3 + qT * sqrt(diag(W)/(n-g) * (1/n2+1/n3))
#inf24 <- m2 - m4 - qT * sqrt(diag(W)/(n-g) * (1/n2+1/n4))
#sup24 <- m2 - m4 + qT * sqrt(diag(W)/(n-g) * (1/n2+1/n4))
#inf34 <- m3 - m4 - qT * sqrt(diag(W)/(n-g) * (1/n3+1/n4))
#sup34 <- m3 - m4 + qT * sqrt(diag(W)/(n-g) * (1/n3+1/n4))

CI <- list(g1_g2 = cbind(inf12, sup12),
           g1_g3 = cbind(inf13, sup13),
           g2_g3 = cbind(inf23, sup23))
CI

# being in g2 means having higher width than g1, but comparable length
# being in g1 means having higher length than g3, but comparable width
# g3 elements are smaller than g2's


