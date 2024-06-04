setwd("C:/Users/Giuseppe/Documents/.magistrale/secondo anno/AppStat/Exams/2021/20210906/20210906")

library(mvtnorm)
library(MVN)
library(rgl)
library(car)
library(dbscan)
library(cluster)
library(fields)
data <- read.table("pinnanobilis.txt",header=T)
head(data)
dim(data)



# stabilize the ordering of the data
misc <- sample(dim(data)[1])
data <- data[misc,]
data.e <- dist(data, method='euclidean')
data.ec <- hclust(data.e, method='complete')


# plot dendrograms
{
  
  plot(data.ec, main='euclidean-complete', hang=-0.1, xlab='', labels=F, cex=0.6, sub='')
  par(mfrow=c(1,1))
  
  

  plot(data.ec, main='euclidean-complete', hang=-0.1, xlab='', labels=F, cex=0.6, sub='')
  rect.hclust(data.ec, k=2)
}

# Cut the dendrogram: generate labels for each point
{
  cluster.ec <- cutree(data.ec, k=2) # euclidean-complete:

}

# Plot the clusters 
{
  plot(data, col=cluster.ec +1, pch=19, main='Complete linkage')
  
  # 3d plots if there are 3 features
  # plot3d(data, size=3, col=cluster.ew+1, aspect = F)
}

# Size of clusters
{
  length(which(cluster.ec ==1))
  length(which(cluster.ec ==2))
}


# Centroid of the clusters
{
  colMeans(data[cluster.ec == 1, ])
  colMeans(data[cluster.ec == 2, ])
  
  plot(data, col=cluster.ec +1, pch=19, main='Complete linkage')
  points(colMeans(data[cluster.ec == 1, ])[1],colMeans(data[cluster.ec == 1, ])[2], pch=17,col="black")
  points(colMeans(data[cluster.ec == 2, ])[1],colMeans(data[cluster.ec == 2, ])[2], pch=17,col="black")
}


data$label <- cluster.ec
# one way MANOVA


p = dim(data)[2] - 1  # -1: remove the grouping structure if exists, otherwise just 
# the number of features

# Preprocess: depending on where is the label
{
  # rename features with X1,X2,X3,X4
  v = 'X1'
  for(i in 2:p){
    v = c(v, paste('X',i,sep=''))
  }
  
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
  n1 = length(i1)
  n2 = length(i2)
  n = n1 + n2
}


# Assumptions
{
  # 1) normality
  {
    Ps = NULL
    for(i in 1:g){
      mvn.test <- mvn(data.feats[get(paste('i',i, sep='')), 1:p])
      Ps <- c(Ps, mvn.test$multivariateNormality$`p value`)
    }
    Ps
    # high pval
  }
  
  # 2) same covariance structure (homoschedasticity) (= same covariance matrix Sigma)
  # we was told in the question to consider them equal
}


# H0: The membership to a class don't have any significant effect on the mean
#     of X.ij (in any direction of R^p) 
# H1: There is at least one direction in R^p along which at least two classes
#     have some feature significantly different
fit = manova(as.matrix(data.feats) ~ data$label )
summary.manova(fit)
# yes signficance

# estimates of the parameters
fit$coefficients
# Sigma
W <- summary.manova(fit)$SS$Residuals
W





# Bonferroni for treatment effect (mean difference)
# We want to know the levels (of labels) that induce the difference
{
  # CI of the group effects (tau_i - tau_j): every pair of feature for every pair of groups
  alpha = 0.1
  k <- p*g*(g-1)/2
  qT <- qt(1-alpha/(2*k), n-g)
  
  W <- summary.manova(fit)$SS$Residuals
  # (if one way replace data.feats with measure, and then measure[i1] removing ,)
  m  <- sapply(data.feats, mean)         # estimates mu 
  m1 <- sapply(data.feats[i1,], mean)    # estimates mu.1 = mu + tau.1
  m2 <- sapply(data.feats[i2,], mean)    # estimates mu.2 = mu + tau.2

  
  inf12 <- m1 - m2 - qT * sqrt(diag(W)/(n-g) * (1/n1+1/n2))
  sup12 <- m1 - m2 + qT * sqrt(diag(W)/(n-g) * (1/n1+1/n2))
  
  CI <- list(g1_g2 = cbind(inf12, sup12))
  CI
}
par(mfrow=c(1,1))
plot(data[,1:2],col=cluster.ec+1,pch=19)
#           0 is not inside -> both the heigth and width are greater in group 1 than 2
#           this difference is significative






