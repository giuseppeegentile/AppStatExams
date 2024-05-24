setwd("C:/Users/Giuseppe/Documents/.magistrale/secondo anno/AppStat/Exams/2022/20220712/20220712")
data <- read.table("demogorgons.txt",header=T)

library(mvtnorm)
library(MVN)
library(rgl)
library(car)
library(dbscan)
library(cluster)
library(fields)
head(data)
dim(data)
plot(data,pch=19)

# stabilize the ordering of the data
misc <- sample(dim(data)[1])
data <- data[misc,]


data.e <- dist(data, method='euclidean')
data.ea <- hclust(data.e, method='average')
plot(data.ea, main='euclidean-average', hang=-0.1, xlab='', labels=F, cex=0.6, sub='')
rect.hclust(data.ea, k=2)
cluster.ea <- cutree(data.ea, k=2) 
plot(data, col=cluster.ea +1, pch=19, main='Average linkage')

data <- data.frame(data,cluster.ea)

p = dim(data)[2] - 1
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
g     = length(treat)  


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
    
    # satisfied
  }
  
  # 2) same covariance structure (homoschedasticity) (= same covariance matrix Sigma)
  {
    S  = cov(data.feats)
    S1 = cov(data.feats[i1,])
    S2 = cov(data.feats[i2,])
    
    
    # Quantitatively:
    round(S1, digits=1)
    round(S2, digits=1)
    
    # heat map of covariances
    par(mfrow=c(1,2))
    image(S1, col=heat.colors(100),main='Cov. S1', asp=1, axes = FALSE, breaks = quantile(rbind(S1,S2), (0:100)/100, na.rm=TRUE))
    image(S2, col=heat.colors(100),main='Cov. S2', asp=1, axes = FALSE, breaks = quantile(rbind(S1,S2), (0:100)/100, na.rm=TRUE))
    
    
    
    
  }
}
fit = manova(as.matrix(data.feats) ~ data$label )
summary.manova(fit)
# the clustering structure clearly affect the position of the demogorgon

fit$coefficients


# Bonferroni for treatment effect (mean difference)
# We want to know the levels (of labels) that induce the difference
{
  # CI of the group effects (tau_i - tau_j): every pair of feature for every pair of groups
  alpha = 0.05
  k <- p*g*(g-1)/2
  qT <- qt(1-alpha/(2*k), n-g)
  
  W <- summary.manova(fit)$SS$Residuals
  W
  # (if one way replace data.feats with measure, and then measure[i1] removing ,)
  m  <- sapply(data.feats, mean)         # estimates mu 
  m1 <- sapply(data.feats[i1,], mean)    # estimates mu.1 = mu + tau.1
  m2 <- sapply(data.feats[i2,], mean)    # estimates mu.2 = mu + tau.2
  
  inf12 <- m1 - m2 - qT * sqrt(diag(W)/(n-g) * (1/n1+1/n2))
  sup12 <- m1 - m2 + qT * sqrt(diag(W)/(n-g) * (1/n1+1/n2))
  
  CI <- list(g1_g2 = cbind(inf12, sup12))
  CI
}


# Size of clusters
{
  length(which(cluster.ea ==1))
  length(which(cluster.ea ==2))
}


# Centroid of the clusters
{
  colMeans(data[cluster.ea == 1, ])
  colMeans(data[cluster.ea == 2, ])
  
  
  plot(data[,1:2], col=ifelse(cluster.ea==1,'red','blue'), pch=20, main='Complete linkage')
  points(colMeans(data[cluster.ea == 1, ])[1],colMeans(data[cluster.ea == 1, ])[2], pch=17,col="black")
  points(colMeans(data[cluster.ea == 2, ])[1],colMeans(data[cluster.ea == 2, ])[2], pch=17,col="black")
  
}



D <- data[cluster.ea==1,1:2]

n <- dim(D)[1]
p <- dim(D)[2]

# Assumption: normality for the new dataset (not for the old one!)
result <- mvn(D)
result$multivariateNormality
# high p-val -> normality
# However, even if the pval is not so low, is ok to proceed assuming normality


# T2 Hotelling Test H0:  delta.0 = (0,0)
{
  D.mean   <- sapply(D, mean) 
  # if D.mean and delta.0 incompatible -> D.mean   <- colMeans(D)
  D.cov    <- cov(D)
  D.invcov <- solve(D.cov)
  
  alpha   <- .05
  delta.0 <- c(0, 0)
  
  D.T2 <- n * (D.mean - delta.0) %*% D.invcov %*% (D.mean - delta.0)
  D.T2
  par(mfrow=c(1,1))
  
  alpha   <- .05
  cfr.fisher <- ((n - 1) * p / (n - p)) * qf(1 - alpha, p, n - p)
  plot(data[,1:2], asp=1, pch=19, main='Dataset of the Differences')
  ellipse(center=D.mean, shape=D.cov/n, radius=sqrt(cfr.fisher), lwd=2)
  
}

D2 <- data[cluster.ea==2,1:2]

n <- dim(D2)[1]
p <- dim(D2)[2]

# Assumption: normality for the new dataset (not for the old one!)
result <- mvn(D2)
result$multivariateNormality
# high p-val -> normality
# However, even if the pval is not so low, is ok to proceed assuming normality


# T2 Hotelling Test H0:  delta.0 = (0,0)
{
  D2.mean   <- sapply(D2, mean) 
  # if D.mean and delta.0 incompatible -> D.mean   <- colMeans(D)
  D2.cov    <- cov(D2)
  D2.invcov <- solve(D2.cov)
  
  
  D.T2 <- n * (D.mean - delta.0) %*% D.invcov %*% (D.mean - delta.0)
  D.T2
  par(mfrow=c(1,1))
  
  alpha   <- .05
  cfr.fisher2 <- ((n - 1) * p / (n - p)) * qf(1 - alpha, p, n - p)
  ellipse(center=D2.mean, shape=D2.cov/n, radius=sqrt(cfr.fisher2), lwd=2)
  
}
