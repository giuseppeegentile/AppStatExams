setwd("C:/Users/Giuseppe/Documents/.magistrale/secondo anno/AppStat/Exams/2019/20190719/20190719")
library(mvtnorm)
library(MVN)
library(rgl)
library(car)
library(dbscan)
library(cluster)
library(fields)


orig_data <- read.table('buoys.txt', header = T)

head(orig_data)
data <- orig_data[,1:2]
# stabilize the ordering of the data
misc <- sample(dim(data)[1])
data <- data[misc,]
data.e <- dist(data, method='euclidean')
data.ew <- hclust(data.e, method='ward.D2')
plot(data,pch=19)
# plot dendrograms
{

  plot(data.ew, main='euclidean-ward', hang=-0.1, xlab='', labels=F, cex=0.6, sub='')
  rect.hclust(data.ew, k=3)
  par(mfrow=c(1,1))
}
cluster.ew <- cutree(data.ew, k=3) # euclidean-ward

plot(data,col=as.factor(cluster.ew + 1), pch=19)


# Centroid of the clusters
{
  colMeans(data[cluster.ew == 1, ])
  colMeans(data[cluster.ew == 2, ])
  colMeans(data[cluster.ew == 3, ])
  
 # Long      Lat 
 # 14.68326 42.50844 
 # Long      Lat 
 # 14.57181 42.44749
 # Long      Lat 
 # 14.41417 42.41184
  
  dim(data[cluster.ew == 1, ])[1] # 331
  dim(data[cluster.ew == 2, ])[1] # 245
  dim(data[cluster.ew == 3, ])[1] # 176
}

# is a one way anova, with the treatment as the cluster
 data <- data.frame(orig_data$DO, as.factor(cluster.ew))
 
 # preprocess depending on the column indexes
 {
   colnames(data) = c('measure', 'label')
 }

 head(data)
 
 
 n       <- dim(data)[1]      # total observations, LEVELS
 ng      <- table(data$label)   # observations in each group
 # treat   <- levels(data$label)  
 treat   <- levels(factor(data$label)) # levels of the treatment
 g       <- length(treat)     # number of levels (of groups)
 
 
 #Assumptions:
 {
   # 1) normality (univariate) in each group
   {
     Ps = 0*(1:g)
     for(i in 1:g){
       Ps[i] = shapiro.test(data$measure[data$label==treat[i]])$p
     }
     Ps
     # 0.1081403 0.7749549 0.7163759
     # -> we can't reject normality assumption -> assumption satisfied
   }
   
   # 2) Same covariance structure (same sigma^2)
   {
     # WARNING: Test very sensitive to departures from normality (low robustness),
     # you need normality to make this test 
     Var = 0*(1:g)
     for(i in 1:g){
       Var[i] = var(data$measure[data$label==treat[i]])
     }
     Var
     bartlett.test(data$measure, data$label) # H0: sigma.1 = .. = sigma.g
     # pval > 5% can't reject at 1% -> assumptions satisfied
   }
 }
 
 fit = aov(data$measure ~ data$label)
 summary(fit)
# no significant effect, pval  =12%
 
 

