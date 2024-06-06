setwd("C:/Users/Giuseppe/Documents/.magistrale/secondo anno/AppStat/Exams/2020/20200717/20200717")
library(MASS)
library(class)
library(rgl)
library(mvtnorm)
library(MVN)
library(e1071)
library(heplots)

options(rgl.printRglwidget = TRUE)
data <- read.table("occupancy.txt",header=T)

par(mfrow=c(1,1))
p <- dim(data)[2] - 1 # no categorical variable
par(mfrow=c(1,1))
plot(data[,1:p],pch=19)
head(data)
colnames(data) <- c("feature1","feature2","group")


groups.name <- factor(data$group)
g = length(levels(groups.name)) 
g
# Do accordingly to how many g
i1 <- which(groups.name == levels(groups.name)[1])
i2 <- which(groups.name == levels(groups.name)[2])

# Do accordingly to how many g
n1 <- length(i1)
n2 <- length(i2)

n <- n1 + n2 

data.feats <- data[,1:p] 


true <- 1 # occupied
false <- 0  # not occupied

#prior probabilities
pt <- 9/24
pf <- 1-pt
prior = c(pf, pt)
prior


# Estimates and MANOVA to see if there is some difference in the groups
{
  m <-  colMeans(data[,1:p])
  m1 <- colMeans(data[i1, 1:p])
  m2 <- colMeans(data[i2, 1:p])
  
  S1 <- cov(data[i1, 1:p])
  S2 <- cov(data[i2, 1:p])
  Sp  <- ((n1 - 1) * S1 + (n2 - 1) * S2  ) / (n - g)
  
  # One-way MANOVA 
  fit <- manova(as.matrix(data.feats) ~ groups.name)
  summary.manova(fit, test="Wilks")
  # low pval -> there is difference in groups
}

# Assumptions
{
  # multivariate normality in each group
  mvn(data[which(data$group == levels(groups.name)[1]), 1:dim(data.feats)[2]])$multivariateNormality
  mvn(data[which(data$group == levels(groups.name)[2]), 1:dim(data.feats)[2]])$multivariateNormality
  
  # if there is normality
  boxM(data.feats, groups.name)
  
  # if no normality: the ratio of the S1's element wise on diagonal must be < 4
  #                  out of diagonal < 10
  
  # Qualitatively, boxplot
  {
    par(mfrow=c(1,length(levels(groups.name))))
    boxplot(scale(data[which(data$group == levels(groups.name)[1]),1:p], center = T, scale = F))
    boxplot(scale(data[which(data$group == levels(groups.name)[2]),1:p], center = T, scale = F))
    par(mfrow=c(1,1))
  }
  
  # Qualitatively,S
  {
    S1 <- cov(data.feats[i1,])
    S2 <- cov(data.feats[i2,])
    S1
    S2
    
    # heat map of covariances
    par(mfrow=c(1,g))
    image(S1, col=heat.colors(100),main='Cov. S1', asp=1, axes = FALSE, breaks = quantile(rbind(S1,S2), (0:100)/100, na.rm=TRUE))
    image(S2, col=heat.colors(100),main='Cov. S2', asp=1, axes = FALSE, breaks = quantile(rbind(S1,S2), (0:100)/100, na.rm=TRUE))
    par(mfrow=c(1,1))
  }
}
par(mfrow=c(1,1))
plot(data[,1:p], col = ifelse(data$group == "0", "blue","red"),pch=19)
# too different, boxM reject, and shapes are very different in plot



qda.data <- qda(data.feats, groups.name, prior=prior
                # add priors if given
) 
qda.data




# Estimate of parameters
{
  # Means of parameters
  qda.data$means
  
  # Covariance matrix 
  cov(data[which(data$group == '0'), 1:p])
  cov(data[which(data$group == '1'), 1:p])
}


# Plot the 2D partition induced by QDA
{
  plot(data.feats, main='dataset', xlab='X1', ylab='X2', pch=20)
  points(data.feats[i1,], col='red', pch=20)
  points(data.feats[i2,], col='green', pch=20)
  legend("topright", legend=levels(groups.name), fill=c('red','green','blue'))
  
  points(qda.data$means, col=c('red','green','blue'), pch=4, lwd=2, cex=1.5)
  
  x  <- seq(min(data.feats[,1]), max(data.feats[,1]), length=200)
  y  <- seq(min(data.feats[,2]), max(data.feats[,2]), length=200)
  
  # rename this, don't use feature1 feature2, but the true name for features
  xy <- expand.grid(feature1=x, feature2=y)
  
  # if g = 2
  {
    z  <- predict(qda.data, xy)$post  
    z1 <- z[,1] - pmax(z[,2])    
    z2 <- z[,2] - pmax(z[,1])    
  }
  
  contour(x, y, matrix(z1, 200), levels=0, drawlabels=F, add=T)
  contour(x, y, matrix(z2, 200), levels=0, drawlabels=F, add=T)
}




# With custom prios
# IMPORTANT: always use the priors without the cost information!!!
{
  inference_train <- predict(qda.data)
  G <- g
  misc <- table(class.true=groups.name, class.assigned=inference_train$class)
  APER <- 0
  for(g in 1:G)
    APER <- APER + sum(misc[g,-g])/sum(misc[g,]) * prior[g]  
  APER
  
}
# Predict on new entry
{
  Z0 <- data.frame(feature1=26., feature2=9)
  predict(qda.data, Z0) #-> occupated with 99%
}


#plot the region
{
  k <- 5
  plot(data.feats, main='Dataset', xlab='X1', ylab='X2', pch=20)
  points(data.feats[i1,], col=2, pch=20)
  points(data.feats[i3,], col=4, pch=20)
  legend("topright", legend=levels(groups.name), fill=c(2,3,4))
  
  x  <- seq(min(data.feats[,1]), max(data.feats[,1]), length=200)
  y  <- seq(min(data.feats[,2]), max(data.feats[,2]), length=200)
  
  # don't use feature1 feature2, but the true names
  xy <- expand.grid(feature1=x, feature2=y)
  
  data.knn <- knn(train = data.feats, test = xy, cl = data$group, k = k)
  
  z  <- as.numeric(data.knn)
  
  contour(x, y, matrix(z, 200), levels=c(1.5, 2.5), drawlabels=F, add=T)
}

# APER KNN
# fix the K in the for!
{
  errorknn <- 0
  for(i in 1:nrow(data)) {                     
    data.knn <- knn(train = data[,1:2], test = data[i,1:2], cl = data[,3], k = k)
    if(data.knn != data[i,3])
      errorknn <- errorknn + 1
  }
  APERknn <- errorknn/nrow(data)
  APERknn 
}



