setwd("C:/Users/Giuseppe/Documents/.magistrale/secondo anno/AppStat/Exams/2019/20190912/20190912")
library(MASS)
library(class)
library(rgl)
library(mvtnorm)
library(MVN)
library(e1071)
library(heplots)

options(rgl.printRglwidget = TRUE)
data <- read.table("debris.txt",header=T)
plot(data[,1:2], pch=19, col=factor(data[,3]))




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

data.feats <- data[,1:p] # in this case features are before



# Assumptions
{
  # multivariate normality in each group
  mvn(data[which(data$group == levels(groups.name)[1]), 1:dim(data.feats)[2]])$multivariateNormality
  mvn(data[which(data$group == levels(groups.name)[2]), 1:dim(data.feats)[2]])$multivariateNormality
  # no normality for seccond group, as expected from plot
  
  
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
    15.6631038/3
  }
}
par(mfrow=c(1,1))
plot(data[,1:2], pch=19, col=factor(data$group))
# we use QDA in this case, not so similar covariances, and boundaries not linear
# also we have 300 observations, split as
n1
n2
# we can estimate the covariances with such amount of data




qda.data <- qda(data.feats, groups.name
                #, prior=prior.c
)
qda.data




# Estimate of parameters
{
  # Means of parameters
  qda.data$means
  
  # Covariance matrix 
  cov(data[which(data$group == levels(factor(groups.name))[1]), 1:p])
  cov(data[which(data$group == levels(factor(groups.name))[2]), 1:p])
}


# Leave one out CV
{
  
  # With default priors 
  {
    QdaCV.data <- qda(data.feats, groups.name, CV=T)
    misc <- table(class.true=groups.name, class.assignedCV=QdaCV.data$class)
    errorsqCV <- (QdaCV.data$class != groups.name)
    AERqCV   <- sum(errorsqCV)/length(groups.name)
    AERqCV
  }
} # 0.13

# Plot the 2D partition induced by QDA
{
  plot(data.feats, main='dataset', xlab='X1', ylab='X2', pch=20)
  points(data.feats[i1,], col='red', pch=20)
  points(data.feats[i2,], col='green', pch=20)
  points(data.feats[i3,], col='blue', pch=20)
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
# weakness of the model: we're using priors estimated from training set, we had not prior knowledge of the 
# problem: overfit prone

#plot the region
{
  k <- 7
  plot(data.feats, main='Dataset', xlab='X1', ylab='X2', pch=20)
  points(data.feats[i1,], col=2, pch=20)
  points(data.feats[i3,], col=4, pch=20)
  points(data.feats[i2,], col=3, pch=20)
  legend("topright", legend=levels(groups.name), fill=c(2,3,4))
  
  x  <- seq(min(data.feats[,1]), max(data.feats[,1]), length=200)
  y  <- seq(min(data.feats[,2]), max(data.feats[,2]), length=200)
  
  # don't use feature1 feature2, but the true names
  xy <- expand.grid(feature1=x, feature2=y)
  
  data.knn <- knn(train = data.feats, test = xy, cl = data$group, k = k)
  
  z  <- as.numeric(data.knn)
  
  contour(x, y, matrix(z, 200), levels=c(1.5, 2.5), drawlabels=F, add=T)
}

# choose best k by leave one out cross validation 
{
  set.seed(321)
  optimal_k <- 0
  min_error_rate <- Inf
  
  for (k in 10:30) {
    error_rate <- 0
    
    # Perform leave-one-out cross-validation
    for (i in 1:nrow(data)) {
      train_data <- data[-i,1:2 ]  # Training data (excluding the i-th sample)
      test_data <- data[i, 1:2]    # Test data (i-th sample)
      train_target <- data[-1,ncol(data)]
      
      # Apply k-nearest neighbors classification
      data.knn <- knn(train = train_data[, 1:2], test = test_data[, 1:2], cl = train_target, k = k)
      
      # Calculate misclassification error
      if (data.knn != data[i,3]) {
        error_rate <- error_rate + 1
      }
    }
    
    # Update optimal k if the error rate is lower
    if (error_rate < min_error_rate) {
      optimal_k <- k
      min_error_rate <- error_rate
    }
  }
  optimal_k
  #error rate (AER by CV):
  min_error_rate/nrow(data)
}


#plot the region
{
  k <- optimal_k
  plot(data.feats, main='Dataset', xlab='X1', ylab='X2', pch=20)
  points(data.feats[i1,], col=2, pch=20)
  points(data.feats[i2,], col=3, pch=20)
  legend("topright", legend=levels(groups.name), fill=c(2,3,4))
  
  x  <- seq(min(data.feats[,1]), max(data.feats[,1]), length=200)
  y  <- seq(min(data.feats[,2]), max(data.feats[,2]), length=200)
  
  # don't use feature1 feature2, but the true names
  xy <- expand.grid(feature1=x, feature2=y)
  
  data.knn <- knn(train = data.feats, test = xy, cl = data$group, k = k)
  
  z  <- as.numeric(data.knn)
  
  contour(x, y, matrix(z, 200), levels=c(1.5, 2.5), drawlabels=F, add=T)
}


# predict with knn 
{
  z0 <- data.frame( feature1=1, feature2=-4 )
  predictions <- knn(train = data[, 1:2], test = z0, cl = data$group, k = optimal_k)
  predictions #High
}

