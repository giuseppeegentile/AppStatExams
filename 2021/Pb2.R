setwd("C:/Users/Giuseppe/Documents/.magistrale/secondo anno/AppStat/Exams/2021/20210706/20210706")
data <- read.table("orthopaedics.txt")
head(data)
library(MASS)
library(class)
library(rgl)
library(mvtnorm)
library(MVN)
options(rgl.printRglwidget = TRUE)

factor(data$norm_abnorm)
prior=c(0.35,0.65)


colnames(data) <- c("feature1","feature2","group")


groups.name <- factor(data$group)
g = length(levels(groups.name)) 
g
p <- dim(data)[2] - 1 # no categorical variable
# Do accordingly to how many g
i1 <- which(groups.name == levels(groups.name)[1])
i2 <- which(groups.name == levels(groups.name)[2])


# Do accordingly to how many g
n1 <- length(i1)
n2 <- length(i2)

n <- n1 + n2 

data.feats <- data[,1:p] #


plot(data.feats, col=factor(data$group),pch=19)
# covariance seems already very different


# Estimates and MANOVA to see if there is some difference in the groups
{
  m <-  colMeans(data[,1:p])
  m1 <- colMeans(data[i1, 1:p])
  m2 <- colMeans(data[i2, 1:p])
  
  S1 <- cov(data[i1, 1:p])
  S2 <- cov(data[i2, 1:p])
  Sp  <- ((n1 - 1) * S1 + (n2 - 1) * S2 ) / (n - g)
  
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

  
  # var.test is used instead of bartlett.test when there are only 2 groups
  # var.test(data.reduced[A,1], data.reduced[B,1])
  bartlett.test(data.feats[i1,], data.feats[i2,])
  
  # Qualitatively, boxplot
  {
    par(mfrow=c(1,length(levels(groups.name))))
    boxplot(scale(data[which(data$group == levels(groups.name)[1]),1:p], center = T, scale = F))
    boxplot(scale(data[which(data$group == levels(groups.name)[2]),1:p], center = T, scale = F))
  }
}


qda.data <- qda(data.feats, groups.name
                , prior=prior
)
qda.data



# Estimate of parameters
{
  # Means of parameters
  qda.data$means
  
  # Covariance matrix, estimated for each group
  cov(data[which(data$group == 'NO'), 1:2])
  cov(data[which(data$group == 'AB'), 1:2])
}
par(mfrow=c(1,1))
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



# AER CV with priors
# always use the priors without the cost information!!!
{
  qdaCV <- qda(data.feats, data$group, CV=TRUE, prior = prior) 
  misc <- table(class.true=data$group, class.assignedCV=qdaCV$class)
  AERCV  <- misc[1,2]*prior[1]/sum(misc[1,]) + misc[2,1]*prior[2]/sum(misc[2,])
  AERCV
}


# Predict on new entry
{
  Z0 <- data.frame(feature1=60, feature2=0)
  predict(qda.data, Z0)
  # -> abnormal with prob 76%
}
library(e1071)


# model
{
  svmfit <- svm(as.factor(data$group)~., data, kernel ='linear', cost=0.1, scale=FALSE)
  summary(svmfit)
}
# plot model
{
  par(mfrow=c(1,2))
  plot(svmfit, data, col =c('salmon', 'light blue'), pch=19, asp=1)

  par(mfrow=c(1,1))
}
# too much support vectors and too much errors...classes are not separable



# Prediction for a new observation 
{
  Z0 <- data.frame(feature1=60, feature2=0)
  
  predict(svmfit, Z0)
}




# A better model would have been using radial kernel
# model
{
  svmfit <- svm(as.factor(data$group)~., data, kernel ='radial', cost=1, scale=FALSE)
  summary(svmfit)
}
# plot model
{
  par(mfrow=c(1,2))
  plot(svmfit, data, col =c('salmon', 'light blue'), pch=19, asp=1)
  
  par(mfrow=c(1,1))
}
# too much support vectors and too much errors...classes are not separable



# Prediction for a new observation 
{
  Z0 <- data.frame(feature1=60, feature2=0)
  
  predict(svmfit, Z0)
}



