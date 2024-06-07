setwd("C:/Users/Giuseppe/Documents/.magistrale/secondo anno/AppStat/Exams/2019/20190117/20190117")

library(MASS)
library(class)
library(rgl)
library(mvtnorm)
library(MVN)
library(e1071)
library(heplots)


data <- read.table('trading.txt', header = T)

head(data)
dim(data)
p <- dim(data)[2] - 1 # no categorical variable
par(mfrow=c(1,1))

colnames(data) <- c("feature1","feature2","group")
plot(data[,1:p],pch=19,col=factor(data$group))



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
data.feats <- data[,1:(dim(data)[2]-1)]

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

# with default priors 
{
  inference_train <- predict(qda.data)
  misc <- table(class.true=groups.name, class.assigned=inference_train$class)
  
  errorsq <- (inference_train$class != groups.name)
  APERq   <- sum(errorsq)/length(groups.name)
  APERq # 0.310757
}

# With default priors 
{
  QdaCV.data <- qda(data.feats, groups.name, CV=T)
  misc <- table(class.true=groups.name, class.assignedCV=QdaCV.data$class)
  errorsqCV <- (QdaCV.data$class != groups.name)
  AERqCV   <- sum(errorsqCV)/length(groups.name)
  AERqCV # 0.3227092
}

# better than random (trivial): 0.5

# Predict on new entry
{
  Z0 <- data.frame(feature1=-3, feature2=-1.2)
  predict(qda.data, Z0)
}
# no i wouldn't







