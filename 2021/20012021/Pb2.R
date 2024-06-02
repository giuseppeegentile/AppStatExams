setwd("C:/Users/Giuseppe/Documents/.magistrale/secondo anno/AppStat/Exams/2021/20210120/20210120")
library(MASS)
library(class)
library(rgl)
library(mvtnorm)
library(MVN)
library(e1071)

data <- read.table("activity.txt",header=T)
head(data)

# accel <- feat1
# gyro <- feat2
colnames(data) <- c("feature1","feature2","group")


groups.name <- factor(data$group)
g = length(levels(groups.name)) 
g
p <- dim(data)[2] - 1 # no categorical variable


i1 <- which(groups.name == levels(groups.name)[1])
i2 <- which(groups.name == levels(groups.name)[2])
i3 <- which(groups.name == levels(groups.name)[3])


n1 <- length(i1)
n2 <- length(i2)
n3 <- length(i3)
n <- n1 + n2 + n3

data.feats <- data[,1:p] 

# we have priors -> use LDA/QDA to exploit that information 

# Plot the data after jittering (add or remove if you have more/less than 3 groups)
{
  plot(data.feats, main='Data', xlab='X1', ylab='X2', pch=19, col=as.factor(data$group))
}

# Estimates and MANOVA to see if there is some difference in the groups
{
  m <-  colMeans(data[,1:p])
  m1 <- colMeans(data[i1, 1:p])
  m2 <- colMeans(data[i2, 1:p])
  m3 <- colMeans(data[i3, 1:p])
  
  S1 <- cov(data[i1, 1:p])
  S2 <- cov(data[i2, 1:p])
  S3 <- cov(data[i3, 1:p])
  Sp  <- ((n1 - 1) * S1 + (n2 - 1) * S2 + (n3 - 1) * S3) / (n - g)
  
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
  mvn(data[which(data$group == levels(groups.name)[3]), 1:dim(data.feats)[2]])$multivariateNormality
  # yes, normality satisfied
  # Qualitatively, boxplot
  {
    par(mfrow=c(1,length(levels(groups.name))))
    boxplot(scale(data[which(data$group == levels(groups.name)[1]),1:p], center = T, scale = F))
    boxplot(scale(data[which(data$group == levels(groups.name)[2]),1:p], center = T, scale = F))
    boxplot(scale(data[which(data$group == levels(groups.name)[3]),1:p], center = T, scale = F))
    par(mfrow=c(1,1))
  }
  
  
  # Qualitatively,S
  {
    S1 <- cov(data.feats[i1,])
    S2 <- cov(data.feats[i2,])
    S3 <- cov(data.feats[i3,])
    S1
    S2
    S3
    par(mfrow=c(1,3))
    image(S1)
    image(S2)
    image(S3)
    par(mfrow=c(1,1))
  }
}

# we use QDA
prior = c(9/24,0.5,3/24)

qda.data <- qda(data.feats, groups.name
                , prior=prior
)
qda.data

# Estimate of parameters
{
  # Means of parameters
  qda.data$means
  
  # Covariance matrix 
  cov(data[which(data$group == 'laying'), 1:p])
  cov(data[which(data$group == 'sitting'), 1:p])
  cov(data[which(data$group == 'walking'), 1:p])
}



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
  
  # if g = 3
  {
    z  <- predict(qda.data, xy)$post  
    z1 <- z[,1] - pmax(z[,2], z[,3])    
    z2 <- z[,2] - pmax(z[,1], z[,3])    
    z3 <- z[,3] - pmax(z[,1], z[,2])
  }
  
  contour(x, y, matrix(z1, 200), levels=0, drawlabels=F, add=T)
  contour(x, y, matrix(z2, 200), levels=0, drawlabels=F, add=T)
  contour(x, y, matrix(z3, 200), levels=0, drawlabels=F, add=T)
}




# Predict on training data
{
  inference_train <- predict(qda.data, data.feats)
  names(inference_train)
}

# APER
{
  {
    inference_train <- predict(qda.data, data.feats,prior=prior)
    G <- g
    misc <- table(class.true=groups.name, class.assigned=inference_train$class)
    APER <- 0
    for(g in 1:G)
      APER <- APER + sum(misc[g,-g])/sum(misc[g,]) * prior[g]  
    APER
  }
}


# Predict on new entry
{
  Z0 <- data.frame(feature1=.45, feature2=.52)
  predict(qda.data, Z0)
}
# -> sitting, but is not a strong prediction, only with 53%


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
  
  # if g = 3
  {
    z  <- predict(qda.data, xy)$post  
    z1 <- z[,1] - pmax(z[,2], z[,3])    
    z2 <- z[,2] - pmax(z[,1], z[,3])    
    z3 <- z[,3] - pmax(z[,1], z[,2])
  }
  
  contour(x, y, matrix(z1, 200), levels=0, drawlabels=F, add=T)
  contour(x, y, matrix(z2, 200), levels=0, drawlabels=F, add=T)
  contour(x, y, matrix(z3, 200), levels=0, drawlabels=F, add=T)
  
  
  points(0.45,0.51,pch=4, col = "black")
}
# we see indeed that is in the sitting region, but very close to the walking

{
  k <- 5
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

# APER KNN
{
  errorknn <- 0
  for(i in 1:nrow(data)) {                     
    data.knn <- knn(train = data[,1:2], test = data[i,1:2], cl = data[,3], k = 5)
    if(data.knn != data[i,3])
      errorknn <- errorknn + 1
  }
  APERknn <- errorknn/nrow(data)
  APERknn 
}

# we get lower APER with KNN, but APER is not a good estimate of AER
# on training set the KNN perform good, but is prone to overfitting


# We must estimate AER with CV to have a better understanding of the performance:

# AER KNN
{
  errorknn <- 0
  for(i in 1:nrow(data)) {                     
    data.knn <- knn(train = data[-i,1:2], test = data[i,1:2], cl = data[-i,3], k = k)
    if(data.knn != data[i,3])
      errorknn <- errorknn + 1
  }
  AERcv <- errorknn/nrow(data)
  AERcv
}

# AER CV QDA 
# always use the priors without the cost information!!!
{
  qdaCV <- qda(data.feats, data$group, CV=TRUE, prior = prior) 
  misc <- table(class.true=data$group, class.assignedCV=qdaCV$class)
  AERCV  <- misc[1,2]*prior[1]/sum(misc[1,]) + misc[2,1]*prior[2]/sum(misc[2,])
  AERCV
}

# Confirmed what we sayed: lower APER for KNN wrt to QDA -> better fit to training data
# , but on unobserved data QDA has better accuracy


# See what happens on unbserved data now (QDA makes less errors here)
# predict with knn 
{
  z0 <- data.frame( feature1=0.45, feature2=.52 )
  predictions <- knn(train = data[, 1:2], test = z0, cl = data$group, k = k)
  predictions
# -> walking
  
  points(.45,.52,col="black",pch=4)
}




# try by using LDA
# AERCV QDA = 0.045

lda.data <- lda(data.feats, groups.name
                , prior=prior
)
lda.data

# Predict on new entry
{
  Z0 <- data.frame(feature1=.45, feature2=.52)
  predict(lda.data, Z0)
}
# much more sure about its decision: 96% sitting


# AER CV LDA 
# always use the priors without the cost information!!!
{
  ldaCV <- lda(data.feats, data$group, CV=TRUE, prior = prior) 
  misc <- table(class.true=data$group, class.assignedCV=ldaCV$class)
  AERCV  <- misc[1,2]*prior[1]/sum(misc[1,]) + misc[2,1]*prior[2]/sum(misc[2,])
  AERCV
}
# 0.06, higher than QDA


# Plot the 2d partition induced by LDA
{
  x  <- seq(min(data.feats[,1]), max(data.feats[,1]), length=200)
  y  <- seq(min(data.feats[,2]), max(data.feats[,2]), length=200)
  
  # rename this, don't use feature1 and feature2, but the true name
  xy <- expand.grid(feature1=x, feature2=y)
  
  plot(data.feats, main='Dataset', xlab='X1', ylab='X2', pch=20)
  points(data.feats[i1,], col='red', pch=20)
  points(data.feats[i2,], col='green', pch=20)
  points(data.feats[i3,], col='blue', pch=20)
  legend("topright", legend=levels(groups.name), fill=c('red','green','blue'), cex=.7)
  
  points(lda.data$means, pch=4,col=c('red','green','blue') , lwd=2, cex=1.5)
  
  # if g = 3
  {
    z  <- predict(lda.data, xy)$post  # these are P_i*f_i(x,y)  
    z1 <- z[,1] - pmax(z[,2], z[,3])  # P_1*f_1(x,y)-max{P_j*f_j(x,y)}  
    z2 <- z[,2] - pmax(z[,1], z[,3])  # P_2*f_2(x,y)-max{P_j*f_j(x,y)}    
    z3 <- z[,3] - pmax(z[,1], z[,2])  # P_3*f_3(x,y)-max{P_j*f_j(x,y)}
  }

  
  # Plot the contour line of level (levels=0) of z1, z2, z3: 
  # P_i*f_i(x,y)-max{P_j*f_j(x,y)}=0 i.e., boundary between R.i and R.j 
  # where j realizes the max.
  contour(x, y, matrix(z1, 200), levels=0, drawlabels=F, add=T)  
  contour(x, y, matrix(z2, 200), levels=0, drawlabels=F, add=T)
  contour(x, y, matrix(z3, 200), levels=0, drawlabels=F, add=T)
  
}
