setwd("C:/Users/Giuseppe/Documents/.magistrale/secondo anno/AppStat/Exams/2022/20220909/20220909")
library(MASS)
library(class)
library(rgl)
library(mvtnorm)
library(MVN)
data <- read.table('fossil.txt',header=T)
head(data)
dim(data)
p <- dim(data)[2]-1
plot(data[,1:2],pch=19,col=as.factor(data[,3]))
data.feats <- data[,1:p]
colnames(data) <- c("feature1","feature2","group")
groups.name <- factor(data$group)
g = length(levels(groups.name)) 
g
p <- dim(data)[2] - 1 #
i1 <- which(groups.name == levels(groups.name)[1])
i2 <- which(groups.name == levels(groups.name)[2])


n1 <- length(i1)
n2 <- length(i2)
n <- n1 + n2


# Estimates and MANOVA to see if there is some difference in the groups
{
  m <-  colMeans(data.feats)
  m1 <- colMeans(data.feats[i1, ])
  m2 <- colMeans(data.feats[i2, ])
  
  S1 <- cov(data.feats[i1, ])
  S2 <- cov(data.feats[i2, ])
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
  
  # Quantitatively, boxplot
  {
    par(mfrow=c(1,length(levels(groups.name))))
    boxplot(scale(data.feats[which(data$group == levels(groups.name)[1]),], center = T, scale = F))
    boxplot(scale(data.feats[which(data$group == levels(groups.name)[2]),], center = T, scale = F))
  }
  
  # Quantitatively, images of S
  {
    S1 <- cov(data.feats[i1,])
    S2 <- cov(data.feats[i2,])
    
    par(mfrow=c(1,2))
    image(S1)
    image(S2)
  }
}
# We can assume homogeneity of variances






lda.data <- lda(data.feats, groups.name,
                # add priors if given
) 
lda.data

# Estimate of parameters
{
  # Means of parameters
  lda.data$means
  
  # Covariance matrix (diagonal since is LDA)
  lda.data$scaling
}
par(mfrow=c(1,1))
# Plot the 2d partition induced by LDA
{

  x  <- seq(min(data.feats[,1]), max(data.feats[,1]), length=200)
  y  <- seq(min(data.feats[,2]), max(data.feats[,2]), length=200)
  xy <- expand.grid(lon=x, lat=y)
  
  plot(data.feats, main='Dataset', xlab='X1', ylab='X2', pch=20)
  points(data.feats[i1,], col='red', pch=20)
  points(data.feats[i2,], col='green', pch=20)
  legend("topright", legend=levels(groups.name), fill=c('red','green','blue'), cex=.7)
  
  points(lda.data$means, pch=4,col=c('red','green','blue') , lwd=2, cex=1.5)
  
  z  <- predict(lda.data, xy)$post  # these are P_i*f_i(x,y)  
  z1 <- z[,1] - pmax(z[,2])  # P_1*f_1(x,y)-max{P_j*f_j(x,y)}  
  z2 <- z[,2] - pmax(z[,1])  # P_2*f_2(x,y)-max{P_j*f_j(x,y)}   
  
  # Plot the contour line of level (levels=0) of z1, z2, z3: 
  # P_i*f_i(x,y)-max{P_j*f_j(x,y)}=0 i.e., boundary between R.i and R.j 
  # where j realizes the max.
  contour(x, y, matrix(z1, 200), levels=0, drawlabels=F, add=T)  
  contour(x, y, matrix(z2, 200), levels=0, drawlabels=F, add=T)


}

# Predict on training data
{
  inference_train <- predict(lda.data, data.reduced)
  names(inference_train)
}

# 1.1) APER with priors default
{
  inference_train <- predict(lda.data, data.feats)
  names(inference_train)
  # Confusion matrix
  table(class.true=groups.name, class.assigned=inference_train$class)
  
  errors <- (inference_train$class != groups.name)
  sum(errors)
  length(groups.name)
  
  APER   <- sum(errors)/length(groups.name)
  APER
}

{
  LdaCV.R <- lda(data.feats, groups.name, CV=TRUE)
  table(class.true=groups.name, class.assignedCV=LdaCV.R$class)
  errorsCV <- (LdaCV.R$class != groups.name)
  AERCV   <- sum(errorsCV)/length(groups.name)
  AERCV
}





# kNN-classifier
{
 
  # choose best k by leave one out cross validation 
  {
    set.seed(9)
    optimal_k <- 0
    min_error_rate <- Inf
    
    for (k in 5:20) {
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
    #error rate:
    min_error_rate/nrow(data)
  }
  
  
  #plot the region
  {
    k <- optimal_k
    plot(data.feats, main='Dataset', xlab='X1', ylab='X2', pch=20)
    points(data.feats[i1,], col=2, pch=20)
    points(data.feats[i3,], col=4, pch=20)
    legend("topright", legend=levels(groups.name), fill=c(2,3,4))
    
    x  <- seq(min(data.feats[,1]), max(data.feats[,1]), length=200)
    y  <- seq(min(data.feats[,2]), max(data.feats[,2]), length=200)

    xy <- expand.grid(lat=x, long=y)
    
    data.knn <- knn(train = data.feats, test = xy, cl = data$group, k = k)
    
    z  <- as.numeric(data.knn)
    
    contour(x, y, matrix(z, 200), levels=c(1.5, 2.5), drawlabels=F, add=T)
  }
}





#knn CVerror : 0.08241758
#LDA CVerror:  0.08791209
#we use KNN

# predict with knn 
{
  z0 <- data.frame( lat=21.9, lon=-159.5 )
  predictions <- knn(train = data[, 1:2], test = z0, cl = data$group, k = optimal_k)
  predictions # ->perisphinctes
}


