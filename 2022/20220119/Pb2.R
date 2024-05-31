library(MASS)
library(class)
library(rgl)
library(mvtnorm)
library(MVN)

setwd("C:/Users/Giuseppe/Documents/.magistrale/secondo anno/AppStat/Exams/2022/20220119/20220119")
data <- read.table("fish.txt",header=T)
head(data)
colnames(data) <- c("feature1","feature2","group")


groups.name <- factor(data$group)
g = length(levels(groups.name)) 
g
p <- dim(data)[2] - 1 # no categorical variable

i1 <- which(groups.name == levels(groups.name)[1])
i2 <- which(groups.name == levels(groups.name)[2])


n1 <- length(i1)
n2 <- length(i2)

n <- n1 + n2

data.feats <- data[,1:p] 


# Estimates and MANOVA to see if there is some difference in the groups
{
  m <-  colMeans(data[,1:p])
  m1 <- colMeans(data[i1, 1:p])
  m2 <- colMeans(data[i2, 1:p])
  
  S1 <- cov(data[i1, 1:p])
  S2 <- cov(data[i2, 1:p])
  Sp  <- ((n1 - 1) * S1 + (n2 - 1) * S2) / (n - g)
  
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
  # no normality for second group, however, LDA is robust to normality assumption
  # (fisher idea)
  
  # var.test is used instead of bartlett.test when there are only 2 groups
  # var.test(data.reduced[A,1], data.reduced[B,1])
  bartlett.test(data.feats[i1,], data.feats[i2,])
  
  # Quantitatively, boxplot
  {
    par(mfrow=c(1,length(levels(groups.name))))
    boxplot(scale(data[which(data$group == levels(groups.name)[1]),1:p], center = T, scale = F))
    boxplot(scale(data[which(data$group == levels(groups.name)[2]),1:p], center = T, scale = F))
  }
  
  # Quantitatively, images of S
  {
    S1 <- cov(data.feats[i1,])
    S2 <- cov(data.feats[i2,])
    
    par(mfrow=c(1,2))
    image(S1)
    image(S2)
    par(mfrow=c(1,1))
  }
}
plot(data[,1:p], col = ifelse(data$group == "H", "blue","red"),pch=19)

# weakness: we're using priors from dataset
#           we don't have normality for group2 (even if is not a big problem)
#           QDA is more prone to overfit than LDA, which can't be used since we don't 
#           have same covariances


qda.data <- qda(data.feats, groups.name
                #, prior=prior.c
)
qda.data

# priors used 
# p_H = 0.48, p_L = 0.52

# Estimate of parameters
{
  # Means of parameters
  qda.data$means
  
  # Covariance matrix 
  cov(data[which(data$group == 'H'), 1:2])
  cov(data[which(data$group == 'L'), 1:2])
}


# Predict on training data
{
  inference_train <- predict(qda.data, data.feats)
  names(inference_train)
}

# With default priors 
{
  QdaCV.data <- qda(data.feats, groups.name, CV=T)
  table(class.true=groups.name, class.assignedCV=inference_train$class)
  errorsqCV <- (inference_train$class != groups.name)
  AERqCV   <- sum(errorsqCV)/length(groups.name)
  AERqCV # 0.096
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





# choose best k by leave one out cross validation 
{
  set.seed(19)
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
  #error rate (AER):
  min_error_rate/nrow(data)
}


# QDA has lower AER, we use it


# Predict on new entry
{
  Z0 <- data.frame(feature1=10.8, feature2=39.4)
  predict(qda.data, Z0) #-> L
  names(inference_train)
}
