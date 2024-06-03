setwd("C:/Users/Giuseppe/Documents/.magistrale/secondo anno/AppStat/Exams/2023/20230207/20230207")
library(MASS)
library(class)
library(rgl)
library(mvtnorm)
library(MVN)

data <- read.table('pressure.txt', header=T)

head(data)
dim(data)

n <- dim(data)[1]
p <- dim(data)[2]

# we don't have any prior knowledge of the problem, therefore we assume priors as the 
# frequenceis in the dataset (even if is wrong)

colnames(data) <- c("feature1","feature2","group")


groups.name <- factor(data$group)
g = length(levels(groups.name)) 
g

i1 <- which(groups.name == levels(groups.name)[1])
i2 <- which(groups.name == levels(groups.name)[2])

n1 <- length(i1)
n2 <- length(i2)
n <- n1 + n2 
n1

# priors will be 50-50

data.feats <- data[,1:2]
# Assumptions
{
  # multivariate normality in each group
  mvn(data[which(data$group == levels(groups.name)[1]), 1:dim(data.feats)[2]])$multivariateNormality
  mvn(data[which(data$group == levels(groups.name)[2]), 1:dim(data.feats)[2]])$multivariateNormality
  # second group is not following a normal distribution, 
  # however LDA/QDA are robust to normality assumptions
  
  # var.test is used instead of bartlett.test when there are only 2 groups
  # var.test(data.reduced[A,1], data.reduced[B,1])
  bartlett.test(data.feats[i1,], data.feats[i2,])
  # they have same variance structure, we can use LDA
  

  # Quantitatively, images of S
  {
    S1 <- cov(data.feats[i1,])
    S2 <- cov(data.feats[i2,])
    
    par(mfrow=c(1,2))
    image(S1)
    image(S2)
  }
}
par(mfrow=c(1,1))
plot(data[,1:2], col = ifelse(data$group == "H", "blue","red"),pch=19)


lda.data <- lda(data.feats, groups.name) 
lda.data

# Estimate of parameters
{
  # Means of parameters
  lda.data$means
  
  # Extract the covariance matrix (diagonal since is LDA)
  lda.data$scaling
}



# the weakness of this model is that is using the prior estimated as frequencies
# we don't have any knowledge of the problem. 
# Moreover, classes are overlapped, so there is not a clear separation. 

# Predict on training data
{
  inference_train <- predict(lda.data, data.feats)
  names(inference_train)
}


{
  data.reduced <- data.feats
  # with R, but can't specify different priors:
  LdaCV.R <- lda(data.reduced, groups.name, CV=TRUE)
  table(class.true=groups.name, class.assignedCV=LdaCV.R$class)
  errorsCV <- (LdaCV.R$class != groups.name)
  AERCV   <- sum(errorsCV)/length(groups.name)
  AERCV
  
}
# 21%










# kNN-classifier
{
  
  
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
    #error rate:
    min_error_rate/nrow(data)
  }
  optimal_k
  # = 18
  #plot the region
  {
    k <- optimal_k
    plot(data.reduced, main='Dataset', xlab='X1', ylab='X2', pch=20)
    points(data.reduced[i1,], col=2, pch=20)
    points(data.reduced[i2,], col=4, pch=20)
    legend("topright", legend=levels(groups.name), fill=c(2,3,4))
    
    x  <- seq(min(data[,1]), max(data[,1]), length=200)
    y  <- seq(min(data[,2]), max(data[,2]), length=200)
    xy <- expand.grid(feature1=x, feature2=y)
    
    data.knn <- knn(train = data.reduced, test = xy, cl = data$group, k = k)
    
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
  # = 13.5%
  
  
  # predict with knn 
  {
    z0 <- data.frame( feature1=10.8, feature2=39.4 )
    predictions <- knn(train = data[, 1:2], test = z0, cl = data$group, k = optimal_k)
    predictions
  }
  # low pressure
}

