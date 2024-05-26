setwd("C:/Users/Giuseppe/Documents/.magistrale/secondo anno/AppStat/Exams/2024/20240206/20240206")
data <- read.table("food.txt",header=T)
head(data)
library(MASS)
library(class)
library(rgl)
library(mvtnorm)
library(MVN)
# We should use LDA/QDA since we have information of priors and cost of misclassification

# high = true
# low = false (have quality issue)

{
  # misclassification costs
  c.tf <- 100000   # predict as high when they are low: cost 100000$
  c.ft <- 500      # since will need to do lab analysis when not needed
  
  #prior probabilities
  pf <- 0.001    # 0.1% have quality issue (false=low)    
  pt <- 1-pf
  prior = c(pt, pf)
  prior
  
  # Prior modified to account for the misclassification costs
  prior.c <- c(pt*c.ft/(c.tf*pf+c.ft*pt), pf*c.tf/(c.tf*pf+c.ft*pt))
  prior.c
}

colnames(data) <- c("feature1","feature2","feature3","feature4","group")


groups.name <- factor(data$group)
g = length(levels(groups.name)) 
g

i1 <- which(groups.name == levels(groups.name)[1])
i2 <- which(groups.name == levels(groups.name)[2])

# Do accordingly to how many g
n1 <- length(i1)
n2 <- length(i2)
n <- n1 + n2

# we take only the first two features here
data.feats <- data[,1:4]






# Assumptions
{
  # multivariate normality in each group
  mvn(data[which(data$group == levels(groups.name)[1]), 1:dim(data.feats)[2]])$multivariateNormality
  mvn(data[which(data$group == levels(groups.name)[2]), 1:dim(data.feats)[2]])$multivariateNormality
  # we can't reject normality at 1%
  
  # var.test is used instead of bartlett.test when there are only 2 groups
  # var.test(data.reduced[A,1], data.reduced[B,1])
  bartlett.test(data.feats[i1,], data.feats[i2,])
  
  S1 <- cov(data.feats[i1,])
  S2 <- cov(data.feats[i2,])
  
  par(mfrow=c(1,2))
  image(S1)
  image(S2)
  
  pairs(data[,1:4], col = ifelse(data$group == "high", "blue","red"),pch=19)
  dim(data[which(data$group == levels(groups.name)[1]),1:4])
  dim(data[which(data$group == levels(groups.name)[2]),1:4])
  par(mfrow=c(1,2))
  boxplot(scale(data[which(data$group == levels(groups.name)[1]),1:4], center = T, scale = F))
  boxplot(scale(data[which(data$group == levels(groups.name)[2]),1:4], center = T, scale = F))
}

# We end up rejecting the bartlett test for homogeneity of covariance
# Even if we can see from boxplot that they are comparable, also from the image of 
# covariance matrix..pval is too low therefore we proceed with QDA


qda.data <- qda(data.reduced, groups.name, prior=prior.c)
qda.data
{
  inference_train <- predict(qda.data, data.reduced)
  prior
  G <- g
  misc <- table(class.true=groups.name, class.assigned=inference_train$class)
  APER <- 0
  for(g in 1:G)
    APER <- APER + sum(misc[g,-g])/sum(misc[g,]) * prior[g]  
  APER
}
{
  # Leave One Out CV: specify priors accordingly in the for loop!
  errors_CV <- 0
  for(i in 1:dim(data)[1]){
    QdaCV.i <- qda(data.reduced[-i,], groups.name[-i], prior=prior)
    errors_CV <- errors_CV + as.numeric(predict(QdaCV.i,data.reduced[i,])$class != groups.name[i])
  }
  AERCV   <- sum(errors_CV)/length(groups.name)
  AERCV # typically higher than APER, more accurate
}
APER
AERCV

# we get very low APER: 0.067 but is not a good estimator of AER
# Indeed with CV we get 0.5, that is very high, meaning we classify wrong one
# out of 2.
# -> overfitting. QDA is overfitting prone since uses more parameters.
# Also, we have only 30 observatinos per group, that may be too low to estimate
# covariance matrix.
# Our model doesn't have enoguh data, so is performing good only on training data.


# The same problem would occours also in LDA, because 60 observations aren't enough
# to estimate Spooled. 

# Proof also with LDA (exercise purpose, wouldn't have done in exam)
lda.data <- lda(data.reduced, groups.name,prior=prior.c) # add priors
lda.data


# APER
{
  inference_train <- predict(lda.data, data.reduced)
  names(inference_train)
  G <- g
  misc <- table(class.true=groups.name, class.assigned=inference_train$class)
  APER <- 0
  for(g in 1:G)
    APER <- APER + sum(misc[g,-g])/sum(misc[g,]) * prior[g]  
  APER
}


LdaCV.s <- lda(data.reduced, groups.name, prior=prior.c, CV=T)
table(class.true=groups.name, class.assignedCV=LdaCV.s$class)

# Upcoming event will use 1000 observations, calculate the budget
{
  total_products <- 1000
  TP <- 0  # True Positives
  TN <- 0  # True Negatives
  FP <- 0  # False Positives
  FN <- 0  # False Negatives
  
  # Perform LOOCV
  for(i in 1:nrow(data.reduced)) {
    LdaCV.i <- lda(data.reduced[-i,], groups.name[-i], prior=prior.c)
    prediction <- predict(LdaCV.i, data.reduced[i,])$class
    true_label <- groups.name[i]
    
    if(prediction == true_label && true_label == "low") {
      TP <- TP + 1
    } else if(prediction == true_label && true_label == "high") {
      TN <- TN + 1
    } else if(prediction != true_label && true_label == "low") {
      FN <- FN + 1
    } else if(prediction != true_label && true_label == "high") {
      FP <- FP + 1
    }
  }
  
  # Sensitivity and specificity
  sensitivity <- TP / (TP + FN)  # True Positive Rate
  specificity <- TN / (TN + FP)  # True Negative Rate
  
  # Estimate number of low quality in 1000 observations
  expected_false <- total_products * pf
  expected_true  <- total_products * pt
  
  
  # Calculate true positives and false positives
  true_positives <- sensitivity * expected_false
  false_positives <- (1 - specificity) * expected_true
  
  # Number of flagged products: those that will be analysed in lab
  total_flagged <- true_positives + false_positives
  
  cost_per_test <- 500 # cost of the lab test
  budget <- total_flagged * cost_per_test
  budget # = 33533.33
  # If we would test all the observations, we'd spend
  500*1000
  # That is greater than 33533.33
  budget < 500*1000
}

# economic saving
500*1000 - budget










