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

true <- "high"
false <- "low"
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
  
  
  S1 <- cov(data.feats[i1,])
  S2 <- cov(data.feats[i2,])
  
  par(mfrow=c(1,2))
  image(S1)
  image(S2)
}

# Covar matrix are very similar, we can use lda


lda.data <- lda(data.feats, groups.name, prior=prior.c)
lda.data
{
  inference_train <- predict(lda.data, data.feats)
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
  LdaCV <- lda(data.feats, groups.name, CV=TRUE, prior = prior.c)  # specify the argument CV
  misc <- table(class.true=groups.name, class.assignedCV=LdaCV$class)
  AERCV  <- misc[1,2]*prior.c[1]/sum(misc[1,]) + misc[2,1]*prior.c[2]/sum(misc[2,])
  AERCV
}
APER
AERCV




# we get very low APER: 0.067 but is not a good estimator of AER
# Indeed with CV we get 0.1445093.
# -> some overfitting due to the fact that we have only 30 observations per group. (60 to estimate covar mat)
# Our model doesn't have enoguh data, so is performing good only on training data.



# Upcoming event will use 1000 observations, calculate the budget
{
  total_products <- 1000
  TP <- 0  # True Positives
  TN <- 0  # True Negatives
  FP <- 0  # False Positives
  FN <- 0  # False Negatives
  
  # Perform LOOCV
  for(i in 1:nrow(data.feats)) {
    LdaCV.i <- lda(data.feats[-i,], groups.name[-i], prior=prior.c)
    prediction <- predict(LdaCV.i, data.feats[i,])$class
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
# "how much money do we need to have in order to perform the lab tests?"
# Instead, if the meaning was: "what will be the average economic loss?", I would have removed the TP part
prev_strategy_cost <- total_products * cost_per_test
cur_strategy_cost <- (FP / (TN + FP)) * total_products * pt * c.ft + (FN / (TP + FN)) * total_products * pf * c.tf

rbind("Savings", prev_strategy_cost - cur_strategy_cost)


# Economic loss of the classifier
(c.ft*pt*FP/(FP+TP) + c.tf*FN/(FN+TP))













