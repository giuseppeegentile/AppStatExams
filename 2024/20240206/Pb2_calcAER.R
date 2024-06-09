setwd("C:/Users/Giuseppe/Documents/.magistrale/secondo anno/AppStat/Exams/2024/20240206/20240206")
library(MASS)
library(class)
library(rgl)
library(mvtnorm)
library(MVN)
library(e1071)
library(heplots)

options(rgl.printRglwidget = TRUE)
data <- read.table("food.txt",header=T)
head(data)

# Cost of misclassification -> use this priors
{
  
  true <- "high"
  false <- "low"

  {
    # misclassification costs
    c.tf <- 10000       # predict as true when they are false cost 10$
    c.ft <- 500        # predict as false when they are true cost 5cents
    
    #prior probabilities
    pf <- 0.0001   # text was saying 0.1% but for explanation i tryed 0.01%
    pt <- 1-pf
    prior = c(pt, pf)
    prior
    
    # Prior modified to account for the misclassification costs
    prior.c <- c(pt*c.ft/(c.tf*pf+c.ft*pt), pf*c.tf/(c.tf*pf+c.ft*pt))
    prior.c
  }
}

p <- dim(data)[2] - 1 # no categorical variable
par(mfrow=c(1,1))
plot(data[,1:p],pch=19)
head(data)
colnames(data) <- c("feature1","feature2","feature3","feature4","group")


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
  
  
  # if there is normality
  boxM(data.feats, groups.name)
  # quite pval, even though no normality for second group
  
  
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
# we can assume somehow homogeneity of covariances, also, we have few data per group
n1
n2
# we won't be able to estimate well the sigmas, so we use Sp -> LDA


lda.data <- lda(data.feats, groups.name, prior=prior.c
) 
lda.data



# APER
# with prios without cost information
{
  inference_train <- predict(lda.data)
  G <- g
  misc <- table(class.true=groups.name, class.assigned=inference_train$class)
  APER <- 0
  for(g in 1:G)
    APER <- APER + sum(misc[g,-g])/sum(misc[g,]) * prior[g]  
  APER
}
# very low: 1e-4 (too low?): Qui le classificazioni sono tutte ad high

# with prios with cost information
{
  inference_train <- predict(lda.data)
  G <- g
  misc <- table(class.true=groups.name, class.assigned=inference_train$class)
  APER <- 0
  for(g in 1:G)
    APER <- APER + sum(misc[g,-g])/sum(misc[g,]) * prior.c[g]  
  APER
} #more reasonable 0.001996207



# AER CV
# with prios without cost information
{
  # Leave One Out CV: specify priors accordingly in the for loop!
  LdaCV <- lda(data.feats, groups.name, CV=TRUE, prior = prior.c)  # specify the argument CV
  misc <- table(class.true=groups.name, class.assignedCV=LdaCV$class)
  AERCV  <- misc[1,2]*prior[1]/sum(misc[1,]) + misc[2,1]*prior[2]/sum(misc[2,])
  AERCV
} # 1e-4: come l'APER! Qui le classificazioni sono tutte ad high

# with prios with cost information
{
  # Leave One Out CV: specify priors accordingly in the for loop!
  LdaCV <- lda(data.feats, groups.name, CV=TRUE, prior = prior.c)  # specify the argument CV
  misc <- table(class.true=groups.name, class.assignedCV=LdaCV$class)
  AERCV  <- misc[1,2]*prior.c[1]/sum(misc[1,]) + misc[2,1]*prior.c[2]/sum(misc[2,])
  AERCV
} # More reasonable 0.001996207





# overfitting due to the fact that we have only 60 observations








# Upcoming event will use 1000 observations, calculate the budget
# Adjust for QDA
{
  new_total <- 1000
  cost_per_test <- 500 # cost of the lab test
  TP <- 0  # True Positives
  TN <- 0  # True Negatives
  FP <- 0  # False Positives
  FN <- 0  # False Negatives
  
  # Perform LOOCV
  for(i in 1:nrow(data.feats)) {
    LdaCV.i <- lda(data.feats[-i,], groups.name[-i], prior=prior.c)
    prediction <- predict(LdaCV.i, data.feats[i,])$class
    true_label <- groups.name[i]
    
    if(prediction == true_label && true_label == false) {
      TP <- TP + 1                                      
    } else if(prediction == true_label && true_label == true) {
      TN <- TN + 1
    } else if(prediction != true_label && true_label == false) {
      FN <- FN + 1
    } else if(prediction != true_label && true_label == true) {
      FP <- FP + 1
    }
  }
  # Sensitivity and specificity
  # both the true positive and the true negative will be analysed in lab
  sensitivity <- TP / (TP + FN)  # True Positive Rate
  specificity <- TN / (TN + FP)  # True Negative Rate
  
  # Estimate number of "false" in "new_total" observations
  expected_false <- new_total * pf
  expected_true  <- new_total * pt
  
  
  # Calculate true positives and false positives
  true_positives <- sensitivity * expected_false
  false_positives <- (1 - specificity) * expected_true
  
  # Number of flagged products: those that will be analysed in lab
  total_flagged <- true_positives + false_positives
  
  
  budget <- total_flagged * cost_per_test
  budget 
  # If we would test all the observations, we'd spend
  cost_per_test*new_total
  # We're saving
  prev_strategy_cost <- new_total * cost_per_test
  cur_strategy_cost <- (FP / (TN + FP)) * new_total * pt * c.ft + (FN / (TP + FN)) * new_total * pf * c.tf
  rbind("Savings", prev_strategy_cost - cur_strategy_cost)
}

# Economic loss of the classifier
(c.ft*pt*misc[2,1]/(misc[2,1]+ misc[2,2]) + c.tf*pf*misc[1,2]/(misc[1,1]+ misc[1,2]))













