setwd("C:/Users/Giuseppe/Documents/.magistrale/secondo anno/AppStat/Exams/2023/20230904/20230904")
library(MASS)
library(class)
library(rgl)
library(mvtnorm)
library(MVN)
library(e1071)
library(heplots)

data <- read.table("doping.txt",header=T)
head(data)

colnames(data) <- c("feature1","feature2","feature3","feature4","group")


groups.name <- factor(data$group)
g = length(levels(groups.name)) 
g
p <- dim(data)[2] - 1 # no categorical variable
# Do accordingly to how many g
i1 <- which(groups.name == levels(groups.name)[1])
i2 <- which(groups.name == levels(groups.name)[2])


n1 <- length(i1)
n2 <- length(i2)
n <- n1 + n2

data.feats <- data[,1:p] 
head(data.feats)


# Assumptions
{
  # multivariate normality in each group
  mvn(data[which(data$group == levels(groups.name)[1]), 1:dim(data.feats)[2]])$multivariateNormality
  mvn(data[which(data$group == levels(groups.name)[2]), 1:dim(data.feats)[2]])$multivariateNormality
  
  # if there is normality
  boxM(data.feats, groups.name)
  
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

plot(data.feats,pch=19, col=as.factor(groups.name))

# Variances are quite different, we rejected with boxM (we had enough normality)



{
  true <- "clean"
  false <- "doped"
    
  {
    # misclassification costs
    c.tf <- 50000       # predict as clean when they are doped 
    c.ft <- 1000        # predict as doped when they are clean 
    
    #prior probabilities
    pf <- 0.01       # doped:
    pt <- 1-pf
    prior = c(pt, pf)
    prior
    
    # Prior modified to account for the misclassification costs
    prior.c <- c(pt*c.ft/(c.tf*pf+c.ft*pt), pf*c.tf/(c.tf*pf+c.ft*pt))
    prior.c
  }
}
qda.data <- qda(data.feats, groups.name, prior=prior.c
                # add priors if given
) 
qda.data

# With custom priors
{
  errors_CV <- 0
  miscCV <- cbind(0, 0, 0, 0)
  colnames(miscCV) <- c("TN", "FN", "FP", "TP")
  for(i in 1:dim(data)[1]){
    QdaCV.i <- qda(data.feats[-i,], groups.name[-i], prior=prior.c)
    errors_CV <- errors_CV + as.numeric(predict(QdaCV.i, data.feats[i,])$class != groups.name[i])
    for(j in 1:2)
      for(k in 1:2)
        if (predict(QdaCV.i, data.feats[i, ])$class == levels(groups.name)[j] && groups.name[i] == levels(groups.name)[k]) 
          miscCV[j*k+as.numeric(j == 2 && k == 1)] <- miscCV[j*k+as.numeric(j == 2 && k == 1)] + 1
  }
  AERCV <- sum(errors_CV) / length(groups.name)
  AERCV
  
}



# Upcoming event will use 1000 observations, calculate the budget
# Adjust for QDA
{
  new_total <- 200
  cost_per_test <- 1000 # cost of the lab test
  TP <- 0  # True Positives
  TN <- 0  # True Negatives
  FP <- 0  # False Positives
  FN <- 0  # False Negatives
  
  # Perform LOOCV
  for(i in 1:nrow(data.feats)) {
    QdaCV.i <- qda(data.feats[-i,], groups.name[-i], prior=prior.c)
    prediction <- predict(QdaCV.i, data.feats[i,])$class
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

# Economic loss of the classifier: 
(c.ft*pt*miscCV[3]/(miscCV[3]+ miscCV[4]) + c.tf*pf*miscCV[2]/(miscCV[1]+ miscCV[2]))



