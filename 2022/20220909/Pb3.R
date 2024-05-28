setwd("C:/Users/Giuseppe/Documents/.magistrale/secondo anno/AppStat/Exams/2022/20220909/20220909")

library(nlmeU) ## --> for the dataset
library(nlme)  ## --> for models implementation

library(corrplot)
library(lattice)
library(insight)
library(plot.matrix)
library(MASS)
library(car)
library(rgl)
library(glmnet)
data <- read.table("cameo.txt",header=T)
head(data)

plot(data)
# some correlation in weight and price
y <- data$price


fm <- lm(y ~ processing + processing*dimension + processing*weight 
         + processing*dimension*weight,data=data)
summary(fm) 


coefficients(fm)

# Residual standard error: (estimated variance of residual )^/1/2
sqrt(sum(residuals(fm)^2)/fm$df)
  

par(mfrow=c(2,2))
plot(fm)
# everything fine

# b) here we must consider processing, but also the interactions with it
linearHypothesis(fm, rbind(c(0,1,0,0,0,0,0,0),
                           c(0,0,0,0,1,0,0,0),
                           c(0,0,0,0,0,1,0,0),
                           c(0,0,0,0,0,0,0,1)), c(0,0,0,0))
# very low pval, we can't reject the H0: processing = 0
# -> is influenting the price significantly, at any reasonable level (pval < e-16)

#c )
summary(fm)
# we can remove with any confidence the crossed term: processing*weights
fm <- lm(y ~ processing + processing*dimension 
         + processing*dimension*weight, data=data)
summary(fm)
# we can remove with any confidence the crossed term: dimension*weights
fm <- lm(y ~ processing + processing*dimension 
         + processing*weight, data=data)
summary(fm)


# we can remove with any confidence the crossed term: processing*dimension
fm <- lm(y ~ processing + dimension 
         + processing*weight, data=data)
summary(fm)



# we can also remove with any confidence the last crossed term: processing*weight
fm <- lm(y ~ processing + dimension + weight, data=data)
summary(fm)

par(mfrow=c(2,2))
plot(fm)

# c)
# obtained an additive model, without loosing the high value of fit 97%, with less parameters

# d)

## Prediction for a new point
{
  # Use same names used in fm
  Z0.new <- data.frame(processing="handmade", dimension=10, weight=80)
  
  # Conf. int. for the mean
  Conf <- predict(fm, Z0.new, interval='confidence', level=1-0.05)  
  Conf
  # 95% of the times contain the mean of the y value predicted with new data
  #     this is for E[y|x]. "Is one at the time"
  
  # Pred. int. for a new obs
  Pred <- predict(fm, Z0.new, interval='prediction', level=1-0.05)  
  Pred
  # 95% of the times contain the y value predicted. Wider than previous
  #     this is for y. That is y = E[y|x] + eps
  
  # Point wise estimate for price: 364.789 euro
  
  # it is hard to say if is reliable since the range of weights in the dataset for 
  # handmade is under 45. So the prices are all under 250.
  # The new datum is very different from the population on which the model was trained
  plot(data[which(data$processing =="handmade" ), 1:3])
}




