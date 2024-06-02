setwd("C:/Users/Giuseppe/Documents/.magistrale/secondo anno/AppStat/Exams/2021/20210706/20210706")
data <- read.table("pc.txt",header=T)
plot(data,pch=19)

library(nlmeU) 
library(nlme) 
library(lme4)
library(corrplot)
library(lattice)
library(insight)
library(plot.matrix)
library(MASS)
library(car)
library(rgl)
library(glmnet)
head(data)


n   <- dim(data)[[1]]


# Target variable
y   <- data$price



fm <- lm(y ~ OS:freq + OS:cache_acc,data=data)
summary(fm) 
# Look at the coefficient's: if they have high Pr:
#                             * if are positive they increase the target y

# Residual standard error: estimated variance of residual 
sum(residuals(fm)^2)/fm$df 
# High variability, we have only 60 observations
coefficients(fm)
par(mfrow=c(2,2))
plot(fm)

shapiro.test(residuals(fm))

vif(fm)
# there is correlation between variables, but assumptions are satisfied
par(mfrow=c(1,1))



linearHypothesis(fm, rbind(c(0,1,0,0,0,0,0),
                           c(0,0,1,0,0,0,0),
                           c(0,0,0,1,0,0,0),
                           c(0,0,0,0,1,0,0),
                           c(0,0,0,0,0,1,0),
                           c(0,0,0,0,0,0,1)), c(0,0,0,0,0,0))
# yes, is significant


# c)
linearHypothesis(fm, rbind(c(0,0,0,0,1,0,0),
                           c(0,0,0,0,0,1,0),
                           c(0,0,0,0,0,0,1)), c(0,0,0))
# no, is not significant 

# d)
fm <- lm(y ~ OS:freq,data=data)
summary(fm) 
coefficients(fm)
# higher clock frequency associated to higher prices 

# e)
## Prediction for a new point
{
  # Use same names used in fm
  Z0.new <- data.frame(freq=3.2, cache_acc =10,OS="Windows")
  
  alpha = 0.1
  # Conf. int. for the mean
  Conf <- predict(fm, Z0.new, interval='confidence', level=1-alpha)  
  Conf
  # 95% of the times contain the mean of the y value predicted with new data
  #     this is for E[y|x]. "Is one at the time"
}



