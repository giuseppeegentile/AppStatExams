setwd("C:/Users/Giuseppe/Documents/.magistrale/secondo anno/AppStat/Exams/2019/20190117/20190117")
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

options(rgl.printRglwidget = TRUE)


data <- read.table("areaC.txt",header=T)
head(data)

# See how data are spread: if lot of point on low values and few on high 
# -> log transform both target and features
plot(data, pch=19)
# If variables are correlated 
# ->  you'll have collinearity 


n   <- dim(data)[[1]]

# Target variable
y   <- data$Total


# Model:
# distance = beta_0 + beta_1 * speed + beta_2 * speed^2 + Eps
# (linear in the parameters!)

# Assumptions:
# 1) For parameter estimation (OLS): Homoschedasticity of residual (no need normality to estimate params!) 
#                             E(Eps) = 0  obvious if there is the intercept
#                           and  Var(Eps) = sigma^2 
# 2) For inference (so conf intervals):  Eps ~ N(0, sigma^2)


## Parameter estimation -----------------------------------------------------------------------
# Assumptions: E(Eps) = 0  and  Var(Eps) = sigma^2 

fm <- lm(y ~ Weekend + Petrol +Diesel +Electric+  GPL+ Natural.Gas+ Hybrid.Diesel+ Hybrid.Petrol,data=data)
summary(fm) 
# Look at the coefficient's: if they have high Pr:
#                             * if are positive they increase the target y
# R^2: how much of the columns of Z are useful to explain the variability of y.
# R-adjusted^2: how good is the fit
# Residual standard error: (estimated variance of residual )^/1/2
sqrt(sum(residuals(fm)^2)/fm$df)  # s estimate of sigma
coefficients(fm)

vif(fm)
# there is collinearity


## Model diagnostic, Verify assumptions
{
  par(mfrow=c(2,2))
  plot(fm)
 
  
  
  shapiro.test(residuals(fm))
  
  # also look at 
  vif(fm)
  
  par(mfrow=c(1,1))
  
  # we have 8 features but only 31 observations
  # Residual are heteroschedastic (increasing the fitted value as the fitted value)
  # Non normal at 1%
  
  boxplot(fm$residuals ~ data$Weekend, xlab='weekend', ylab='Residuals')
  
  par(mfrow=c(1,1))
}


# Perform PCA
{
  boxplot(data)
  # We scale since doesn't ask about interpretation
  data <- scale(data)
  data <- as.data.frame(data)
  data.pc <- princomp(data, scores=TRUE)
  summary(data.pc)
  # Now they're uncorrelated
  
  
  # Loadings, first 
  p <- dim(data)[2]
  par(mfrow = c(2,4))
  for(i in 1:4)barplot(data.pc$load[,i], ylim = c(-1, 1))
  for(i in 5:8)barplot(data.pc$load[,i], ylim = c(-1, 1))
  par(mfrow=c(1,1))
  
  # PC1: general high flow of cars (but not much of petrol) in areaC in the weekdays
  # PC2: high flow of petrol cars in areaC during the weekends
  
  sc1.pc <- data.pc$scores[,1]
  sc2.pc <- data.pc$scores[,2]
  
  
  # Estimate the model by using PCs instead of original regressors 
  # Model: y = b0 + b1*PC1+ b2*PC2 + eps, eps~N(0,sigma^2)
  fm.pc <- lm(y ~ data.pc$scores[,1] + data.pc$scores[,2] + data.pc$scores[,3] + data.pc$scores[,4]
                 + data.pc$scores[,5] + data.pc$scores[,6] + data.pc$scores[,7]+ data.pc$scores[,8])
  summary(fm.pc) 
  # Check if the fit is comparable to the starting one
  
  # Be careful in performing dimensionality reduction: the primary goal is to uncorrelate
  #         variables. Removing a component is risky, even if explain low variability of
  #         the dataset, can explain a lot of variability of y!
}


par(mfrow=c(2,2))
plot(fm.pc)
shapiro.test(residuals(fm.pc))
par(mfrow=c(1,1))

# didn't change anything, but we don't have anymore collinearity, we can reduce this model to 
# improve the situation in the residual

fm.pc.red1 <- lm(y ~ data.pc$scores[,1] + data.pc$scores[,2] + data.pc$scores[,3] + data.pc$scores[,4]
            + data.pc$scores[,5] + data.pc$scores[,7]+ data.pc$scores[,8])
summary(fm.pc.red1) # removed 6
fm.pc.red2 <- lm(y ~ data.pc$scores[,1] + data.pc$scores[,2] + data.pc$scores[,3] + data.pc$scores[,4]
                 + data.pc$scores[,5] +  data.pc$scores[,8])
summary(fm.pc.red2) # removed 7

fm.pc.red3 <- lm(y ~ data.pc$scores[,1] + data.pc$scores[,2] + data.pc$scores[,4]
                 + data.pc$scores[,5] +  data.pc$scores[,8])
summary(fm.pc.red3) # removed 3

linearHypothesis(fm.pc, rbind(c(0,0,0,1,0,0,0,0,0),
                           c(0,0,0,0,0,0,1,0,0),
                           c(0,0,0,0,0,0,0,1,0)), c(0,0,0))
# ok, they are ininfluential together

fm.pc.red3 <- lm(y ~ data.pc$scores[,1] + data.pc$scores[,2] + data.pc$scores[,4]
                 + data.pc$scores[,5] +  data.pc$scores[,8])
summary(fm.pc.red3)


AIC(fm.pc.red3)
AIC(fm)
BIC(fm.pc.red3)
BIC(fm)
anova(fm.pc.red3, fm.pc)
anova(fm.pc.red3, fm)
