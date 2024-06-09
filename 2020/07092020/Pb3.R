setwd("C:/Users/Giuseppe/Documents/.magistrale/secondo anno/AppStat/Exams/2020/20200907/20200907")
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


data <- read.table("leaven.txt",header=T)
head(data)

# See how data are spread: if lot of point on low values and few on high 
# -> log transform both target and features
plot(data, pch=19)
# If variables are correlated 
# ->  you'll have collinearity 


n   <- dim(data)[[1]]

# Target variable
y   <- data$volume 



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

g = ifelse(data$yeast=='sd',1,0)
fm <- lm(y ~ g+ time + I(time^2) + g:time + g:I(time^2),data=data)
summary(fm) 
# equivalent to:
fit1 <-lm(y~yeast + yeast:time + yeast:I(time^2),data=data)
summary(fit1) 


# Residual standard error: (estimated variance of residual )^/1/2
sqrt(sum(residuals(fm)^2)/fm$df)  # s estimate of sigma
coefficients(fm)




par(mfrow=c(2,2))
plot(fm)
# residual seems a bit too on the left, but I wouldn't say is a pattern -> ass satisfied


shapiro.test(residuals(fm))

linearHypothesis(fm,   rbind(c(0,0,0,0,1,0), c(0,0,0,0,0,1)), c(0,0)) # dummy version 
# equivalent to:
linearHypothesis(fit1, rbind(c(0,0,0,0,1,0)), c(0)) # non dummy version

linearHypothesis(fm,   rbind(c(0,0,0,0,0,1)), c(0)) # dummy version
linearHypothesis(fit1, rbind(c(0,0,0,0,1,-1)), c(0)) # non dummy version
# we can reject -> the degree of polynomial of sd is influential 
# also for by (see from summary)
# -> no, there isn't significant difference of the polynomial degree


summary(fm) 
summary(fit1) 


# we can remove time
fm <- lm(y ~ g+  I(time^2) + g:time + g:I(time^2),data=data)
summary(fm) 
fm <- lm(y ~ I(time^2) + g:time + g:I(time^2),data=data)
summary(fm) 

# Non dummy version
fit1 <-lm(y~yeast + yeast:time + yeast:I(time^2),data=data)
summary(fit1) 
# we can remove yeastby interaction with time, considering only the one with sd
sd = ifelse(data$yeast=='sd',1,0)
fit1 <- lm(y ~ yeast + sd:time + yeast:I(time^2),data=data)
summary(fit1) 
# Now we can remove the second order with sd
by = ifelse(data$yeast=='by',1,0)
fit1 <- lm(y ~ yeast + sd:time + by:I(time^2),data=data)
summary(fit1)  
# also the yeast is not influencing
fit1 <- lm(y ~  sd:time + by:I(time^2),data=data)
summary(fit1) 

anova(fit1,fm)

## Prediction for a new point
{
  Z0.new <- data.frame(time=2, g=1)
  
  alpha = 0.01

  # Pred. int. for a new obs
  Pred <- predict(fm, Z0.new, interval='prediction', level=1-alpha)  
  Pred
  # 1.126164 0.9989172 1.25341
  Z0.new <- data.frame(time=2, g=0)
  
  # Pred. int. for a new obs
  Pred <- predict(fm, Z0.new, interval='prediction', level=1-alpha)  
  Pred
  # 1.063421 0.9347615 1.19208
  # with sd it gets bigger in 2 hours
  
  # Conf. int. for the mean
  Conf <- predict(fm, Z0.new, interval='confidence', level=1-alpha)  
  Conf
  # 1.063421 1.034237 1.092604
}

## Prediction for a new point
{
  Z0.new <- data.frame(time=2, sd=1, by = 0)
  
  alpha = 0.01
  
  # Pred. int. for a new obs
  Pred <- predict(fit1, Z0.new, interval='prediction', level=1-alpha)  
  Pred
  # 1.126164 0.9989172 1.25341
  Z0.new <- data.frame(time=2, sd=0, by = 1)
  
  # Pred. int. for a new obs
  Pred <- predict(fm, Z0.new, interval='prediction', level=1-alpha)  
  Pred
  # 1.063421 0.9347615 1.19208
  # with sd it gets bigger in 2 hours
  
  # Conf. int. for the mean
  Conf <- predict(fm, Z0.new, interval='confidence', level=1-alpha)  
  Conf
  # 1.063421 1.034237 1.092604
  
}












