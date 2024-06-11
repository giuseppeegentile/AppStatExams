setwd("C:/Users/Giuseppe/Documents/.magistrale/secondo anno/AppStat/Exams/2017/20170718/20170718")

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
data <- read.table("albatross.txt",header=T)
head(data)




n   <- dim(data)[[1]]

# Target variable
y   <- data$distance


# Model:
# Assumptions:
# 1) For parameter estimation (OLS): Homoschedasticity of residual (no need normality to estimate params!) 
#                           E(Eps) = 0  obvious if there is the intercept
#                           Var(Eps) = sigma^2  Note: also uncorrelated eps (eps_i+1 don't depent on eps_i)
# 2) For inference (so conf intervals):  Eps ~ N(0, sigma^2)


## Parameter estimation -----------------------------------------------------------------------
# Assumptions: E(Eps) = 0  and  Var(Eps) = sigma^2 
fm <- lm(y ~ wind + wind:I(Va^2)+ wind:I(Vi^2),data=data)
summary(fm) 

# alpha0.2 = 3.2880632
# alpha0.1 = 3.2880632 + 1.3632778
# beta2 = 0.0197096
# beta1 = 0.0111417
# gamma2 = -0.0175382
# gamma1 = -0.0097722


# Residual standard error: (estimated variance of residual )^/1/2 estimate of sigma
sqrt(sum(residuals(fm)^2)/fm$df)  
coefficients(fm)



## Model diagnostic, Verify assumptions
{
  par(mfrow=c(2,2))
  plot(fm)
  # 1st: no pattern of fitted values with the residual: we want normality around zero.
  #        we don't want dependence of residual with the fitted value (if there is -> non linearity of data) 
  #        heteroschedasticity if there is "the tunnel" (correlation between eps_i and eps_i+1)
  #         TUNNEL -> log(Y) or sqrt(Y)
  #       ideally: you should do against every variable: if you observe a pattern
  #                 with a variable, ADD that pattern in function of that variable
  # 2nd: QQ plot: must have diagonal fit. Acceptable to have some outliers
  # 4th: Detect influential points: 
  #       if cook's distance is high (point >1) that point is influential
  
  
  shapiro.test(residuals(fm))
  
  par(mfrow=c(1,1))
  # we'd like high pvalue.
  # however, if you have low pvalue and this is due to outliers (in qq plot some points
  # are far from the diagonal), if those points are not influential (from 4-th plot)
  # you can proceed 
  
}


fm <- lm(y ~  wind:I(Va^2)+ wind:I(Vi^2),data=data)
summary(fm) 




# for g=1
linearHypothesis(fm, rbind(c(0,1,0,1,0)), c(0))
# for g=2
linearHypothesis(fm, rbind(c(0,0,1,0,1)), c(0))

# gamma1 = -beta1   and gamma2 = -beta2

# y = beta1*Va^2*windup + beta2*Va^2*winddown + gamma1*Vi^2*windup + gamma2*Vi^2*winddown
# y = beta1*Va^2*windup + beta2*Va^2*winddown - beta1*Vi^2*windup - beta2*Vi^2*winddown
# y = beta1*(Va^2*windup - Vi^2*windup) + beta2*(Va^2*winddown - Vi^2*winddown)
new_regressor <- data$Va^2 - data$Vi^2


fm.red <- lm(y ~  wind:new_regressor,data=data)
summary(fm.red) 



AIC(fm.red,fm)
BIC(fm.red,fm)
anova(fm.red,fm)

Z0.new <- data.frame(Vi=25,Va=35,wind="downwind")

## Prediction for a new point
{
  alpha = 0.01
  # Conf. int. for the mean
  Conf <- predict(fm, Z0.new, interval='confidence', level=1-alpha)  
  Conf
  Z0.new <- data.frame(Vi=25,Va=35,wind="upwind")
  Conf <- predict(fm, Z0.new, interval='confidence', level=1-alpha)  
  Conf
  
  # not safe when upwind, safe when downwin
}






