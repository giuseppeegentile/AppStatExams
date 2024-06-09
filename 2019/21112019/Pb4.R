setwd("C:/Users/Giuseppe/Documents/.magistrale/secondo anno/AppStat/Exams/2019/20191121/20191121")


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


data <- read.table("kimonos.txt",header=T)
head(data)

# See how data are spread: if lot of point on low values and few on high 
# -> log transform both target and features
plot(data, pch=19)
# If variables are correlated 
# ->  you'll have collinearity 


n   <- dim(data)[[1]]

# Target variable
y   <- data$price



# Model Assumptions:
# 1) For parameter estimation (OLS): Homoschedasticity of residual (no need normality to estimate params!) 
#                             E(Eps) = 0  obvious if there is the intercept
#                           and  Var(Eps) = sigma^2 
# 2) For inference (so conf intervals):  Eps ~ N(0, sigma^2)


## Parameter estimation -----------------------------------------------------------------------
# Assumptions: E(Eps) = 0  and  Var(Eps) = sigma^2 

# If he says: impose the same intercept for the two models, don't put the categorical alone, but only in interactions
fm <- lm(y ~ q + q:l + q:ncolors,data=data)
summary(fm) 
# Positive coefficient influence positively the target y (if they have high Pr)
# R^2 coeff of determination: how much of the columns of Z are useful to explain the variability of y.
# R-adjusted^2: how good is the fit. Penalize lots of covariates
# Residual standard error: (estimated variance of residual )^/1/2 estimate of sigma
sum(residuals(fm)^2)/fm$df
coefficients(fm) 
# alpha1 = 127.776
# alpha2 = 127.776 +64.161
# beta1 = 612.042
# beta2 = 230.7254
# gamma1 = 10.456
# gamma2 = 33.563


## Model diagnostic, Verify assumptions
{
  par(mfrow=c(2,2))
  plot(fm)
  # 1st: no pattern of fitted values with the residual: we want normality around zero.
  #       we don't want dependence of residual with the fitted value
  #        heteroschedasticity if there is "the tunnel"
  #       ideally: you should do against every variable: if you observe a pattern
  #                 with a variable, ADD that pattern in function of that variable
  # 2nd: QQ plot: must have diagonal fit. Acceptable to have some outliers
  # 4th: Detect influential points: 
  #       if cook's distance is high (point >1) that point is influential
  
  
  shapiro.test(residuals(fm))
  
  par(mfrow=c(1,1))
} # everything ok


# quality is significantly influencing price, pval of f test is low
linearHypothesis(fm, rbind(c(0,0,0,0,1,0), c(0,0,0,0,0,1)), c(0,0))
# not significant, we can reject at 5%

fm <- lm(y ~ q + q:l,data=data)
summary(fm) 

# we can remove also the qualoty in the intercept
fm <- lm(y ~ q:l,data=data)
summary(fm) 
fm <- lm(y ~ q + q:l + q:ncolors,data=data)
linearHypothesis(fm, rbind(c(0,0,0,0,1,0), c(0,0,0,0,0,1),c(0,1,0,0,0,0)), c(0,0,0))
# ok
fm <- lm(y ~ q:l,data=data)
summary(fm) 
coefficients(fm)
sum(residuals(fm)^2)/fm$df


{
  # mean fixed price of high/medium quality kimono: 
  alpha = 0.1
  confint(fm, level= 1-alpha)[1,]
  # [151.512, 334.001]
  
  confint(fm, level= 1-alpha)[3,]# this is beta_2
  # mean price of medium quality kimono: [224.870, 250.493]
}

## Prediction for a new point
{
  # Use same names used in fm
  Z0.new <- data.frame(q="high", l=6.5)
  
  alpha = 0.1
  # Conf. int. for the mean total cost
  Conf <- predict(fm, Z0.new, interval='confidence', level=1-alpha)  
  Conf
  # 95% of the times contain the mean of the y value predicted with new data
  #     this is for E[y|x]. "Is one at the time"
  
}










