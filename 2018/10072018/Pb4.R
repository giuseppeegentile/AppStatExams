setwd("C:/Users/Giuseppe/Documents/.magistrale/secondo anno/AppStat/Exams/2018/20180710/20180710")

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


data <- read.table("Focaccia.txt",header=T)
head(data)



n   <- dim(data)[[1]]

# Target variable
y   <- data$kg

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
fm <- lm(y ~ day + day:t,data=data)
summary(fm) 
# Positive coefficient influence positively the target y (if they have high Pr)
# R^2 coeff of determination: how much of the columns of Z are useful to explain the variability of y.
# R-adjusted^2: how good is the fit. Penalize lots of covariates
# Residual standard error: (estimated variance of residual )^/1/2 estimate of sigma
sqrt(sum(residuals(fm)^2)/fm$df)  
coefficients(fm)

# beta0.0 = 35.2723           # weekday
# beta0.1 = 73.4141+35.2723   # weekend
# beta1.0 = 2.135             # weekday
# beta1.1 = 1.630             # weekend


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
  
  # also look at 
  vif(fm)
  
  par(mfrow=c(1,1))
}


# yes tehre is significance on days: low pval of Fstatistic


linearHypothesis(fm, rbind(c(0,0,1,-1)), c(0))
# not so much significant




# We rewrite the model with dummy variable
g = ifelse(data$day=='weekend',1,0)

fm <- lm(y ~ g + t + g:t,data=data)
summary(fm) 
# we can remove the time with weekends

fm <- lm(y ~ g + t ,data=data)
summary(fm) 


# beta0.0 = 40.1139           # weekday
# beta0.1 = 40.1139+59.1279   # weekend
# beta1   = 1.9925            # time

# "Is that feature influencing positively the y?"
{
  alpha = 0.05
  confint(fm, level= 1-alpha)[2,]# this is beta_3
  # yes, it increases of 60kg
  # [49.502, 68.7538]
}

# Use same names used in fm
Z0.new <- data.frame(t=61, g=0)

alpha = 0.05

# Pred. int. for a new obs
Pred <- predict(fm, Z0.new, interval='prediction', level=1-alpha)  
Pred # 161.655
# 95% of the times contain the y value predicted. Wider than previous
#     this is for y. That is y = E[y|x] + eps

