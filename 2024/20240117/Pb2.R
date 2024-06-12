setwd("C:/Users/Giuseppe/Documents/.magistrale/secondo anno/AppStat/Exams/2024/20240117/20240117")

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


data <- read.table("StorageCentres.txt",header=T)
head(data)

# See how data are spread: if lot of point on low values and few on high 
# -> log transform both target and features
plot(data, pch=19)
# If variables are correlated -> you'll have collinearity 


n   <- dim(data)[[1]]

# Target variable
y   <- data$costs

# Model:
# distance = beta_0 + beta_1 * f1 + beta_2 * f2^2 + Eps
# (linear in the parameters!)

# Assumptions:
# 1) For parameter estimation (OLS): Homoschedasticity of residual (no need normality to estimate params!) 
#                           E(Eps) = 0  obvious if there is the intercept
#                           Var(Eps) = sigma^2  Note: also uncorrelated eps (eps_i+1 don't depent on eps_i)
# 2) For inference (so conf intervals):  Eps ~ N(0, sigma^2)


## Parameter estimation -----------------------------------------------------------------------
# Assumptions: E(Eps) = 0  and  Var(Eps) = sigma^2 
fm <- lm(y ~ time +costs0 + time:growth + rad_less_15_city + size,data=data)
summary(fm) 
# Positive coefficient influence positively the target y (if they have high Pr)
# R^2 coeff of determination: how much of the columns of Z are useful to explain the variability of y.
# R-adjusted^2: how good is the fit. Penalize lots of covariates
# Residual standard error: (estimated variance of residual )^/1/2 estimate of sigma
sum(residuals(fm)^2)/fm$df 
coefficients(fm) # watch down if you used dummy!


AIC(fm)


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

# also look at 
vif(fm)

par(mfrow=c(1,1))
# we'd like high pvalue.
# however, if you have low pvalue and this is due to outliers (in qq plot some points
# are far from the diagonal), if those points are not influential (from 4-th plot)
# you can proceed 

# Residual against a specific feature
{
  plot(resid(fm) ~ time, data = data)
  
  # spot a residual with ^2 dependence
  # -> add I(speed^2) to the linear model (don't remove speed!)
  
  boxplot(fm$residuals ~ data$time, xlab='time', ylab='Residuals')
}
par(mfrow=c(1,1))

fm9.2 <- gls(y ~  time +costs0 + time:growth + rad_less_15_city + size,
             weights = varPower(form = ~time), # Var. function; <delta, v_it>-group
             data = data)
summary(fm9.2)


{
  intervals(fm9.2, which = "var-cov") 
}
# delta:
#         lower     est.    upper
# power 0.7102282 0.88779 1.065352
AIC(fm9.2)
# 1192.182

# 3.2 Correlation 2: AR(1) autoregressive
{
  # storage centre = school: the heteroschedasticity structure
  # time: 
  fm12.2 <- gls(y ~  time +costs0 + time:growth + rad_less_15_city + size, 
                weights = varPower(form = ~time), 
                correlation=corAR1(form= ~time|id_storage_centre),
                data = data)
  
  summary(fm12.2)
  
  {
    intervals(fm12.2, which = "var-cov") 
  }
  #        lower        est.     upper
  # Phi -0.1999194 -0.04387526 0.1143395
  
}

anova(fm12.2,fm9.2)
# no significant difference, therefore we prefer fm9.2 since has less params
