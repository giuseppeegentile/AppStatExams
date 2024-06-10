setwd("C:/Users/Giuseppe/Documents/.magistrale/secondo anno/AppStat/Exams/2018/20180913/20180913")

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


data <- read.table("Lumieres.txt",header=T)
head(data)

# See how data are spread: if lot of point on low values and few on high 
# -> log transform both target and features
plot(data[,1:3], pch=19)
# If variables are correlated 
# ->  you'll have collinearity 


n   <- dim(data)[[1]]

  # Target variable
y   <- data$N.people


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
fm <- lm(y ~ rain + day + I(day^2) + temperature,data=data)
summary(fm) 


# beta0.0 =  174.10941
# beta0.1 = 174.10941-5.25620
# beta1   = 5.00795
# beta2   =  -0.04295
# beta3   =  -0.31875

sqrt(sum(residuals(fm)^2)/fm$df)  




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

} #ass. ok

linearHypothesis(fm, rbind(c(0,0,1,0,0), c(0,0,0,1,0)), c(0,0))
# yes,depends on the day


linearHypothesis(fm, rbind(c(0,1,0,0,0), c(0,0,0,0,1)), c(0,0))
# we can remove both temperature and rain factor

fit <- lm(y ~ day + I(day^2),data=data)
summary(fit) 
AIC(fit,fm)
BIC(fit,fm)
anova(fm,fit)

fm <- lm(y ~ day + I(day^2),data=data)
summary(fm) 



# y = beta_0 + beta_1*day + beta_2*day^2
# max y = y'(day) = 0
# y = beta_1 + 2*beta_2*day = 0
# test if beta_1 = -2*beta_2*day
# is it on day 61? -> 
linearHypothesis(fm, rbind(c(0,1,2*61)), c(0))
# can't reject -> yes, the maximum of y is on 61-th day

# we can reduce the model, since  beta_1 = -2*beta_2*61
# original model y = beta_0 + beta_1*day + beta_2*day^2
# beta_1 = -2*beta_2*61
# reduced model y = beta_0 + (-2*beta_2*61)*day + beta_2*day^2
# y = beta_0 + beta_2(-122*day + day^2)
# y = beta_0 + (-beta_2 )*day^2
fit.red <- lm(y ~ I(-122*day + day^2),data=data)
summary(fit.red)


## Prediction for a new point
{
  # Use same names used in fm
  Z0.new <- data.frame(day=58)
  
  alpha = 0.05
  # Pred. int. for a new obs
  Pred <- predict(fm, Z0.new, interval='prediction', level=1-alpha)  
  Pred
}
#     fit    lwr    upr
#1 311.16 268.27 354.05


