setwd("C:/Users/Giuseppe/Documents/.magistrale/secondo anno/AppStat/Exams/2019/20190912/20190912")
data <- read.table("mickey.txt",header=T)


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

plot(data[,1:2], pch=19) # there is some periodic behaviour
head(data)

n   <- dim(data)[[1]]

# Target variable
y   <- data$waiting.time


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

g <- ifelse(data$day.of.the.week == 'weekend', 1, 0)
fm <- lm(y ~ g + I(1+cos(4*pi*day/365)) + g:I(1+cos(4*pi*day/365)) , data =data)
summary(fm)

# alpha0 = 16.882
# alpha1 = 17.4543 + 16.882
# beta0 = 15.5056
# beta1 = 15.5056 - 2.4828 

sqrt(sum(residuals(fm)^2)/fm$df)  # s estimate of sigma
coefficients(fm)


## Model diagnostic, Verify assumptions: are hold
{
  par(mfrow=c(2,2))
  plot(fm)
  shapiro.test(residuals(fm))
  par(mfrow=c(1,1))
}

# Interpretation of the coefficients
# they all influence positively the mean average waiting time, they are all significant
# residents go more on weekends, tourists more or less the same

linearHypothesis(fm, rbind(c(0,1,0,0), c(0,0,0,1)), c(0,0))
# day of the week is influential


# Reduced model: day is important only in days of the week, but not in weekends for tourists
# for residence is important, instead
fm <- lm(y ~ g + I(1+cos(4*pi*day/365)), data =data)
summary(fm)


# maximum when cos(4*pi*day/365) = 1
# -> 4*pi*day/365 = 0 -> x=0
# also, max g = 1
# max of the mean -> confidence intervals
{
  # Use same names used in fm
  Z0.new <- data.frame(day=0, g=1)
  
  alpha = 0.05
  # Conf. int. for the mean
  Conf <- predict(fm, Z0.new, interval='confidence', level=1-alpha)  
  Conf
  #  fit = 62.352 [59.725, 64.979]
  # 60 is included in the confidence interval -> can't reject
  # we can affirm the maximum of the mean waiting times is 60 minutes
}

Z0.new <- data.frame(day=238, g=0)
Pred <- predict(fm, Z0.new, interval='prediction', level=1-alpha)  
Pred

# fit= 27.389 [10.733, 44.045]

# Assumption for inference:  Eps ~ N(0, sigma^2)
shapiro.test(residuals(fm))

