setwd("C:/Users/Giuseppe/Documents/.magistrale/secondo anno/AppStat/Exams/2018/20180628/20180628")

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


data <- read.table("Hotels.txt",header=T)
head(data)


plot(data, pch=19)

# price shows periodic pattern with t


n   <- dim(data)[[1]]

# Target variable
y   <- data$price


# Model:
# Assumptions:
# 1) For parameter estimation (OLS): Homoschedasticity of residual (no need normality to estimate params!) 
#                             E(Eps) = 0  obvious if there is the intercept
#                           and  Var(Eps) = sigma^2 
# 2) For inference (so conf intervals):  Eps ~ N(0, sigma^2)


## Parameter estimation -----------------------------------------------------------------------
# Assumptions: E(Eps) = 0  and  Var(Eps) = sigma^2 

fm <- lm(y ~ Position + Season  + Position:I(1+cos(4*pi*t/365))+ Season:I(1+cos(4*pi*t/365)),data=data)
summary(fm) 
# beta0.0 = 75.489
# beta0.1 = 75.489 + 36.032
# beta0.2 = 75.489 -32.468
# beta1.0 = 74.452
# beta1.1 = 66.293
# beta1.2 = -5.204

# Residual standard error: (estimated variance of residual )^/1/2 estimate of sigma
sqrt(sum(residuals(fm)^2)/fm$df)  
coefficients(fm) 


## Model diagnostic, Verify assumptions
{
  par(mfrow=c(2,2))
  plot(fm)
  shapiro.test(residuals(fm))

  # everything ok

  par(mfrow=c(1,1))
}


linearHypothesis(fm, rbind(c(0,0,1,0,0,0), 
                           c(0,0,0,0,0,1)), c(0,0))
# Seasonality is influential

linearHypothesis(fm, rbind(c(0,1,0,0,0,0), 
                           c(0,0,0,1,0,0),
                           c(0,0,0,0,1,0)), c(0,0,0))
# Position is influential

linearHypothesis(fm, rbind(c(0,0,0,0,0,1), 
                           c(0,0,0,1,0,0),
                           c(0,0,0,0,1,0)), c(0,0,0))
# yes, as well


fm <- lm(y ~ Position + Season  + Position:I(1+cos(4*pi*t/365)),data=data)
summary(fm) 

# max when cos (..) = 1 -> ... = 0 -> t = 0
# coeff of wetseason is negative, so we don't put the wet season to get the max
# coeff of position is >0 -> we put seafront
Z0.new <- data.frame(Position="Seafront", Season="", t = 0)

alpha = 0.01
# Conf. int. for the mean
Conf <- predict(fm, Z0.new, interval='confidence', level=1-alpha)  
Conf
# fit      lwr      upr
# 1 243.4566 235.7597 251.1536







