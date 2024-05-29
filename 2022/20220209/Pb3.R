setwd("C:/Users/Giuseppe/Documents/.magistrale/secondo anno/AppStat/Exams/2022/20220209/20220209")

library(nlmeU) 
library(nlme) 

library(corrplot)
library(lattice)
library(insight)
library(plot.matrix)
library(MASS)
library(car)
library(rgl)
library(glmnet)

options(rgl.printRglwidget = TRUE)


data <- read.table("wine.txt",header=T)
head(data)


n   <- dim(data)[[1]]


# Target variable
y   <- data$alcohol

fm <- lm(y ~ type + type:sugar,data=data)
summary(fm) 


coefficients(fm)
{
  # beta0.0 -1.25338
  # beta0.1 -1.25338 -0.45853
  # beta0.2 -1.25338 +0.87848
  # beta1.0 0.72728
  # beta1.1 0.66214
  # beta1.1 0.59625
}

# Residual standard error: (estimated variance of residual )^/1/2
sqrt(sum(residuals(fm)^2)/fm$df) 




## Model diagnostic, Verify assumptions
{
  par(mfrow=c(2,2))
  plot(fm)
  # 1st: no pattern of fitted values with the residual: we want normality around zero.
  #       we don't want dependence of residual with the fitted value
  #         it also can indicate heteroschedasticity: "the tunnel"
  #       ideally: you should do against every variable: if you observe a pattern
  #                 with a variable, you should include in the variable
  # 2nd: QQ plot: must have diagonal fit. Acceptable to have some outliers
  # 3rd: standardized residual: similar to 1st. Just look at the first
  # 4th: residual vs leverage:
  #     Detect influential points: 
  #       if cook's distance is high (point >1) that point is influential
  
  
  shapiro.test(residuals(fm))
  par(mfrow=c(1,1))
}


linearHypothesis(fm, rbind(c(1,0,0,0,0,0), 
                           c(0,1,0,0,0,0),
                           c(0,0,1,0,0,0),
                           c(0,0,0,1,0,0),
                           c(0,0,0,0,1,0),
                           c(0,0,0,0,0,1)), c(0,0,0,0,0,0))



linearHypothesis(fm, rbind(c(0,0,0,1,0,0),
                           c(0,0,0,0,1,0),
                           c(0,0,0,0,0,1)), c(0,0,0))
# For both the test can't reject H0


# we try to test if the type without interaction is significatly influencing alcohol
# since we notice from summary that those coeff have high pval
linearHypothesis(fm, rbind(c(0,1,0,0,0,0),
                           c(0,0,1,0,0,0)), c(0,0))
# indeed, high pval

fm <- lm(y ~ type:sugar,data=data)
summary(fm) 


## Prediction for a new point
{
  # Use same names used in fm
  Z0.new <- data.frame(type="Red", sugar    =20)
  
  alpha = 0.01

  # Pred. int. for a new obs
  Pred <- predict(fm, Z0.new, interval='prediction', level=1-alpha)  
  Pred
  
}