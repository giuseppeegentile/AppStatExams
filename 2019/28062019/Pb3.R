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


data <- read.table("airport.txt",header=T)
head(data)

y <- data$duration 

n   <- dim(data)[[1]]
fm <- lm(y ~ time.of.the.day + distance:time.of.the.day,data=data)
summary(fm) 
# Residual standard error: (estimated variance of residual )^/1/2
sqrt(sum(residuals(fm)^2)/fm$df)  # s estimate of sigma
coefficients(fm)


# beta_00 : 17.228
# beta_01 : 17.228 -16.170
# beta_02 : 17.228 - 3.891
# beta_10 : 1.157
# beta_11 : 1.451
# beta_12 : 1.740




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
  # 4th: residual vs leverage:
  #     Detect influential points: 
  #       if cook's distance is high (point >1) that point is influential
  
  
  shapiro.test(residuals(fm))
  

  
  par(mfrow=c(1,1))
} # everythin ok

# Yes, there is, low pval for F statistic

# about distance
linearHypothesis(fm, rbind(c(0,0,0,1,0,0), 
                           c(0,0,0,0,1,0),
                           c(0,0,0,0,0,1)), c(0,0,0))
# dependence



fm <- lm(y ~ time.of.the.day + distance:time.of.the.day,data=data)
summary(fm) 
linearHypothesis(fm, rbind(c(1,0,0,0,0,0), 
                           c(0,1,0,0,0,0),
                           c(0,0,1,0,0,0)), c(0,0,0))
# -> we can reduce the time in the intercept
fm <- lm(y ~ distance:time.of.the.day,data=data)
summary(fm)


## Prediction for a new point
{
  # Use same names used in fm
  Z0.new <- data.frame(distance=57, time.of.the.day="6-10")
  
  alpha = 0.01

  # Pred. int. for a new obs
  Pred <- predict(fm, Z0.new, interval='prediction', level=1-alpha)  
  Pred 
  # you will have to travel 112 mins, so to be at 9.30 on airport, take 7.30
  
}



