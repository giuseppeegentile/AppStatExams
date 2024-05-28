setwd("C:/Users/Giuseppe/Documents/.magistrale/secondo anno/AppStat/Exams/2023/20230207/20230207")
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


data <- read.table("beachVolley.txt",header=T)
head(data)
plot(data, pch=19)

# price and rent.length correlated (reasonable)

n   <- dim(data)[[1]]
# Target variable
y   <- data$price


fm <- lm(y ~ rent.length + center.distance + parking.distance + available.courts +
           shower + environment + sand.color,data=data)
summary(fm) 



## Model diagnostic, Verify assumptions
{
  par(mfrow=c(2,2))
  plot(fm)

  # no pattern in residual
  
  shapiro.test(residuals(fm))
  # ok, high pval
  
}

linearHypothesis(fm, rbind(c(0,0,1,0,0,0,0,0), c(0,0,0,1,0,0,0,0)), c(0,0))
# yes, they are (jointly) significantly impacting the price of the house, even if 
# distance from center doesn't


# d)
# Sand color is not significantly different from zero
fm <- lm(y ~ rent.length + center.distance + parking.distance + available.courts +
           shower + environment,data=data)
summary(fm) 

# Available court is not significantly different from zero
fm <- lm(y ~ rent.length + center.distance + parking.distance +
           shower + environment,data=data)
summary(fm) 
# distance from center is not significantly different from zero
fm <- lm(y ~ rent.length  + parking.distance +
           shower + environment,data=data)
summary(fm) 
coefficients(fm)
# Residual standard error: (estimated variance of residual )^/1/2
sqrt(sum(residuals(fm)^2)/fm$df) 




## Prediction for a new point
{
  # Use same names used in fm
  Z0.new <- data.frame(rent.length=2, parking.distance=100, shower=TRUE, environment = 'Indoor')
  
  alpha = 0.05
  # Pred. int. for a new obs
  Pred <- predict(fm, Z0.new, interval='prediction', level=1-alpha)  
  Pred
  
}




