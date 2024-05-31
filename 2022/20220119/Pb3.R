setwd("C:/Users/Giuseppe/Documents/.magistrale/secondo anno/AppStat/Exams/2022/20220119/20220119")
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


data <- read.table("tattoo.txt",header=T)
head(data)

plot(data[,1:2],pch=19)

n   <- dim(data)[[1]]


# Target variable
y   <- data$price



fm <- lm(y ~ method + method:dimension + method:ncolors,data=data)
summary(fm) 


# Residual standard error: (estimated variance of residual )^/1/2
sqrt(sum(residuals(fm)^2)/fm$df)  # s estimate of sigma
coefficients(fm)

# alpha0 = 11.817743
# alpha1 = 11.817743 + 4.86588
# beta0 = 15.737418
# beta1 = 15.737418 + 8.487567
# gamma0 = 3.553972                 
# gamma1 = 3.553972 + 2.561518 



## Model diagnostic, Verify assumptions
{
  par(mfrow=c(2,2))
  plot(fm)
  
  shapiro.test(residuals(fm))
  par(mfrow=c(1,1))
}
vif(fm)


summary(fm) 
# method has significative (at any level) impact on the y, since the F statistic has low pval

linearHypothesis(fm, rbind(c(0,0,0,0,1,0), c(0,0,0,0,0,1)), c(0,0))
# yes the number of colors has impact on the price


# d)
fm <- lm(y ~ method:dimension + method:ncolors,data=data)
summary(fm)

# Residual standard error: (estimated variance of residual )^/1/2
sqrt(sum(residuals(fm)^2)/fm$df)  # s estimate of sigma
coefficients(fm)

# alpha0 = 14.202578
# beta0 = 15.422247
# beta1 = 15.422247 + 8.715589
# gamma0 = 3.506168                  
# gamma1 = 3.506168  + 2.646478


# mean fixed cost of a tattoo: is the mean contribution of being handmade
# -> beta_0
{
  alpha = 0.05
  confint(fm, level= 1-alpha)[1,]
}



## Prediction for a new point
{
  Z0.new <- data.frame(dimension=6.5,ncolors=1, method="handmade")
  
  alpha = 0.05
  
  # Mean  -> confidence interval
  Pred <- predict(fm, Z0.new, interval='confidence', level=1-alpha)  
  Pred
 
}