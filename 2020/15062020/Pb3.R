setwd("C:/Users/Giuseppe/Documents/.magistrale/secondo anno/AppStat/Exams/2020/20200615/20200615")

data <- read.table("airfoil.txt",header=T)

plot(data[,1:2])


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


n   <- dim(data)[[1]]


# Target variable
y   <- data$sound

fm <- lm(y ~ velocity + velocity:frequency,data=data)
summary(fm) 

sqrt(sum(residuals(fm)^2)/fm$df)  # s estimate of sigma
coefficients(fm)



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
}


linearHypothesis(fm, rbind(c(0,0,1,0), c(0,0,0,1)), c(0,0))
# yes, there is



# there is dependence with velocity, low pval of the F statistic





linearHypothesis(fm, rbind(c(0,0,1,-1)), c(0))
# We can't reject that the increase of Y because of the frequencies when we have high and low velocity is the same

fm <- lm(y ~ velocity + frequency,data=data)
summary(fm) 

sqrt(sum(residuals(fm)^2)/fm$df)  # s estimate of sigma
coefficients(fm)




# Use same names used in fm
Z0.new <- data.frame(frequency =15000, velocity="H")

alpha = 0.05
# Conf. int. for the mean
Conf <- predict(fm, Z0.new, interval='confidence', level=1-alpha)  
Conf








