setwd("C:/Users/Giuseppe/Documents/.magistrale/secondo anno/AppStat/Exams/2021/20210120/20210120")
data <- read.table("bikes.txt",header=T)

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
head(data)
plot(data[,1:3], pch=19)

n   <- dim(data)[[1]]


# Target variable
y   <- data$bike_count 



fm <- lm(y ~ day + day:mean_temp + day:mean_wind,data=data)
summary(fm) 


# Residual standard error: (estimated variance of residual )^/1/2
sqrt(sum(residuals(fm)^2)/fm$df)  # s estimate of sigma
coefficients(fm)


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
# all ass. satisfied

linearHypothesis(fm, rbind(c(0,0,1,0,0,0),
                           c(0,0,0,1,0,0),
                           c(0,0,0,0,1,0),
                           c(0,0,0,0,0,1)), c(0,0,0,0))
# yes, there is significant evidence of the mean number of bikes rented on weather information


# b point 2: yes, there is: low pval of F statistic


# we have very high coefficients (in particular the intercept and the holiday factor)
# Also, low fitting (37.8%)

# Reduce model
linearHypothesis(fm, rbind(c(0,0,0,0,1,0),
                           c(0,0,0,0,0,1)), c(0,0))
# We can remove mean wind
fm <- lm(y ~ day + day:mean_temp,data=data)
summary(fm) 


# We can remove Noholiday
fm <- lm(y ~ day:mean_temp,data=data)
summary(fm) 


# Check if all togheter are not significantly influencing
fm <- lm(y ~ day + day:mean_temp + day:mean_wind,data=data)
linearHypothesis(fm, rbind(c(0,1,0,0,0,0),
                           c(0,0,0,0,1,0),
                           c(0,0,0,0,0,1)), c(0,0,0))
# ok
# final model
fm <- lm(y ~ day:mean_temp,data=data)
summary(fm) 
coefficients(fm)
sqrt(sum(residuals(fm)^2)/fm$df)  # s estimate of sigma
## Prediction for a new point
{
  # Use same names used in fm
  Z0.new <- data.frame(mean_temp =2, mean_temp=3, day="Holiday")
  
  alpha = 0.05
  
  # Pred. int. for a new obs
  Pred <- predict(fm, Z0.new, interval='prediction', level=1-alpha)  
  Pred
  # pointwise estimate = 355.8849

}
