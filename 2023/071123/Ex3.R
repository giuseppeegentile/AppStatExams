# Marco Scarpelli

# Exam 07/11/2023

# Exercise 3

library(MASS)
library(car)
library(rgl)
library(leaps)
library(tree)
library(corrplot)
library(glmnet)
library(mvnormtest)
library(MVN)
library(heplots)
library(ggplot2)
library(mvtnorm)
library(nlme)
library(lme4)
library(insight)
library(nlmeU)
library(lattice)
library(class)
library(dbscan)
library(cluster)

# ATTENZIONE: questa libreria interferisce con biplot
# library(plot.matrix) 


rm(list=ls())
graphics.off()

df <- read.table('students.txt', header=T)

plot(df, pch=19) # pairs if p>2, matplot for time

head(df)
n<-dim(df)[1]
p<-dim(df)[2]

##########################################
# Point A

# Assumption for parameter estimation:
#   the residuals have 0 mean and are homoscedastic.

fm <- lm(watchtv ~ gender + age + height + distance + siblings
         + computertime + exercisehours + musiccds + playgames, data=df) #+ ... + regressor(r+1)) 
summary(fm)

chosen.model<- fm
coefs<-coefficients(chosen.model)
sigmasq<-sum(residuals(chosen.model)^2)/chosen.model$df
data.frame(estimated.parameters=c(coefs, 'sigma_squared'= sigmasq))

# Get RSE Residual Standard Error and coefficients
summary(chosen.model)$sigma # 4.91774 = sqrt(sigmasq)
summary(chosen.model)$coefficients
#                    Estimate   Std. Error    t value
# (Intercept)    6.4329080684 9.8143851204  0.6554571
# gendermale     0.4381438789 1.1654908305  0.3759308
# age            0.1948575363 0.1899514932  1.0258279
# height        -0.1077501870 0.1337049821 -0.8058801
# distance       0.0005002154 0.0001972114  2.5364424
# siblings       0.7107673587 0.2644621617  2.6875957
# computertime   0.1901958654 0.0562389485  3.3819243
# exercisehours  0.0460659418 0.0978122850  0.4709627
# musiccds       0.0033897185 0.0025794382  1.3141305
# playgames      0.1434152000 0.1256614085  1.1412828
#                   Pr(>|t|)
# (Intercept)   0.5132647193
# gendermale    0.7075456342
# age           0.3067675035
# height        0.4216984617
# distance      0.0123116738
# siblings      0.0080823893
# computertime  0.0009367273
# exercisehours 0.6384109566
# musiccds      0.1909824692
# playgames     0.2557288624

# Diagnostics
par(mfrow=c(2,2))
plot(chosen.model)
par(mfrow=c(1,1))
shapiro.test(residuals(chosen.model)) # p=0.01178

##########################################
# Point B
x <- model.matrix(watchtv ~ gender + age + height + distance + siblings
                  + computertime + exercisehours + musiccds + playgames, data=df)[,-1] #don't forget the-1 because of the intercept
#vector of response
y <- df$watchtv

mylambda <- .3
fit.lasso <- glmnet(x,y, lambda = mylambda)
coef.lasso <- predict(fit.lasso, s=mylambda, type = 'coefficients')   # p+1(intercetta)-1(y)=p
coef.lasso
selected =  coef.lasso[which(abs(coef.lasso)> 0.01)]
selected
# 4.08940184 0.04941395 0.45262089 0.14226808 0.02304069

##########################################
# Point C
set.seed(20231108)
lambda.grid <- seq(0.01, 10, length=1000) #lambda.grid <- 10^seq(5,-3,length=100), o "by"

cv.lasso <- cv.glmnet(x,y,lambda=lambda.grid)  #alpha=0 is ridge CV!

#Best lambda
bestlam.lasso <- cv.lasso$lambda.min
bestlam.lasso # 0.48

#Optimal lambda
optlam.lasso <- cv.lasso$lambda.1se
optlam.lasso # 10

chosen.lasso <- glmnet(x, y, lambda = optlam.lasso, alpha = 1) # 1 = Lasso
# Coefficients for bestlam.lasso
coef.lasso <- predict(chosen.lasso, 
                      s=optlam.lasso, type = 'coefficients') #also fit.lasso$beta
coef.lasso
