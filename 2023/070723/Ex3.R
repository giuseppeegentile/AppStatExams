# Marco Scarpelli

# Exam 07/07/2023

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
#library(plot.matrix)
library(nlmeU)
library(lattice)

rm(list=ls())
graphics.off()

df <- read.table('expenditure.txt', header=T)

head(df)
n<-dim(df)[1]
p<-dim(df)[2]

##########################################
# Point A + B
fm <- lm(avg_exp ~ income + age + perc_taxes + owns_house, data=df) #+ ... + regressor(r+1)) 
summary(fm)

chosen.model<- fm
coefs<-coefficients(chosen.model)
sigmasq<-sum(residuals(chosen.model)^2)/chosen.model$df
data.frame(estimated.parameters=c(coefs, 'sigma_squared'= sigmasq))

# Get RSE Residual Standard Error and coefficients
summary(chosen.model)$sigma # 13.68525
summary(chosen.model)$coefficients

par(mfrow=c(2,2))
plot(chosen.model)
par(mfrow=c(1,1))
shapiro.test(residuals(chosen.model)) # p=1.845e-08

# No normality; residuals exhibit a pattern.
par(mfrow=c(1,3))
plot(resid(fm) ~ income, data = df)
plot(resid(fm) ~ age, data = df) # spot a residual with ^2 dependence
# add it to linear model
plot(resid(fm) ~ perc_taxes, data = df)
par(mfrow=c(1,1))

boxplot(fm$residuals ~ df$owns_house, ylab='Residuals')

df$age2 <- df$age^2

fm2 <- lm(avg_exp ~ income + age + age2 + perc_taxes + owns_house, data=df) #+ ... + regressor(r+1)) 
summary(fm2)

chosen.model<- fm2

summary(chosen.model)$sigma # 13.68525
summary(chosen.model)$coefficients

par(mfrow=c(2,2))
plot(chosen.model)
par(mfrow=c(1,1))
shapiro.test(residuals(chosen.model)) # p=0.6793

##########################################
# Point C
alpha <- 0.05
confint(chosen.model, level=1-alpha)
# The difference is -47.42404924 since this coefficient
#   is multiplied by owns_house which i 0 or 1,
#   so in this case it is appropriate to consider the
#   beta since people who own a house will have an increase
#   of 47.

##########################################
# Point D
# We have no normality in the residuals of the initial model,
#   so it would have been appropriate to use Ridge or Lasso
#   regression since we do not have that requirement.
# T-tests performed on the initial model have no meaning
#   since we do not have normality, hence they are useless.


##########################################
# Point E
set.seed(20230707)
lambda.grid = 10^seq(10,-2,length.out=100)

x <- model.matrix(avg_exp ~ income + age + perc_taxes + owns_house, data=df)[,-1]
y <- df$avg_exp

fit.lasso.grid <- glmnet(x,y, lambda = lambda.grid) 
cv.lasso <- cv.glmnet(x,y,lambda=lambda.grid)  #alpha=0 is ridge CV!

# Plot variables w.r.t. lambda
plot(fit.lasso.grid,xvar='lambda',label=TRUE, col = rainbow(dim(x)[2]))
legend('topright', dimnames(x)[[2]], col =  rainbow(dim(x)[2]), lty=1, cex=1)

bestlam.lasso <- cv.lasso$lambda.min
optlam.lasso <- cv.lasso$lambda.1se
bestlam.lasso
optlam.lasso
plot(cv.lasso)
abline(v=log(bestlam.lasso), lty=1)

# I would use the best lambda.