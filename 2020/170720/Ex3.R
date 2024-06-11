# Marco Scarpelli

# Exam 17/07/2020

# Exercise 2

library(MASS)
library(car)
library(rgl)   #3D plots
library(leaps) #best subset selection
library(tree)  #decision trees
library(corrplot) #correlation
library(glmnet)
library(mvnormtest)
library(MVN)
library(heplots)
library(ggplot2)
library(mvtnorm)

rm(list=ls())
graphics.off()

df <- read.table('toxicity.txt', header=T)

head(df)

n<-dim(df)[1]
p<-dim(df)[2]

##########################################
# Point A
fm <- lm(tox  ~ C1 + C2 + C3 + C4 + C5 + C6, data=df)
summary(fm)
# Coefficients:
#   Estimate Std. Error t value Pr(>|t|)    
# (Intercept) 30.15705    1.84491  16.346   <2e-16 ***
# C1           0.14915    0.01334  11.180   <2e-16 ***
# C2           0.65670    1.29088   0.509    0.612    
# C3           0.72840    0.45802   1.590    0.115    
# C4          -7.04633    0.45972 -15.327   <2e-16 ***
# C5           0.47276    1.07374   0.440    0.661    
# C6           5.98045    0.38619  15.486   <2e-16 ***

# Estimated parameters:
chosen.model<- fm
coefs<-coefficients(chosen.model)
sigmasq<-sum(residuals(chosen.model)^2)/chosen.model$df
data.frame(estimated.parameters=c(coefs, 'sigma_squared'= sigmasq))
#               estimated.parameters
# (Intercept)             30.1570460
# C1                       0.1491506
# C2                       0.6567033
# C3                       0.7284042
# C4                      -7.0463329
# C5                       0.4727555
# C6                       5.9804451
# sigma_squared           12.8713301

# Model assumptions: homoscedastic residuals with 0 mean.
par(mfrow=c(2,2))
plot(chosen.model)
par(mfrow=c(1,1))
shapiro.test(residuals(chosen.model))
#         Shapiro-Wilk normality test
# 
# data:  residuals(chosen.model)
# W = 0.98191, p-value = 0.1866

##########################################
# Point B

Z0.new <- data.frame(C1=100, C2=0.7, C3=2, C4=4, C5=1.4, C6=3)
globalAlpha <- 0.05
myalphaCORRECTED<-globalAlpha 
Conf <- predict(fm, Z0.new, 
                interval='predict', level=1-myalphaCORRECTED)  
t(Conf)
# 1
# fit 37.40647
# lwr 29.93171
# upr 44.88122

##########################################
# Point C

x <- model.matrix(tox  ~ C1 + C2 + C3 + C4 + C5 + C6, data=df)[,-1] #don't forget the-1 because of the intercept
y <- df$tox
lambda.grid <- seq(0.01,1,length=200) #lambda.grid <- 10^seq(5,-3,length=100), o "by"
fit.lasso.grid <- glmnet(x,y, lambda = lambda.grid) 

# Plot variables:
plot(fit.lasso.grid,xvar='lambda',label=TRUE, col = rainbow(dim(x)[2]))
legend('topright', dimnames(x)[[2]], col =  rainbow(dim(x)[2]), lty=1, cex=1)

set.seed(1) 
#default: 10-fold CV, if you have low n better add nfolds=5 or 3
cv.lasso <- cv.glmnet(x,y,lambda=lambda.grid)  #alpha=0 is ridge CV!

bestlam.lasso <- cv.lasso$lambda.min
bestlam.lasso # 0.169196

# Optimal lambda
optlam.lasso <- cv.lasso$lambda.1se
optlam.lasso # 0.6865829

plot(cv.lasso)
abline(v=log(optlam.lasso), lty=1)

opt.lasso <- glmnet(x, y, lambda = optlam.lasso, alpha = 1) # 1 = Lasso
# Coefficients for bestlam.lasso
coef.lasso <- predict(opt.lasso, 
                      s=optlam.lasso, type = 'coefficients') #also fit.lasso$beta
coef.lasso 
# Coefficients:
#             s1
# (Intercept) 29.3505875
# C1           0.1118093
# C2           .        
# C3           0.4535429
# C4          -5.3235344
# C5           .        
# C6           5.0318487

##########################################
# Point D

predict(opt.lasso, newx=as.matrix(Z0.new), s=optlam.lasso, type = "response")
# Prediction:
# 1
# 35.24001