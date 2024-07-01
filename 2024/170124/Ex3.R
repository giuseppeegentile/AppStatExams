# Marco Scarpelli

# Exam 2024/01/17

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

df <- read.table('StorageCentres.txt', header=T)
df$rad_less_15_city <- factor(df$rad_less_15_city)
#df$time <- factor(df$time)
df$growth <- factor(df$growth)

head(df)
n<-dim(df)[1]
p<-dim(df)[2]

plot(df, main='', pch=19) # plot if p=1 or 2, pairs otherwise

##########################################
# Point A

# Hypotheses: parameter estimation requires the residuals be homoscedastic
#   with 0 mean; inference requires them to be distributed as a Gaussian with
#   0 mean and a constant, diagonal variance matrix.

fm <- lm(costs ~ time + costs0 + growth:time + rad_less_15_city + size, data=df) #+ ... + regressor(r+1)) 
summary(fm)

chosen.model<- fm
coefs<-coefficients(chosen.model)
sigmasq<-sum(residuals(chosen.model)^2)/chosen.model$df
sigmasq.sqrt <- sqrt(sigmasq)
data.frame(estimated.parameters=c(coefs, 'sigma'= sigmasq.sqrt))
#                   estimated.parameters
# (Intercept)               -299.6983020
# time                       -11.5391670
# costs0                       0.8024675
# rad_less_15_city1          127.2306123
# size                         1.9957220
# time:growth0                37.3910344
# time:growth1                75.7967984
# sigma                        5.3314446
AIC(fm) # 1245.899

##########################################
# Point B
par(mfrow=c(2,2))
plot(chosen.model)
par(mfrow=c(1,1))
shapiro.test(residuals(chosen.model)) # 0.005284

# It looks like the residuals increase with the abs. value of the data.

boxplot(fm$residuals ~ df$time,  xlab='Time.f', ylab='Residuals')  
# There is heteroscedasticity in the model residuals.

##########################################
# Point C
lmm.heteroscedastic <- gls(costs ~ time + costs0 + growth:time + rad_less_15_city + size,
                           weights = varPower(form = ~time), # Var. function; <delta, v_it>-group
                           data = df)
summary(lmm.heteroscedastic)

chosen_lmm <- lmm.heteroscedastic
  
AIC(chosen_lmm) # 1190.585 (lower, hence better)
intervals(chosen_lmm, which = "var-cov")  
# Variance function:
#           lower      est.    upper
# power 0.7056602 0.8836898 1.061719

anova(lmm.heteroscedastic, fm)
# p-value is low: models are different.

##########################################
# Point D
lmm.heteroscedastic.2 <- gls(costs ~ time + costs0 + growth:time + rad_less_15_city + size, 
              weights = varPower(form = ~time),
              correlation = corAR1(form = ~time|id_storage_centre),
              data = df)

summary(lmm.heteroscedastic.2)

AIC(lmm.heteroscedastic.2) # 1192.151 (very slightly higher than the first LMM).
anova(lmm.heteroscedastic, lmm.heteroscedastic.2)
# Models are the same (p=0.5104), so we can use the first one
#   since it has less parameters.