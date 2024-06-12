# Marco Scarpelli

# Exam 18/01/2023

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

# ATTENZIONE: questa libreria interferisce con biplot
# library(plot.matrix) 


rm(list=ls())
graphics.off()

df<-read.table('wineReviews.txt',header = T)
df$logPrice<-log(df$price)
df$logPoints<-log(df$points)


head(df)

##########################################
# Point A
fm <- lm(points  ~ price + alcohol, data=df) #+ ... + regressor(r+1)) 
summary(fm)

fmLog <- lm(logPoints  ~ logPrice + alcohol, data=df) #+ ... + regressor(r+1)) 
summary(fmLog)

vif(fm)
vif(fmLog) # Slightly better


# Plot diagnostics for normal model
par(mfrow=c(2,2))
plot(fm)
par(mfrow=c(1,1))

par(mfrow=c(2,2))
plot(fmLog)
par(mfrow=c(1,1))
shapiro.test(residuals(fm))
shapiro.test(residuals(fmLog))

# It seems that the log-model's residuals are much
#   more uniform, and its normality is better.

##########################################
# Point B

summary(fmLog)$coefficients
#                 Estimate   Std. Error    t value
# (Intercept) 4.3758959052 0.0087475167 500.244362
# logPrice    0.0282611175 0.0011524351  24.522958
# alcohol     0.0008299285 0.0006537974   1.269397
#                 Pr(>|t|)
# (Intercept) 0.000000e+00
# logPrice    5.274785e-90
# alcohol     2.048423e-01

summary(fmLog)$sigma # 0.01752642

##########################################
# Point C
#(la linear hypothesis parte da beta0!)
C=rbind(c(0,1,0),
        c(0,0,1))
hyp0<-c(0,0) #tanti elementi quante righe di c
linearHypothesis(fmLog, C, hyp0) 

# Model 1: restricted model
# Model 2: logPoints ~ logPrice + alcohol
# 
# Res.Df     RSS Df Sum of Sq      F    Pr(>F)    
# 1    546 0.35225                                  
# 2    544 0.16710  2   0.18515 301.38 < 2.2e-16 ***

# The restricted model's RSS is double that of the first,
#   and they are different. We will keep the complete model for now.

##########################################
# Point D
summary(fmLog)
# We can remove alcohol and use logPrice

fm3 <- lm(logPoints ~ logPrice, data=df) #+ ... + regressor(r+1)) 
summary(fm3)

# Check residuals
par(mfrow=c(2,2))
plot(fm3)
par(mfrow=c(1,1))
shapiro.test(residuals(fm3))
# OK

summary(fm3)$coefficients
#               Estimate  Std. Error    t value
# (Intercept) 4.38571235 0.004090987 1072.04265
# logPrice    0.02825545 0.001153073   24.50448
#                 Pr(>|t|)
# (Intercept) 0.000000e+00
# logPrice    5.862561e-90

summary(fm3)$sigma # 0.01753625

##########################################
# Point E
fmrand <- lmer(logPoints ~ (1|region) + logPrice, data=df) #+ ... + regressor(r+1)) 
summary(fmrand)

sigma2_eps <- as.numeric(get_variance_residual(fmrand))
sigma2_b <- as.numeric(get_variance_random(fmrand))

PVRE <- sigma2_b/(sigma2_b+sigma2_eps)
PVRE # 0.2198255^

##########################################
# Point F
dotplot(ranef(fmrand, condVar=T))

# As for random effects, Puglia is the region
#   associated to the lowest number of points.