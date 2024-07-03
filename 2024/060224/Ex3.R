# Marco Scarpelli

# Exam 2024/02/06

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

df <- read.table('asthma.txt', header=T)

#df$urban <- factor(df$urban)
#df$region_id <- factor(df$region_id)
#df$province_id <- factor(df$province_id)


head(df)

plot(df, main='', pch=19) 

##########################################
# Point A

# 1) Parameter estimation requires: E(Eps) = 0  and  Var(Eps) = sigma^2 
# 2) Inference requires:            Eps ~ N(0, sigma^2)

fm <- lm(asthma ~ urban + age + pollution +
           sunny + income + education, data=df) #+ ... + regressor(r+1)) 
summary(fm)
# Residuals:
#    Min     1Q Median     3Q    Max 
# -48.87 -11.67   2.92  11.00  48.34 
# 
# Coefficients:
#             Estimate Std. Error t value Pr(>|t|)    
# (Intercept)    51.41       3.55   14.47  < 2e-16 ***
# urbanYes        5.87       3.74    1.57  0.12001    
# age             2.62       1.86    1.41  0.16207    
# pollution      -6.20       2.95   -2.10  0.03820 *  
# sunny           2.62       1.87    1.40  0.16426    
# income         -7.24       1.90   -3.82  0.00023 ***
# education       3.80       1.88    2.02  0.04573 *  
# ---
# Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1
# 
# Residual standard error: 19.3 on 103 degrees of freedom
# Multiple R-squared:  0.239,	Adjusted R-squared:  0.195 
# F-statistic: 5.39 on 6 and 103 DF,  p-value: 7e-05


chosen.model<- fm
coefs<-coefficients(chosen.model)
sigmasq<-sum(residuals(chosen.model)^2)/chosen.model$df
data.frame(estimated.parameters=c(coefs, 'sigma_squared'= sigmasq))
#               estimated.parameters
# (Intercept)                  51.41
# urbanYes                      5.87
# age                           2.62
# pollution                    -6.20
# sunny                         2.62
# income                       -7.24
# education                     3.80
# sigma_squared               371.06

par(mfrow=c(2,2))
plot(chosen.model)
par(mfrow=c(1,1))
shapiro.test(residuals(chosen.model)) # p=0.2
# Residuals tend to deviate for higher values, but all
#   observations are within Cook's distance.

# Point B
# From the summary age's p value is 0.16, so we cannot say
#   it is significant at a 10% confidence level.

alpha = 0.05
confint(fm, level= 1-alpha)
#                2.5 % 97.5 %
# (Intercept)  44.3639 58.455
# urbanYes     -1.5551 13.289 <--------------
# age          -1.0702  6.312
# pollution   -12.0495 -0.344
# sunny        -1.0884  6.324
# income      -11.0061 -3.484
# education     0.0734  7.523

##########################################
# Point C
# I will try to remove the variables whose p>0.1; sunny:
fm2 <- lm(asthma ~ urban + age + pollution +
           income + education, data=df) #+ ... + regressor(r+1)) 
summary(fm2)

# Urban:
fm3 <- lm(asthma ~ age + pollution +
            income + education, data=df) #+ ... + regressor(r+1)) 
summary(fm3)

# Age:
fm4 <- lm(asthma ~ pollution +
            income + education, data=df) #+ ... + regressor(r+1)) 
summary(fm4)

# Education is at 6.5%, so I will try to remove that too:
fm5 <- lm(asthma ~ pollution +
            income, data=df) #+ ... + regressor(r+1)) 
summary(fm5)

lmm <- gls(asthma ~ pollution + income, 
                             correlation = corCompSymm(form = ~1|region_id),
                             data = df)

summary(lmm)

chosen_lmm <- lmm
  
# From the summary:
intervals(chosen_lmm, which = "var-cov", level=0.99)  
# Approximate 99% confidence intervals
# 
#  Correlation structure:
#     lower est. upper
# Rho 0.456  0.7 0.863
# 
#  Residual standard error:
# lower  est. upper 
#  15.0  20.6  28.5 

##########################################
# Point D
M2 <- lmer(asthma ~ pollution + income + (1|region_id), 
           data = df)

summary(M2)
#             Estimate Std. Error t value
# (Intercept)    53.06       4.27   12.43


chosen.model <- M2

confint(chosen.model, oldNames=TRUE)

par(mfrow=c(2,2))
plot(chosen.model)
par(mfrow=c(1,1))
shapiro.test(residuals(chosen.model)) # p=0.9 (very nice)

dotplot(ranef(M2))
