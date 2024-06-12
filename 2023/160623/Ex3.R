# Marco Scarpelli

# Exam 16/06/2023

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

set.seed(1)

df <- read.table('Lakes_pollution.txt', header=T)

head(df)

##########################################
# Point A
fm <- lm(DO ~ depth + wastewater_discharge + mercury_conc + ph + turbidity, data=df) #+ ... + regressor(r+1)) 
summary(fm)

chosen.model <- fm
par(mfrow=c(2,2))
plot(chosen.model)
par(mfrow=c(1,1))
shapiro.test(residuals(chosen.model))

# Residuals OK

# Remove mercury concentration
fm2 <- lm(DO ~ depth + wastewater_discharge + ph + turbidity, data=df) #+ ... + regressor(r+1)) 
summary(fm2)

chosen.model <- fm2
par(mfrow=c(2,2))
plot(chosen.model)
par(mfrow=c(1,1))
shapiro.test(residuals(chosen.model))

# Residuals slightly worse but OK
anova(fm, fm2)
# RSS is slightly lower in the reduced version
#   and they are equal, so we use that.

summary(fm2)

# Percentage of unexplained variance
1 - summary(fm2)$r.squared # 0.01313434

chosen.model <- fm2
coefs<-coefficients(chosen.model)
sigmasq<-sum(residuals(chosen.model)^2)/chosen.model$df
#data.frame(estimated.parameters=c(coefs, 'sigma_squared'= sigmasq))

##########################################
# Point B

# Get RSE Residual Standard Error and coefficients
summary(chosen.model)$sigma
summary(chosen.model)$coefficients

# From the summary; increase:
3.0005436 * 3 # 9.001631
# Difference:
# 1.8970702 

##########################################
# Point C

fm12.1 <- gls(DO ~ depth + wastewater_discharge + ph + turbidity, 
              correlation = corCompSymm(form = ~1|italian_lakes),      # for dependance
              data = df)
summary(fm12.1)
# rho: 0.03924728 
intervals(fm12.1, # 95% CIs for rho, delta, sigma
          which = "var-cov")          
# Approximate 95% confidence intervals
# 
# Correlation structure:
#           lower       est.     upper
# Rho -0.04945498 0.03924728 0.2158089
# 
# Residual standard error:
#    lower     est.    upper 
# 1.072834 1.218486 1.383913 

##########################################
# Point D
fm16.1mer <- lmer(DO ~ depth + wastewater_discharge + ph + turbidity + (1|italian_lakes),
                  data = df)
summary(fm16.1mer)

plot(fm16.1mer)  ## Pearson and raw residuals are the same now

qqnorm(resid(fm16.1mer))
qqline(resid(fm16.1mer), col='red', lwd=2)

qqnorm(unlist(ranef(fm16.1mer)$italian_lakes), main='Normal Q-Q Plot - Random Effects on Intercept')
qqline(unlist(ranef(fm16.1mer)$italian_lakes), col='red', lwd=2)

sigma2_eps <- as.numeric(get_variance_residual(fm16.1mer))
sigma2_b <- as.numeric(get_variance_random(fm16.1mer))

PVRE <- sigma2_b/(sigma2_b+sigma2_eps)
PVRE # 0.03924725

##########################################
# Point E
dotplot(ranef(fm16.1mer, condVar=T))
