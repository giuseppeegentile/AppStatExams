# Marco Scarpelli

# Exam 04/09/2023

# Exercise 3

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
library(nlme)
library(lme4)
library(insight)
library(plot.matrix)
library(nlmeU)
library(lattice)


rm(list=ls())
graphics.off()

df <- read.table('satisfaction.txt', header=T)

head(df)

n<-dim(df)[1]
p<-dim(df)[2]

##########################################
# Point A
fm <- lm(score  ~ purch_amount + memb_duration + age, data=df) #+ ... + regressor(r+1)) 
summary(fm)

chosen.model<- fm
coefs<-coefficients(chosen.model)
sigmasq<-sum(residuals(chosen.model)^2)/chosen.model$df
data.frame(estimated.parameters=c(coefs, 'sigma_squared'= sigmasq))

##########################################
# Point B

# We want homoscedastic residuals with 0 mean.
# To do inference, we also want residuals to be normal, with 0 mean and
#   homoscedastic
chosen.model <- fm
  
par(mfrow=c(2,2))
plot(chosen.model)
par(mfrow=c(1,1))
shapiro.test(residuals(chosen.model)) # p=0.1348

##########################################
# Point C
#(la linear hypothesis parte da beta0!)
C=rbind(c(0,0,1,0),
        c(0,0,0,1)
)
hyp0<-c(0,0) 
linearHypothesis(fm, C, hyp0) #p=0.04201
# We reject the hypothesis that, simultaneously, they
#   do not contribute to the linear model.

##########################################
# Point D

# I will remove memb_duration since from the first
#   summary it seems that, alone, its contribution is 
#   not too significant.
fm2 <- lm(score  ~ purch_amount + age, data=df) #+ ... + regressor(r+1)) 
summary(fm2)

# All other coefficients are significant. We know that
#   we should not remove both age and memb_duration, so
#   we keep age.

# Parameters:
chosen.model<- fm2
coefs<-coefficients(chosen.model)
sigmasq<-sum(residuals(chosen.model)^2)/chosen.model$df
data.frame(estimated.parameters=c(coefs, 'sigma_squared'= sigmasq))

##########################################
# Point D
fm16.1mer <- lmer(score ~ purch_amount + age + (1|store),
                  data = df)
summary(fm16.1mer)

sigma2_eps <- as.numeric(get_variance_residual(fm16.1mer))
sigma2_b <- as.numeric(get_variance_random(fm16.1mer))

PVRE <- sigma2_b/(sigma2_b+sigma2_eps)
PVRE # 0.2018298

##########################################
# Point E
dotplot(ranef(fm16.1mer, condVar=T))
ranef(fm16.1mer, condVar=T) #list of b_0is con i loro confint (ricordare che si distribuiscono come

# We can see that Store F has the highest value.