library(MASS)
library(nlmeU)
library(corrplot)
library(nlme)
library(lattice)
library(plot.matrix)
library(lme4)
library(insight)
library(car)
library(rgl)
library(glmnet)
library(ISLR)
library(leaps)


data <- read.table("boats.txt", header=TRUE)
head(data)
attach(data)
names(data)
## SIMPLE LINEAR MODELS -----------------------------------------------------------------------
# model fitting
lm1 <- lm(price ~length + power + draught + crew + year + material )
summary(lm1)
# Assumptions:
# 1) Parameter estimation: E(Eps) = 0  and  Var(Eps) = sigma^2 
# 2) Inference:            Eps ~ N(0, sigma^2)

# for estimating parameters you don't need the gaussianity assumption
par(mfrow=c(2,2))
plot(lm1)
shapiro.test(lm1$residuals)

# coefficients estimation
coefficients(lm1) # betas
sqrt(sum(lm1$residuals^2)/lm1$df.residual) # this is sigma not sigma squared!
summary(lm1) # residual standard error is sigma

AIC(lm1)
BIC(lm1)

#b i think it is asking for linear hypothesis on length, power, draught


### LinearHypothesis ----------------------------------------------------
# ncol = nparameters
# nrow tested linear combinations
linearHypothesis(lm1, rbind(c(0,1,0,0,0,0,0),
                            c(0,0,1,0,0,0,0),
                            c(0,0,0,1,0,0,0)),   c(0,0,0) )
# all together they are significant

# c
# linearhypothesis on crew and deck material 
linearHypothesis(lm1, rbind(c(0,0,0,0,1,0,0),
                            c(0,0,0,0,0,0,1)),   c(0,0) )

# all together they are significant

#d
summary(lm1)
# we remove the less significant -> draught
lm2 <- lm(price ~length + power + crew + year + material )
summary(lm2)

# we remove the less significant -> year
lm3 <- lm(price ~length + power + crew  + material )
summary(lm3)
# now everything is significant at 5% level

#e
newobs <- data.frame(length=10,power=1070,crew=1,year=2015, material="fiberglass")
prediction <- predict(lm3, newobs, interval="prediction", level=0.95)
prediction

# prova senza power
lm4 <- lm(price ~length  + crew  + material )
summary(lm4)

anova(lm3,lm4)

par(mfrow=c(2,2))
plot(lm4)
shapiro.test(lm4$residuals)

AIC(lm3)
AIC(lm4)
BIC(lm3)
BIC(lm4)

# lm3 is better