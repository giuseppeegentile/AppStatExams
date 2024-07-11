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


data <- read.table("pc.txt", header=TRUE)
head(data)
attach(data)
OS <- factor(OS)
names(data)
## SIMPLE LINEAR MODELS -----------------------------------------------------------------------
# model fitting
lm1 <- lm(price ~ freq:OS + cache_acc:OS)
summary(lm1)

# Assumptions:
# 1) Parameter estimation: E(Eps) = 0  and  Var(Eps) = sigma^2 
# 2) Inference:            Eps ~ N(0, sigma^2)
# for estimating parameters you don't need the gaussianity assumption
par(mfrow=c(2,2))
plot(lm1)
shapiro.test(lm1$residuals)
par(mfrow=c(1,1))

# plot residuals against variables (to detect pattern, indicating possible transformations to do)
plot(lm1$residuals ~ )


# coefficients estimation
coefficients(lm1) # betas
sum(lm1$residuals^2)/lm1$df.residual 
summary(lm1) # residual standard error is sigma


# does the factor OS have a significant impact?
factor <- factor(data$OS)
response <- data$price
ps <-NULL
for(i in 1:length(levels(factor))){
    ps <- c(ps,shapiro.test(response[which(factor==levels(factor)[i])])$p)
}
ps

vars <- NULL
for(i in 1:length(levels(factor))){
  vars <- c(vars, var(response[which(factor==levels(factor)[i],)]))
}
vars

bartlett.test(response, factor) 

fit <- aov(response ~ factor)
summary(fit)
# it is significant

# do it with linearHypothesis
linearHypothesis(lm1, rbind(c(0,1,-1,0,0,0,0),
                            c(0,0,1,-1,0,0,0),
                            c(0,-1,0,1,0,0,0),
                            c(0,0,0,0,1,-1,0),
                            c(0,0,0,0,0,1,-1),
                            c(0,0,0,0,-1,0,1)),   c(0,0,0,0,0,0) )


# does time to access have a significant impact?
# ncol = nparameters
# nrow tested linear combinations
linearHypothesis(lm1, rbind(c(0,0,0,0,1,0,0),
                            c(0,0,0,0,0,1,0),
                            c(0,0,0,0,0,0,1)),   c(0,0,0) )
# time to access is not significant

lm2 <- lm(price ~ freq:OS)
summary(lm2)

# Diagnostic, is it really better?
par(mfrow=c(2,2))
plot(lm2)
shapiro.test(lm2$residuals) #looks nice

AIC(lm2,lm1) #better lm2
BIC(lm2,lm1) #better lm1

coefficients(lm2)
sum(lm2$residuals^2)/lm2$df.residual


### Prediction ---------------------------------------------------------------------
newobs <- data.frame(freq=3.2,OS="Windows")

# Pred. int. for a new obs
Pred <- predict(lm2, newobs, interval='prediction', level=1-0.1)  
Pred

