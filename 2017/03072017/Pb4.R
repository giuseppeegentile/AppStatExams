setwd("C:/Users/Giuseppe/Documents/.magistrale/secondo anno/AppStat/Exams/2017/20170703/20170703")
library(nlmeU) 
library(nlme) 
library(lme4)
library(corrplot)
library(lattice)
library(insight)
library(plot.matrix)
library(MASS)
library(car)
library(rgl)
library(glmnet)

options(rgl.printRglwidget = TRUE)


data <- read.table("garden.txt",header=T)
head(data)

plot(data, pch=19)
# ->  cherry and maple correlated, stones and caprs correlated


n   <- dim(data)[[1]]

# Target variable
y   <- data$extension
# Model:
# distance = beta_0 + beta_1 * speed + beta_2 * speed^2 + Eps
# (linear in the parameters!)

# Assumptions:
# 1) For parameter estimation (OLS): Homoschedasticity of residual (no need normality to estimate params!) 
#                             E(Eps) = 0  obvious if there is the intercept
#                           and  Var(Eps) = sigma^2 
# 2) For inference (so conf intervals):  Eps ~ N(0, sigma^2)


## Parameter estimation -----------------------------------------------------------------------
# Assumptions: E(Eps) = 0  and  Var(Eps) = sigma^2 
fm <- lm(y ~ carps+ maple+ cherry +stones,data=data)
summary(fm) 
sqrt(sum(residuals(fm)^2)/fm$df)  # s estimate of sigma
coefficients(fm)

vif(fm) # collinearity issue


par(mfrow=c(2,2))
plot(fm)

shapiro.test(residuals(fm))
par(mfrow=c(1,1))
# assumptions are hold


# There is statistical evidence of a dependence of the mean garden extension on the number of maple or cherry
# trees because pval < 2.2e-16
linearHypothesis(fm, rbind(c(0,0,1,0,0), c(0,0,0,1,0)), c(0,0))


# There is statistical evidence of a dependence of the mean garden extension on the number of stones and caprs
# trees because pval < 3.822e-15
linearHypothesis(fm, rbind(c(0,1,0,0,0), c(0,0,0,0,1)), c(0,0))


# The only problem is that there is high correlation between variables, between maple and cherry (together are significant)
# and between stones and carps
vif(fm) 
plot(data, pch=19)

# we try to see if cherry and carps are significant together, and we remove case they aren't, fixing the collinearity
linearHypothesis(fm, rbind(c(0,1,0,0,0), c(0,0,0,1,0)), c(0,0))
# they aren't
fm.red1 <- lm(y ~ maple +stones,data=data)
summary(fm.red1) 

# we can remove the intercept

fm.red2 <- lm(y ~ -1 + maple +stones,data=data)
summary(fm.red2) 

# Better 1 for AIC
AIC(fm.red1)
AIC(fm.red2)

# Better 2 for BIC
BIC(fm.red1)
BIC(fm.red2)

anova(fm.red1, fm.red2)
# there isn't significant difference, we keep the model without intercept (1 DoF more)


par(mfrow=c(2,2))
plot(fm.red2)

shapiro.test(residuals(fm.red2))
par(mfrow=c(1,1))
# assumptions are hold

fm <- fm.red2
sqrt(sum(residuals(fm)^2)/fm$df)  # s estimate of sigma
coefficients(fm)





