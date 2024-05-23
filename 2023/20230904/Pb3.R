setwd("C:/Users/Giuseppe/Documents/.magistrale/secondo anno/AppStat/Exams/2023/20230904/20230904")
library(MASS)
library(class)
library(rgl)
library(mvtnorm)
library(MVN)
library(car)
library(glmnet)
options(rgl.printRglwidget = TRUE)

data <- read.table('satisfaction.txt', header = T)
head(data)

dim(data)
plot(data, pch=19)
#purch_amount and score correlated, also with memb_duration
# we expect collinearity

y   <- data$score
fm <- lm(y ~ purch_amount + memb_duration + age,data=data)
summary(fm) 

# betas
coefficients(fm)
# s estimate of sigma
sqrt(sum(residuals(fm)^2)/fm$df)  

# b)
par(mfrow=c(2,2))
plot(fm)
# assumptions are held: no pattern in residual, normality and no leverages

# c)
linearHypothesis(fm, rbind(c(0,0,1,0), c(0,0,0,1)), c(0,0))
# we can reject at 5% (but not at 1%)

# -> at 5% they have effect on the satisfaction score

# d)
# Proceed by remving variables one at the time, from summary, memb_duration can be removed
# with any significative confidence
fm <- lm(y ~ purch_amount + age,data=data)
summary(fm) 
# we may also remove age at level 1%
fm <- lm(y ~ purch_amount,data=data)
summary(fm) 

# the fitting is still good (we didn't loose much)

plot(data$score, data$purch_amount)

# e)
library(nlmeU) 
library(nlme)  

library(ggplot2)
library(insight)
library(lme4)
library(corrplot)
library(lattice)
library(plot.matrix)

lmm1 = lmer(y ~ purch_amount + (1|store),data=data)
summary(lmm1)



# e)
# PVRE
{
  # Also called the intraclass correlation (ICC), since is also an estimate of the within 
  # cluster correlation.
  sigma2_eps <- as.numeric(get_variance_residual(lmm1))
  sigma2_eps
  sigma2_b <- as.numeric(get_variance_random(lmm1))
  sigma2_b
  
  PVRE <- sigma2_b/(sigma2_b+sigma2_eps)
  PVRE
}
# the percentage of variance explained by random effect is 20%
# there is a clear difference in how stores affect the prices 

# f)
# dotplot
{
  # effect of the school_id on the y
  dotplot(ranef(lmm1))
  
  # Random intercepts and fixed slopes: (beta_0+b_0i, beta_1, beta_2)
  coef(lmm1)
  head(coef(lmm1)$store)
}
# store f is associated to the highest score
# Intercept coefficient 1.2654239

