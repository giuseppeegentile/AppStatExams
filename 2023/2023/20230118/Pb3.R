library(MASS)
library(car)
library(rgl)
library(nlmeU)
library(corrplot)
library(nlme)
library(lattice)
library(plot.matrix)
library(lme4)
library(insight)

data <- read.table("wineReviews.txt", header=TRUE)
head(data)
attach(data)

lm1 <- lm(points ~ price + alcohol)
summary(lm1)
par(mfrow=c(2,2))
plot(lm1)

lnpoint <- log(points)
lnprice <- log(price)
lm2 <- lm(lnpoint ~ lnprice + alcohol)
summary(lm2)
plot(lm2)

# i would choose model 2 since it exhibits clear gaussian residuals, no patterns in the residuals plot and
# no leverages
# In model 1 instead residuals are more concentrated on the left part of the plot, indicating a pattern
# that should not sussist.

coefficients(lm2)
summary(lm2) # sigma = 0.018

linearHypothesis(lm2, rbind(c(0,1,0),c(0,0,1)), c(0,0))
# no they cannot be removed from the model

summary(lm2)
# from the summary we can see that the alcohol variable is not statistically significant 

lm3 <- lm(lnpoint ~ lnprice)
summary(lm3) #sigma=0.018
coefficients(lm3)

region <- factor(region)
lmm <- lmer(lnpoint ~ lnprice + (1|region), data = data)
summary(lmm)

sigma2_eps <- as.numeric(get_variance_residual(lmm))
sigma2_eps
sigma2_b <- as.numeric(get_variance_random(lmm))
sigma2_b
PVRE <- sigma2_b/(sigma2_b+sigma2_eps)
PVRE # 0.220


dotplot(ranef(lmm, condVar=T))
# Puglia's wine is the one with the lowest score
