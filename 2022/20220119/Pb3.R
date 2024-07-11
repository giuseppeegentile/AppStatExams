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

data <- read.table("tattoo.txt", header=TRUE)
head(data)
attach(data)
names(data)

method <- as.factor(method)
# model fitting
lm1 <- lm(price ~  method + method:dimension + method:ncolors )
summary(lm1)

# Assumptons: residuals gaussianity and homoscedastic residuals
par(mfrow=c(2,2))
plot(lm1)
shapiro.test(lm1$residuals)
par(mfrow=c(1,1))

# coefficients estimation
coefficients(lm1) # betas
sqrt(sum(lm1$residuals^2)/lm1$df.residual) # sigma NOT sigmasquared
summary(lm1) # residual standard error is sigma

AIC(lm1)
BIC(lm1)

# linearHypothesis ----------------------------------------------------
# ncol = nparameters
# nrow tested linear combinations
summary(lm1)

# is the number of colors significant?
linearHypothesis(lm1, rbind(c(0,0,0,0,1,0),
                            c(0,0,0,0,0,1)),   c(0,0) )

# yes it is significant


# Reduce the model removing method
lm2 <- lm(price ~ method:dimension + method:ncolors)
summary(lm2)

confint(lm2, level=1-(0.05/2))
#                               0.833 %  99.167 %
#   (Intercept)               4.394664 24.010491

newobs <- data.frame(dimension=6.5, ncolors=1, method="handmade")

prediction <- predict(lm2, newobs, interval="confidence", level=1-(0.05/2))
prediction
#       fit      lwr      upr
# 1 117.9534 115.0973 120.8094

###PROVE SAVAGE
intercept <- lm(price ~  method + method:dimension + method:ncolors )
summary(intercept)
#                           Estimate Std. Error t value Pr(>|t|)    
#   (Intercept)               11.8177     5.6950   2.075   0.0394 *  
#   methodmachine              4.8659     8.1347   0.598   0.5505    
#   methodhandmade:dimension  15.7374     0.7939  19.824  < 2e-16 ***
#   methodmachine:dimension    8.4876     0.5658  15.001  < 2e-16 ***
#   methodhandmade:ncolors     3.5540     0.5840   6.085 6.66e-09 ***
#   methodmachine:ncolors      2.5615     0.5951   4.304 2.73e-05 ***

nointercept <- lm(price ~ -1 + method + method:dimension + method:ncolors )
summary(nointercept)
#                        Estimate Std. Error t value Pr(>|t|)    
# methodhandmade            11.8177     5.6950   2.075  0.03937 *  
# methodmachine             16.6836     5.8087   2.872  0.00456 ** 
# methodhandmade:dimension  15.7374     0.7939  19.824  < 2e-16 ***
# methodmachine:dimension    8.4876     0.5658  15.001  < 2e-16 ***
# methodhandmade:ncolors     3.5540     0.5840   6.085 6.66e-09 ***
# methodmachine:ncolors      2.5615     0.5951   4.304 2.73e-05 ***

# is method significant?
# -> giacomino translation: is the effect of the factor method on the intercept statistically
# significant
# intercept model
summary(intercept)
# methodmachine              4.8659     8.1347   0.598   0.5505 
# it's not significant

# no intercept model
# is b0-b1=0? -> b0=b1? -> can i write 
# y = b0(methodmachine + methodhandmade) + b1*methodhandmade*dimension + b2*methodmachine*dimension
# + b3*methodhandmade*ncolors + b4*methodmachine*ncolors -> being methodmachine + methodhandmade = 1 for
# each observation it is equal to fitting a model with an intercept.
linearHypothesis(nointercept, rbind(c(1,-1,0,0,0,0)),   c(0) )
# Linear hypothesis test
# 
# Hypothesis:
#   methodhandmade - methodmachine = 0
# 
# Model 1: restricted model
# Model 2: price ~ -1 + method + method:dimension + method:ncolors
# 
# Res.Df   RSS Df Sum of Sq      F Pr(>F)
# 1    184 12506                           
# 2    183 12481  1    24.403 0.3578 0.5505

# we obtain the same pvalue (coincidence? I don't think so) -> not significant

# now the question is asking only for the intercept? Or the factor method alltogether?
# meaning i can fit a model without method
linearHypothesis(intercept, rbind(c(1,-1,0,0,0,0),
                                  c(0,0,1,-1,0,0),
                                  c(0,0,0,0,1,-1)),   c(0,0,0) )






