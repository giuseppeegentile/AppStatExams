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

data <- read.table("cameo.txt", header=TRUE)

attach(data)
names(data)
head(data)

# model fitting
processing <- factor(processing)
lm1 <- lm( price ~ -1 + processing + processing:dimension + processing:weight + processing:dimension:weight)
summary(lm1)

par(mfrow=c(2,2))
plot(lm1)
# looks good from the plot

# coefficients estimation
coefficients(lm1) # betas
summary(lm1) # residual standard error is sigma

# linearHypothesis
# ncol = nparameters
# nrow tested linear combinations
linearHypothesis(lm1, rbind(c(1,0,0,0,0,0,0,0),
                            c(0,1,0,0,0,0,0,0)),   c(0,0) )
# it is significant
summary(lm1)
# we see that the last term is not significant
lm2 <- lm( price ~ -1 + processing + processing:dimension + processing:weight)
summary(lm2)

linearHypothesis(lm2, rbind(c(1,0,0,0,0,0),
                            c(0,1,0,0,0,0)),   c(0,0) )
summary(lm2)
# we have the reduced model

# comment: we have a very good Adjusted R squared, and the assumptions are met.
par(mfrow=c(2,2))
plot(lm2)

newobs <- data.frame(dimension=10, weight=80, processing="handmade")
prediction <- predict(lm2, newobs)
prediction
# the prediction is not reliable since the new data is of a completely different magnitude with
# respect of the data on which the model hass been fitted
