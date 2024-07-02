###----------------------###
### Problem 3 (20220909) ###
###----------------------###

library(car)

rm(list = ls())
graphics.off()

data <- read.table('cameo.txt', header = T)
head(data)

n <- dim(data)[1]


# a) ----------------------------------------------------------------------

m0 <- lm(price ~ processing + dimension:processing + weight:processing + dimension:weight:processing, data = data)
summary(m0)

m0$coefficients
sum((m0$residuals)^2)/m0$df
# 46.13588


# b) ----------------------------------------------------------------------

par(mfrow=c(2,2))
plot(m0)
par(mfrow=c(1,1))

shapiro.test(m0$residuals)

linearHypothesis(m0,
                 rbind(c(0,1,0,0,0,0,0,0)),
                 c(0))["Pr(>F)"]
# 0.00501 **


# c) ----------------------------------------------------------------------

linearHypothesis(m0,
                 rbind(c(0,0,0,0,0,0,1,0),
                       c(0,0,0,0,0,0,0,1)),
                 c(0,0))["Pr(>F)"]
# 0.3767

m1 <- lm(price ~ processing + dimension:processing + weight:processing, data = data)
summary(m1)


# d) ----------------------------------------------------------------------

par(mfrow=c(2,2))
plot(m1)
par(mfrow=c(1,1))

shapiro.test(m1$residuals)

summary(m1)$r.squared
# 0.9704085

summary(m1)$adj.r.squared
# 0.9697329

1 - summary(m1)$r.squared # percentage of unexplained variability
# 0.02932642

AIC(m1, m0)
#    df      AIC
# m1  7 1508.509
# m0  9 1510.485

# e) ----------------------------------------------------------------------

new.datum <- data.frame(dimension = 10, weight = 80, processing = "handmade")

# Pred. int. for a new obs
Pred <- predict(m1, new.datum, interval = 'prediction', level = 1-0.05) 
Pred

Pred[1, "fit"]
# 381.7968

range(data[which(data$processing == "handmade"), ]$dimension)
# 2.02 7.98

range(data[which(data$processing == "handmade"), ]$weight)
# 12.03 44.90

range(data[which(data$processing == "handmade"), ]$price)
# 85.61 249.86

# The new datum we are trying to fit presents very different characteristics wrt 
# the ones of data used to perform the fit 
# -> uncertain estimate since we are very far from the baricenter