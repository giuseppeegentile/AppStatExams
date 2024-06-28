###----------------------###
### Problem 3 (20230118) ###
###----------------------###

library(car)
library(lme4)
library(insight)
library(lattice)

rm(list = ls())
graphics.off()

data <- read.table('wineReviews.txt', header = T)
head(data)

n <- dim(data)[1]


# a) ----------------------------------------------------------------------

m0 <- lm(points ~ price + alcohol, data = data)
summary(m0)

par(mfrow=c(2,2))
plot(m0)
par(mfrow=c(1,1))
# We see a bit of heteroschedasticity and leveraging effect

shapiro.test(m0$residuals)
# 0.5678 -> OK

r <- 2
par(mfrow=c(2,floor(r/2)+r%%2))

for(i in 1:r)
{
  plot(data[, i], m0$residuals, xlab = colnames(data)[i], pch = 19)
  abline(h = 0)
}
# We see some log-like shape when we plot against the target variable

par(mfrow=c(1,1))
plot(m0$fitted, m0$residuals, pch = 19)

# -

m0.log <- lm(log(points) ~ log(price) + alcohol, data = data)
summary(m0.log)

par(mfrow=c(2,2))
plot(m0.log)
par(mfrow=c(1,1))
# It seems a beauty!

shapiro.test(m0.log$residuals)
# 0.6504 -> even better than before!

r <- 2
par(mfrow=c(2,floor(r/2)+r%%2))

for(i in 1:r)
{
  plot(log(data[, i]), m0.log$residuals, xlab = colnames(data)[i], pch = 19)
  abline(h = 0)
}

par(mfrow=c(1,1))
plot(m0.log$fitted, m0.log$residuals, pch = 19)


# b) ----------------------------------------------------------------------

m0.log$coefficients
#  (Intercept)   log(price)      alcohol
# 4.3758959052 0.0282611175 0.0008299285

sqrt(sum((m0.log$residuals)^2)/m0.log$df)
# 0.01752642


# c) ----------------------------------------------------------------------

linearHypothesis(m0.log,
                 rbind(c(0,1,0),
                       c(0,0,1)),
                 c(0,0))["Pr(>F)"]
# 2.2e-16 -> price and alcohol can't be both discarded from the model


# d) ----------------------------------------------------------------------

linearHypothesis(m0.log,
                 rbind(c(0,0,1)),
                 c(0))["Pr(>F)"]
# 0.2048 -> we can discard the alcohol variable

m1.log <- lm(log(points) ~ log(price), data = data)
summary(m1.log)


# e) ----------------------------------------------------------------------

m2 <- lmer(log(points) ~ log(price) + (1|region), data = data)
summary(m2)

sigma2_eps <- as.numeric(get_variance_residual(m2))
sigma2_b <- as.numeric(get_variance_random(m2))

PVRE <- sigma2_b/(sigma2_b+sigma2_eps)
PVRE
# 0.2198255


# f) ----------------------------------------------------------------------

dotplot(ranef(m2, condVar=T))
# Puglia is the region associated to the lowest number of points