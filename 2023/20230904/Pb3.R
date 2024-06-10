###---------------------------------------------------###
### Problem 3: Customer satisfaction score (20230904) ###
###---------------------------------------------------###

rm(list = ls())
graphics.off()

library(MASS)
library(car)
library(rgl)
library(nlme)
library(lme4)
library(insight)
library(lattice)

data <- read.table("satisfaction.txt", header = T)
head(data)


# a) ----------------------------------------------------------------------

m0 <- lm(score ~ purch_amount + memb_duration + age, data = data)
summary(m0)

rbind("Sigma", sqrt(sum((m0$residuals)^2)/m0$df))
print("Coefficients"); m0$coefficients


# b) ----------------------------------------------------------------------

par(mfrow=c(2,2))
plot(m0)

# No heteroschedasticity

shapiro.test(residuals(m0))

# Normality verified


# c) ----------------------------------------------------------------------

linearHypothesis(m0,
                 rbind(c(0,0,1,0),
                       c(0, 0,0,1)),
                 c(0,0))

# Yes, at 95% we can state that age and memb_duration doesn't affect the satisfaction score

alpha = 0.05
print("95% confidence interval for age coefficient"); confint(m0, level = 1-alpha, "age")

alpha = 0.05
print("95% confidence interval for memb_duration coefficient"); confint(m0, level = 1-alpha, "memb_duration")


# d) ----------------------------------------------------------------------

m0_red <- lm(score ~ purch_amount + age, data = data)
summary(m0_red)

m0_red <- lm(score ~ purch_amount, data = data)
summary(m0_red)

rbind("Sigma", sqrt(sum((m0_red$residuals)^2)/m0_red$df))
print("Coefficients"); m0_red$coefficients


# e) ----------------------------------------------------------------------

m1 <- lmer(score ~ purch_amount + (1|store), data = data)
summary(m1)

sigma2_eps <- as.numeric(get_variance_residual(m1))
sigma2_eps
sigma2_b <- as.numeric(get_variance_random(m1))
sigma2_b

PVRE <- sigma2_b/(sigma2_b+sigma2_eps)
PVRE # 20%: not that high


# f) ----------------------------------------------------------------------

dotplot(ranef(m1, condVar=T))

# Store F has the highest random intercept