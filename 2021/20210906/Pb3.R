###----------------------###
### Problem 3 (20210906) ###
###----------------------###

library(car)

rm(list = ls())
graphics.off()

data <- read.table('boats.txt', header = T)
head(data)

n <- dim(data)[1]


# a) ----------------------------------------------------------------------

m0 <- lm(price ~ material + length + power + draught + crew + year, data = data)
summary(m0)

m0$coefficients
#   (Intercept)  materialwood        length         power       draught          crew          year 
# -1.107377e+04  4.373853e+02  3.166172e+02  1.041251e-01  4.297902e+01  6.525758e+02  4.787693e+00

sum((m0$residuals)^2)/m0$df
# 21007.11

par(mfrow=c(2,2))
plot(m0)
par(mfrow=c(1,1))

shapiro.test(m0$residuals)
# p-value = 0.2438


# b) ----------------------------------------------------------------------

C <- rbind(c(0,0,1,0,0,0,0),
           c(0,0,0,1,0,0,0),
           c(0,0,0,0,1,0,0))

linearHypothesis(m0, C, c(0,0,0))["Pr(>F)"]
# < 2.2e-16 *** -> at least one of the coefficients related to variables related to the dimension of the boat is not 0


# c) ----------------------------------------------------------------------

C <- rbind(c(0,1,0,0,0,0,0),
           c(0,0,0,0,0,1,0))

linearHypothesis(m0, C, c(0,0))["Pr(>F)"]
# < 2.2e-16 *** -> at least one of the coefficients related to variables related to accessory features is not 0


# d) ----------------------------------------------------------------------

C <- rbind(c(0,0,0,0,1,0,0))

linearHypothesis(m0, C, c(0))["Pr(>F)"]
# 0.4104 -> draught can be removed

m1 <- lm(price ~ material + length + power + crew + year, data = data)
summary(m1)

C <- rbind(c(0,0,0,0,0,1))

linearHypothesis(m1, C, c(0))["Pr(>F)"]
# 0.05198 . -> year can be removed

m2 <- lm(price ~ material + length + power + crew, data = data)
summary(m2)

C <- rbind(c(0,0,0,1,0))

linearHypothesis(m2, C, c(0))["Pr(>F)"]
# 0.03432 * -> let's try to remove power too, althought it may be significant, and compare the reduced model with m2

m3 <- lm(price ~ material + length + crew, data = data)
summary(m3)

AIC(m3, m2)
anova(m3, m2)
# AIC is a bit lower for m2, but since we obtain a DoF for the residuals, m3 seems better...

shapiro.test(m2$residuals)
shapiro.test(m3$residuals)
# p-values = 0.1515 and 0.05762
# ... however, the normality assumption on the residual tends to be invalidated


# d) ----------------------------------------------------------------------

alpha = 0.05

new.datum <- data.frame(length = 10, 
                        power = 1070, 
                        draught = 1.5, 
                        crew = 1,
                        year = 2015,
                        material = "fiberglass")

Pred <- predict(m3, new.datum, interval = 'prediction', level = 1 - alpha) 
Pred
#        fit      lwr      upr
#   2449.827 2136.456 2763.198