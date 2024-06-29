###----------------------###
### Problem 2 (20240613) ###
###----------------------###

library(lme4)
library(insight)
library(lattice)

rm(list = ls())
graphics.off()

data <- read.table('tomatoes.txt', header = T)
head(data)


# a) ----------------------------------------------------------------------

m0 <- lm(yield ~ as.factor(species) + temp, data = data)
summary(m0)

m0$coefficients[1] + m0$coefficients["as.factor(species)2"] #beta_02
# 16.06652

m0$coefficients["temp"]
# 0.5106887


# b) ----------------------------------------------------------------------

m1 <- lmer(yield ~ temp + (1|species), data = data)
summary(m1)

sigma2_b <- as.numeric(get_variance_random(m1))
sigma2_b
# 0.9145998


# c) ----------------------------------------------------------------------

m2 <- lmer(yield ~ temp + (1 + temp|species), data = data)
summary(m2)

dotplot(ranef(m2))

fixef(m2)["temp"] + ranef(m2)$species$temp
min(fixef(m2)["temp"] + ranef(m2)$species[, 2])
# -0.1008211 -> there is a species for which we estimate that the temperature has a negative effect

which(fixef(m2)["temp"] + ranef(m2)$species[, 2] == min(fixef(m2)["temp"] + ranef(m2)$species[, 2]))
# 62


# d) ----------------------------------------------------------------------

anova(m2, m1)
#    npar    AIC    BIC  logLik deviance  Chisq Df Pr(>Chisq)    
# m1    4 9285.1 9307.6 -4638.5   9277.1                         
# m2    6 9234.9 9268.7 -4611.4   9222.9 54.183  2  1.715e-12 ***
# -> at the cost of 2 dofs more, m2 is definitely better