###---------------------------------------------------------###
### Problem 3: Dissolved oxygen in Italian lakes (20230616) ###
###---------------------------------------------------------###

library(nlme)
library(lme4)
library(insight)
library(lattice)

rm(list = ls())
graphics.off()

data <- read.table('Lakes_pollution.txt', header = T)
head(data)

n <- dim(data)[1]

# a) ----------------------------------------------------------------------

m0 <- lm(DO ~ depth + mercury_conc + ph + turbidity + wastewater_discharge, data = data)
summary(m0)

m0.red <- lm(DO ~ depth + ph + turbidity + wastewater_discharge, data = data)
summary(m0.red)

SStot <- sum((data$DO - mean(data$DO))^2)
SSres <- sum((m0.red$residuals)^2)

SSres / SStot # percentage of unexplained variability
1 - summary(m0.red)$r.squared # equivalent

par(mfrow=c(2,2))
plot(m0)
par(mfrow=c(1,1))
plot(m0.red$fitted, m0.red$residuals, pch = 19)
# Homoschedasticity OK

shapiro.test(m0.red$residuals)


# b) ----------------------------------------------------------------------

print("Average increase of DO due to increment of 3 NTU in turbidity"); m0.red$coefficients["turbidity"] * 3
print("Mean di↵erence of DO between the locations with wastewater discharge with respect to the ones that are not discharged"); m0.red$coefficients[5]


# c) ----------------------------------------------------------------------

m1 <- gls(formula(DO ~ depth + ph + turbidity + wastewater_discharge), correlation = corCompSymm(form = ~1|italian_lakes), data = data)
summary(m1)

intervals(m1, which = "var-cov")


# d) ----------------------------------------------------------------------

m2 <- lmer(DO ~ depth + ph + turbidity + wastewater_discharge + (1|italian_lakes), data = data)
summary(m2)

sigma2_eps <- as.numeric(get_variance_residual(m2))
sigma2_eps
sigma2_b <- as.numeric(get_variance_random(m2))
sigma2_b

PVRE <- sigma2_b/(sigma2_b+sigma2_eps)
PVRE
# 0.03924727 -> good

m2.eq <- lme(formula(DO ~ depth + ph + turbidity + wastewater_discharge), random = ~1|italian_lakes, data = data) # equivalent formulation (it should be)
summary(m2.eq)

anova(m1, m2.eq)
# The models seem equivalent

AICs <- c(AIC(m1), AIC(m2), AIC(m2.eq))
AICs
BICs <- c(BIC(m1), BIC(m2), BIC(m2.eq))
BICs
# BIC in m2 is slightly higher (i.e. worse) (I don't know why it's not the same as for m2.eq),
# however the PVRE is really good, so I'd choose the random intercept model


# e) ----------------------------------------------------------------------

dotplot(ranef(m2, condVar = T))
# Lake Levico is the lake associated with the lowest concentration of DO, ignoring the effect of fixed effect covariates
