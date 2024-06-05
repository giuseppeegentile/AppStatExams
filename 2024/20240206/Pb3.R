###------------------------------------------------------------###
### Problem 3: Asthma prevalence in Italian regions (20240206) ###
###------------------------------------------------------------###


rm(list = ls())
graphics.off()

library(MASS)
library(car)
library(rgl)
library(nlme)
library(lme4)
library(insight)
library(lattice)

options(rgl.printRglwidget = TRUE)

data <- read.table("asthma.txt", header = T)
# head(data)


# a) ----------------------------------------------------------------------

m0 <- lm(asthma ~ urban + age + pollution + sunny + income + education, data = data)
# summary(m0)

rbind("Sigma", sqrt(sum((m0$residuals)^2)/m0$df))
print("Coefficients"); m0$coefficients

par(mfrow=c(2,2))
plot(m0)

# Going towards high fitted values, the residuals tend to decrease

shapiro.test(residuals(m0))

# Normality verified

# b) ----------------------------------------------------------------------

alpha = 0.1
print("90% confidence interval for age coefficient"); confint(m0, level = 1-alpha)[3, ]

# No, at 90% confidence level we can't affirm age has a positive effect on asthma prevalence
# (we would at 80%)

alpha = 0.05
print("95% confidence interval for urbanYes coefficient"); confint(m0, level = 1-alpha)[2, ]


# c) ----------------------------------------------------------------------

m0_red <- lm(asthma ~ urban + age + pollution + income + education, data = data)
summary(m0_red)

m0_red <- lm(asthma ~ age + pollution + income + education, data = data)
summary(m0_red)

m0_red <- lm(asthma ~ pollution + income + education, data = data)
summary(m0_red)

m0_red.form <- formula(asthma ~ pollution + income + education)
m1 <- gls(m0_red.form, correlation = corCompSymm(form = ~1|region_id), data = data)
summary(m1)

alpha = 0.01
intervals(m1, which = "var-cov", level = 1-alpha)


# d) ----------------------------------------------------------------------

m2 <- lmer(asthma ~ pollution + income + education + (1|region_id), data = data)
summary(m2)

sigma2_eps <- as.numeric(get_variance_residual(m2))
sigma2_b <- as.numeric(get_variance_random(m2))

PVRE <- sigma2_b/(sigma2_b+sigma2_eps)
PVRE

# The percentage of the region variance out of the total is really high (70%)

dotplot(ranef(m2, condVar=T))
confint(m2)

VarCorr(m2)

# Extra -------------------------------------------------------------------

# Exploring residuals vs. regressors
par(mfrow=c(2,3))

for(i in 1:10)
{
  if (i != 1 && i != 2 && i != 7 && i != 10) # 1: province_id; 2: region_id; 7: urban; 10: asthma
  {
    plot(as.numeric(unlist(data[i])), residuals(m0), xlab = colnames(data)[i], pch = 19)
    abline(h=0)
  }
}