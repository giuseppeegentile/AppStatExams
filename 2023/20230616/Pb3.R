library(MASS)
library(car)

data <- read.table("Lakes_pollution.txt", header=TRUE)

attach(data)
names(data)

fit <- lm(DO ~ depth + mercury_conc + ph + turbidity + wastewater_discharge)
summary(fit)

# mercury conc not significant
fit2 <- lm(DO ~ depth + ph + turbidity + wastewater_discharge)
summary(fit2)

# everything significant
# betas
coefficients(fit2)
residual.var <- (fit2$residuals%*%fit2$residuals)/fit2$df.residual
residual.var

# percentage of unexplained variability, 1-Rsquared, NOT ADJUSTED
1 - 0.9869

par(mfrow=c(2,2))
plot(fit2)
# homoscedasticity can be assumed

# if the turbidity increases of 3, the mean increment in the DO is
3*coefficients(fit2)[4]

# mean difference waterdischargesYES - waterdischargesNO
coefficients(fit2)[5]

# Compound symmetry correlation 

library(nlmeU) ## --> for the dataset
library(nlme)  ## --> for models implementation

library(corrplot)
library(lattice)
library(plot.matrix)

form <- formula(DO ~ depth + ph + turbidity + wastewater_discharge)

fit.cor <- gls(form, correlation=corCompSymm(form = ~1|italian_lakes))
summary(fit.cor)

# estimate and confidence intervals for rho and sigma
intervals(fit.cor, which = "var-cov", level=0.95)

# the confidence interval of rho contains 0

# random intercept
library(nlmeU)
library(corrplot)
library(nlme)
library(lattice)
library(plot.matrix)
library(lme4)
library(insight)
fit.ri <- lmer(DO ~ depth + ph + turbidity + wastewater_discharge + (1|italian_lakes),
                  data = data)
summary(fit.ri)

#PVRE
sigma2_eps <- as.numeric(get_variance_residual(fit.ri))
sigma2_eps
sigma2_b <- as.numeric(get_variance_random(fit.ri))
sigma2_b

PVRE <- sigma2_b/(sigma2_b+sigma2_eps)
PVRE # 3.9 % it's good on real problems, as la tipa said at lezione

# dotplot
dotplot(ranef(fit.ri, condVar=T))
# lake levico is the one associated to the lowest concentratio of DO.


# Bonus comparing the two models
AICS <- c(AIC(fit.cor), AIC(fit.ri))
AICS
BICs <- c(BIC(fit.cor), BIC(fit.ri))
BICs
# AIC is equal, BIC is lower in the CorCompSym. The PVRE however accounts for the fact that
# we are explaining some variability trough the random intercepts. I would choose the random intercept
# one because it models better the reality and the AIC and BIC score are very similar

