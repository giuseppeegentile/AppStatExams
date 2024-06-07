library(nlmeU)
library(corrplot)
library(nlme)
library(lattice)
library(plot.matrix)
library(lme4)
library(insight)
library(nlmeU) ## --> for the dataset
library(nlme)  ## --> for models implementation

library(corrplot)
library(lattice)
library(plot.matrix)


data <- read.table("asthma.txt", header=TRUE)

attach(data)

names(data)
fit <- lm(asthma ~-1 + urban + age + pollution + sunny + tobacco + income + education, data = data)
summary(fit)
par(mfrow=c(2,2))


shapiro.test(fit$residuals) # we can assume normality at 5%

par(mfrow=c(2,2))
plot(fit)
# and we do not see any structure in the residuals, so we may conclude that the homoscedasticity
# assumption holds.

# from the summary:
# age          2.885      1.584   1.821   0.0715 . 
# yes we can assume it has significance at level 10%


# confidence interval for the mean difference between the asthma prevalence in an urban province
# and in a non-urban one
fit1 <- lm(asthma ~ urban + age + pollution + sunny + tobacco + income + education, data = data)
confint(fit1, "urbanYes")


# reduce the model
summary(fit1)
# remove education
fit2 <- lm(asthma ~ urban + age + pollution + sunny + tobacco + income, data = data)
summary(fit2)
# remove age
fit3 <- lm(asthma ~ urban + pollution + sunny + tobacco + income, data = data)
summary(fit3)
# now they are all significant
plot(fit3)

# update it by introducing a compound-Symmetry Correlation structure
lm1.form <- formula(asthma ~ urban + pollution + sunny + tobacco + income)
fm12.1 <- gls(lm1.form,
              correlation = corCompSymm(form = ~1|region_id),
              data = data)
summary(fm12.1)
# this are the estimates for the two parameters
intervals(fm12.1, which = "var-cov", level=0.99)

# add a random intercept to the model
fm16.1mer <- lme(asthma ~ urban + pollution + sunny + tobacco + income ,
                  random = ~1|region_id,
                  data = data)
summary(fm16.1mer)
summary(fm12.1)
# we observe that the AIC, BIC are equal
# the problem is that if i use lmer the AIC and the BIC are not displayed.
# in this case it does not even make sense to use lme since the residuals are homoscedastic
