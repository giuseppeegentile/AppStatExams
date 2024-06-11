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

data <- read.table("satisfaction.txt", header=TRUE)

attach(data)
names(data)

fit <- lm( score ~ purch_amount + memb_duration + age)
coef(fit)

resVar <- (fit$residuals %*% fit$residuals) / fit$df.residual
resVar
summary(fit)

par(mfrow=c(2,2))
plot(fit)
# we do not see evidence of eteroschedasticity in the residual plot
# and the QQ plot shows enough gaussianity

shapiro.test(fit$residuals) # the test further confirms my hypothesis

linearHypothesis(fit,rbind(c(0,0,1,0),c(0,0,0,1)), c(0,0))
# they are significant at level 5%

# model reduction trough extensive backward search
summary(fit)
# we see that memb-duration has the high p-value
fit2 <- lm( score ~ purch_amount + age)
summary(fit2)
# also age has high p value at level 5%
fit3 <- lm(score  ~ purch_amount)
summary(fit3)

# however the R squared is going down as we reduce the regressors, as expected.
# i would just keep the original model


lmm<- lmer( score ~ purch_amount + memb_duration + age+(1|store),
                  data = data)

sigma2_eps <- as.numeric(get_variance_residual(lmm))
sigma2_eps
sigma2_b <- as.numeric(get_variance_random(lmm))
sigma2_b


PVRE <- sigma2_b/(sigma2_b+sigma2_eps)
PVRE # 20 % chiara masci direbbe very nice porcodio


dotplot(ranef(lmm, condVar=T))
# i would say store F is the one associated with the highest score, since the random effect creates
# is higher.
