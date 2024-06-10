library(MASS)
library(car)

data <- read.table("landslides.txt", header=TRUE)

pairs(data)

attach(data)

names(data)

fit <- lm(rate ~ rain + hardness + coarse + fine)
summary(fit)

betas <- coefficients(fit)
betas

sigma <- sqrt((fit$residuals%*%fit$residuals / fit$df.residual))
sigma

par(mfrow=c(2,2))
plot(fit)

shapiro.test(fit$residuals)

# b)
summary(fit)
# from the model summary seems hardness is not statistically significant

fit2 <- lm(rate ~ rain + coarse + fine)
summary(fit2)

# c)
C <- rbind(c(0,0,1,-2))
C
linearHypothesis(fit2, C, 0)
# they are equal. beta2= 2*beta3
# y = beta0 + beta1*rain + beta2*coarse + beta3*fine becomes
# y = beta0 + beta1*rain + beta3 * 2 * coarse + beta3 * fine
# y = beta0 + beta1*rain + beta3(2*coarse + fine)

regressor.new <- 2*coarse + fine
fit4 <- lm(rate ~ rain + regressor.new)
summary(fit4) # higher R adj squared. swag

# d)
newdata <- data.frame(rain=700,regressor.new=20*8)
newdata


prediction <- predict(fit4, newdata)
prediction

CI <- predict(fit4, newdata, interval="confidence", level = 0.99)
CI

PCI<- predict(fit4, newdata, interval="prediction", level = 0.99)
PCI
