library(MASS)
library(car)

data <- read.table("beachVolley.txt", header=TRUE)

attach(data)
names(data)
fit <- lm(price ~ rent.length + center.distance + parking.distance + available.courts +
            shower + environment + sand.color)
summary(fit)

# model assumption
# gaussianity and homoscedasticity of the residuals
par(mfrow=c(2,2))
plot(fit)

shapiro.test(fit$residuals)
# we can assume gaussianity and homoscedasticity.

C <- rbind(c(0,0,1,0,0,0,0,0),
           c(0,0,0,1,0,0,0,0))
C

linearHypothesis(fit, C, c(0,0))
# yes thery are jointly different from zero.

# from the summary we see that we have high p value in the sand color
fit2 <- lm(price ~ rent.length + center.distance + parking.distance + available.courts +
            shower + environment)
summary(fit2)
# now available.courts is the less significant
fit3 <- lm(price ~ rent.length + center.distance + parking.distance +
             shower + environment)
summary(fit3)
# center distance is the less significant
fit4 <- lm(price ~ rent.length + parking.distance +
             shower + environment)
summary(fit4)
# coefficients and standard deviation
coefficients(fit4)
sqrt((fit4$residuals %*% fit4$residuals)/fit4$df.residual)

# prediction
newdata <- data.frame(rent.length=2.0,parking.distance=100,shower=TRUE,environment="Indoor")

prediction <- predict(fit4, newdata, interval="prediction", level=0.95)
prediction


