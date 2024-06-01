rm(list=ls())
graphics.off()

library(corrplot)
library(plot.matrix)
library(ggplot2)
library(insight)
library(lattice)
library(lme4)
library(nlme)
data <- read.table('pc.txt', header=TRUE)
head(data)
data$OS = as.factor(data$OS)
head(data)
fit1= lm(price ~ OS:freq +OS:cache_acc, data=data)
summary(fit1)
fit2 = lm(price ~ freq +cache_acc, data=data)
summary(fit2)
anova(fit1,fit2)

par(mfrow=c(2,2))
plot(fit1)
shapiro.test(residuals(fit1))

par(mfrow=c(2,2))
plot(fit2)
shapiro.test(residuals(fit2))
par(mfrow=c(1,1))

fit3 = lm(price ~ OS:cache_acc, data=data)
par(mfrow=c(2,2))
plot(fit3)
shapiro.test(residuals(fit3))
par(mfrow=c(1,1))

anova(fit1,fit3)

fit4 = lm(price ~ OS:freq, data=data)
par(mfrow=c(2,2))
plot(fit4)
shapiro.test(residuals(fit4))
par(mfrow=c(1,1))

anova(fit1,fit4)

head(data)

Z0.new <- data.frame(freq=3.2, cache_acc=10,OS = 'Windows')
#Z0.new <- data.frame(regressor1=c(2,2), regressor2=c(0,1))
myalphaCORRECTED<-0.1
# Confidence 
Conf <- predict(fit4, Z0.new, 
                interval='confidence', level=1-myalphaCORRECTED)  
t(Conf) # fit is the first value!

