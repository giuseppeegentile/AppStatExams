#16.21
library(MASS)
library(car)
library(rgl)
library(nlmeU) ## --> for the dataset
library(nlme)  ## --> for models implementation

library(corrplot)
library(lattice)
library(plot.matrix)

data <- read.table("StorageCentres.txt",header=TRUE)

attach(data)
names(data)
time
n <-dim(data)[1]

fit <- lm(costs ~ time + costs0 + growth:time + rad_less_15_city + size)
summary(fit)

# coefficients
fit$coefficients
# standard deviation
r <-5
sqrt((fit$residuals %*% fit$residuals)/(n-r-1))
# AIC, the less the better
AIC(fit)

par(mfrow=c(2,2))
plot(fit)
# i can confirm the assumption of homoscedasticity
par(mfrow=c(1,1))


# to check if the model is missing something we plot the residual variance wrt to time and to
# the group
boxplot(fit$residuals ~ data$time)
boxplot(fit$residuals ~ data$id_storage_centre)

# we see that variability of observation changes wrt to the groups and in particular it
# increases as time goes on.

fit2 <- gls(costs ~ time + costs0 + growth:time + rad_less_15_city + size,  
             weights = varPower(form = ~time), 
             data = data)
summary(fit2)

# AIC
AIC(fit2)
# delta
fit2$modelStruct$varStruct

# implement M2
# i assume i have to group wrt to the storage centre id
fit3 <- update(fit2, correlation=corAR1(form= ~time|id_storage_centre))
summary(fit3)

# rho -> phi?
fit3$modelStruct$corStruct

intervals(fit3, which="var-cov")

# anova test to check which model is best
anova(fit2, fit3)
# no statistical relevance to say one i sbetter than the other

# we prefer fit2 due to its semplicity and better values of AIC and BIC
