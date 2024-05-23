setwd("C:/Users/Giuseppe/Documents/.magistrale/secondo anno/AppStat/Exams/2024/20240117/20240117")
library(MASS)
library(car)
library(rgl)
library(glmnet)

library(nlmeU) ## --> for the dataset
library(nlme)  ## --> for models implementation

library(corrplot)
library(lattice)
library(plot.matrix)

options(rgl.printRglwidget = TRUE)


{
  data <- read.table("StorageCentres.txt",header=T)
  head(data)

  plot(data, pch=19)
  
  n   <- dim(data)[[1]]
}


y   <- data$costs


# Assumptions: E(Eps) = 0  and  Var(Eps) = sigma^2 
fm <- lm(y ~ time + costs0 + growth*time + rad_less_15_city + size,data=data)
summary(fm) 

sqrt(sum(residuals(fm)^2)/fm$df)  # s estimate of sigma
coefficients(fm)
AIC(fm)


#b)
par(mfrow=c(2,2))
plot(fm)
#We see that the residual increase with the fitted value, that is heteroschedasticity
#of the residual. 

par(mfrow=c(1,1))
# Is not capturing the residual increasing with time
boxplot(fm$residuals ~ data$time,  xlab='Time.f', ylab='Residuals')  

# We model the variance as a function of time: 

#c)
fm9.2 <- gls(y ~ time + costs0 + growth*time + rad_less_15_city + size,
             weights = varPower(form = ~time), # Var. function; <delta, v_it>-group
             data = data)
summary(fm9.2)
# delta estimated as: 0.8808694

AIC(fm9.2)
#lower than before, better

anova(fm9.2,fm) #very low pval, there is difference in the models
# We may chose this model because with one parameter more we increment the AIC
# and we have a significative difference



# d)
fm12.2 <- gls(y ~ time + costs0 + growth*time + rad_less_15_city + size, 
              weights = varPower(form = ~time),
              correlation = corAR1(form = ~1|time),
              data = data)

summary(fm12.2)

# Confidence intervals for phi and sigma
{
  intervals(fm12.2, which = "var-cov")
  # If CI for phi is 0 -> lack of autocorrelation of first order among the
  #                                       repeated measures within subject.
}


anova(fm12.2,fm9.2)
# AIC and BIC are comparable. Moreover, there isn't a significative difference in the
# models -> we choose to use the model M1, since has fewer parameters.
