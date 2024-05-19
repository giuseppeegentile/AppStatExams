library(MASS)
library(car)
library(rgl)
library(glmnet)


library(ggplot2)
library(insight)
library(lattice)
library(lme4)


options(rgl.printRglwidget = TRUE)
setwd("C:/Users/Giuseppe/Documents/.magistrale/secondo anno/AppStat/Exams/2023/20230118/20230118")

{
  data <- read.table("wineReviews.txt",header=T)
  head(data)
  
  plot(data, pch=19)
  
  n   <- dim(data)[[1]]
}

# Target variable
y   <- data$points
# we can already see from the plot that price an points are correlated
# instead alcohol doesn't, since seems a random point displacement



fm <- lm(y ~ price + alcohol, data = data)
summary(fm) 
# as expected: alcohol is not influent in explaining the variability of y
# moreover, both R^2 (varability explained by the model) and adjusted R^2 are not high
# -> fitted value y_hat is not close to y
# -> the variability of y is not explained well by regression


fm.log <- lm(log(y) ~ log(price) + log(alcohol), data = data)
summary(fm.log) 
# we get same conclusions, but slightly better fit


# b)
# estimates of beta:
coefficients(fm.log)
sqrt(sum(residuals(fm.log)^2)/fm.log$df)

# c)
linearHypothesis(fm.log, rbind(c(0,1,0), c(0,0,1)), c(0,0))
# we reject the H0 hypothesis with any significative confidence
# -> at least one is useful to explain y


# d) from summary we can see that alcohol can be removed from the model
fm.log.reduced <- lm(log(y) ~ log(price) , data = data)
summary(fm.log.reduced) 

coefficients(fm.log.reduced)
sqrt(sum(residuals(fm.log.reduced)^2)/fm.log.reduced$df)


# e)
fm.log.reduced.reg <- lmer(log(y) ~ log(price) + (1|region) , data = data)
summary(fm.log.reduced.reg)


sigma2_eps <- as.numeric(get_variance_residual(fm.log.reduced.reg))
sigma2_eps
sigma2_b <- as.numeric(get_variance_random(fm.log.reduced.reg))
sigma2_b

# Another way to interpret the variance output is to note percentage of the student variance out 
# of the total, i.e. the Percentage of Variance explained by the Random Effect (PVRE).
# This is also called the intraclass correlation (ICC), because it is also an estimate of the within 
# cluster correlation.
PVRE <- sigma2_b/(sigma2_b+sigma2_eps)
PVRE
# 21% 


dotplot(ranef(fm.log.reduced.reg))
# -> puglia

# Da capire ancora bene i LMM 

