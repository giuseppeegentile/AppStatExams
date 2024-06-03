setwd("C:/Users/Giuseppe/Documents/.magistrale/secondo anno/AppStat/Exams/2023/20230707/20230707")

library(MASS)
library(car)
library(rgl)
library(glmnet)

options(rgl.printRglwidget = TRUE)

data <- read.table('expenditure.txt', header=T)
head(data)

dim(data)

y <- data$avg_exp 
n <- dim(data)[1]
p <- dim(data)[2]
pairs(data, pch=19)
# Income and perc_taxes are one linear combination of the other
# Also correlated with avg_Exp


# a)
fm <- lm(y ~ income + age + perc_taxes + owns_house, data=data)
summary(fm) 
# We have a good fit (not meaning is a good model)


# Estimate
sqrt(sum(residuals(fm)^2)/fm$df ) # estimate of sigma
coefficients(fm)

# b)

## Model diagnostic, Verify assumptions
{
  par(mfrow=c(2,2))
  plot(fm)
  # 1st: we have a quadratic relationship
  # 2nd: QQ plot: no gauss hold
  # 3rd: standardized residual: similar to 1st. Just look at the first
  # 4th: we don't have influential points
  
  
  shapiro.test(residuals(fm))
  # Very low pval -> gaussianity not satisfied, but is not a problem for estimation
  #                                           of parameters
}

# Residual against features

# Residual against all features
{
  par(mfrow=c(1,1))
  plot(resid(fm) ~ income, data = data)
  plot(resid(fm) ~ age, data = data) # spot a residual with ^2 dependence
  # add it to linear model
  plot(resid(fm) ~ perc_taxes, data = data)
  
  boxplot(fm$residuals ~ data$owns_house, xlab='owns_house', ylab='Residuals')
}



fm.add <- lm(y ~ income + age + perc_taxes + owns_house + I(age^2), data=data)
summary(fm.add) 
#Even better fit than previous model

par(mfrow=c(2,2))
plot(fm.add)
# 1st: no patterns
# 2nd: QQ plot: gaussian now
# 3rd: standardized residual: similar to 1st. Just look at the first
# 4th: we don't have influential points
shapiro.test(residuals(fm.add)) # high pval


# c)
# The difference is
coefficients(fm.add)[5]
alpha = 0.05
confint(fm, level= 1-alpha)[5,]


# d) as sayed in point a, there is collinearity between variables. 
# Using penalization with the aim to perform variable reduction is beneficial for the 
# model.
# Indeed if we look at the initial model's vif:
vif(fm)
# income and perc_taxes are basically linear combination

# e)
set.seed(20230707)

y <- data$avg_exp
x <- model.matrix(y ~ income + age + perc_taxes + owns_house, data=data)[,-1] # matrix of predictors


# lambda via grid search
{
  grid_length = 100
  lambda.grid <- 10^seq(10,-2,length=grid_length) # always set decreasing lambda!
  fit.lasso <- glmnet(x,y, lambda = lambda.grid, alpha=1) # alpha=1 -> lasso
  
  
  par(mfrow=c(1,1))
  plot(fit.lasso,xvar='lambda',label=TRUE, col = rainbow(dim(x)[2]))
  legend('topright', dimnames(x)[[2]], col =  rainbow(dim(x)[2]), lty=1, cex=1)
  
  # with the aim of variable selection, for any value of lambda perc_taxes is removed
  
  # we can think to remove also owns_house, but is better to use CV
}
# via CV
{
  cv.lasso <- cv.glmnet(x,y, alpha=1, nfolds=10,lambda=lambda.grid)
  
  bestlam.lasso <- cv.lasso$lambda.min
  bestlam.lasso
  
  optlam.lasso <- cv.lasso$lambda.1se
  optlam.lasso
  
  plot(cv.lasso)
  abline(v=log(bestlam.lasso), lty=1)
  
  
  plot(fit.lasso,xvar='lambda',label=TRUE, col = rainbow(dim(x)[2]))
  abline(v=log(bestlam.lasso))
  abline(v=log(optlam.lasso))
}
#Maybe is better to take lambda as 
optlam.lasso
# Since we've already performed variable selection with that value, reducing the collinearity
# Get the coefficients for the optimal lambda
coef.lasso <- predict(fit.lasso, s=bestlam.lasso, type = 'coefficients')
coef.lasso 



fit.lasso <- glmnet(x,y, lambda = bestlam.lasso, alpha=1)
fit.lasso
