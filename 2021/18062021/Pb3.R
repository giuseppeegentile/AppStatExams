setwd("C:/Users/Giuseppe/Documents/.magistrale/secondo anno/AppStat/Exams/2021/20210618/20210618")

library(nlmeU) 
library(nlme) 
library(lme4)
library(corrplot)
library(lattice)
library(insight)
library(plot.matrix)
library(MASS)
library(car)
library(rgl)
library(glmnet)

options(rgl.printRglwidget = TRUE)


data <- read.table("students.txt",header=T)
head(data)

plot(data, pch=19)

n   <- dim(data)[[1]]


# Target variable
y   <- data$watchtv



fm <- lm(y ~ gender+ age+ height+ distance +siblings +computertime +exercisehours+ musiccds +playgames,data=data)
summary(fm) 
# Look at the coefficient's: if they have high Pr:
#                             * if are positive they increase the target y

# bad fit, features are not really useful to explain variability of price
# Residual standard error: (estimated variance of residual )^/1/2
sqrt(sum(residuals(fm)^2)/fm$df)  # s estimate of sigma
coefficients(fm)

par(mfrow=c(2,2))
plot(fm)

shapiro.test(residuals(fm))
par(mfrow=c(1,1))

# assumptions are not satisfied, there is dependence in residual against fitted value
# there is also an influential point (12-th entry)
data[12,]
  

# Residual against a specific feature
{
  plot(resid(fm) ~ age, data = data)
  plot(resid(fm) ~ distance, data = data)
  plot(resid(fm) ~ siblings , data = data)
  plot(resid(fm) ~ musiccds   , data = data)
  plot(resid(fm) ~ playgames    , data = data)
}


# those are the variables causing problems in the residual
# extra exercise: fix the variables and analyze the lm with the new Z matrix
{
  data.new <- data
  
  
  # then all log transform variables with dependence on residual
  data.new$distance <- log(data.new$distance + 1)
  
  
  fm.new <- lm(y ~ gender+ age+ height+ distance +siblings +computertime +exercisehours+ musiccds +playgames,data=data.new)
  summary(fm.new) 
  summary(fm) 
  par(mfrow=c(2,2))
  plot(fm.new)
  shapiro.test(residuals(fm.new))
  
  # we fixed assumptions, but the fitted got worst..
}

attach(data)
x <- model.matrix(y ~ gender+ age+ height+ distance +siblings +computertime +exercisehours+ musiccds +playgames)[,-1] # matrix of predictors
# Get the coefficients 
lambda = 0.3
fit.lasso <- glmnet(x,y, lambda = lambda, alpha=1) # alpha=1 -> lasso
coef.lasso <- predict(fit.lasso, s=lambda, type = 'coefficients')
coef.lasso 
selected =  coef.lasso[which(abs(coef.lasso)> 0.01)]
selected

# lambda via CV
{
  grid_length = 100
  lambda.grid <- 10^seq(0,-2,length=grid_length)
  set.seed(1)
  cv.lasso <- cv.glmnet(x,y,alpha=1,nfolds=10,lambda=lambda.grid)

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

# in this case I'd chose bestlam.lasso, since the opt one will consider only one variable

lambda = bestlam.lasso
fit.lasso <- glmnet(x,y, lambda = lambda.grid, alpha=1) # alpha=1 -> lasso
coef.lasso <- predict(fit.lasso, s=lambda, type = 'coefficients')
coef.lasso 

z0 <- data.frame(gender="male", age=21, height=73,
                 distance=100, siblings=1, computertime=10, exercisehours=2, musiccds=35, playgames=4)
# Predict new data
{
  predict(fit.lasso, as.matrix(z0), s=bestlam.lasso, type = 'response')
}
