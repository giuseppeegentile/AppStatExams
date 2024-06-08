setwd("C:/Users/Giuseppe/Documents/.magistrale/secondo anno/AppStat/Exams/2019/20190719/20190719")
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


data <- read.table('piadeina.txt', header = T)
head(data)
plot(data,pch=19)


n   <- dim(data)[[1]]

# Target variable
y   <- data$Sales



# Model:
# distance = beta_0 + beta_1 * speed + beta_2 * speed^2 + Eps
# (linear in the parameters!)

# Assumptions:
# 1) For parameter estimation (OLS): Homoschedasticity of residual (no need normality to estimate params!) 
#                             E(Eps) = 0  obvious if there is the intercept
#                           and  Var(Eps) = sigma^2 
# 2) For inference (so conf intervals):  Eps ~ N(0, sigma^2)


## Parameter estimation -----------------------------------------------------------------------
# Assumptions: E(Eps) = 0  and  Var(Eps) = sigma^2 

fm <- lm(y ~ . - Sales,data=data)
summary(fm) 
sqrt(sum(residuals(fm)^2)/fm$df)  # s estimate of sigma
coefficients(fm)

par(mfrow=c(2,2))
plot(fm)
# 8 may be an influential point

shapiro.test(residuals(fm))

# also look at 
vif(fm)

par(mfrow=c(1,1))





x <- model.matrix(y ~ . - Sales, data = data)[,-1] # matrix of predictors
fit.lasso <- glmnet(x,y, lambda = 5, alpha=1) # alpha=1 -> lasso
coef.lasso <- predict(fit.lasso, s=5, type = 'coefficients')
coef.lasso 

# lambda via CV
{
  grid_length = 100
  lambda.grid <- 10^seq(2,0,length=grid_length) # always set decreasing lambda!
  set.seed(1)
  cv.lasso <- cv.glmnet(x,y,alpha=1,nfolds=3,lambda=lambda.grid)
  
  bestlam.lasso <- cv.lasso$lambda.min
  bestlam.lasso
  
  optlam.lasso <- cv.lasso$lambda.1se
  optlam.lasso
  
  plot(cv.lasso)
  abline(v=log(bestlam.lasso), lty=1)
  
  par(mfrow=c(1,1))
  fit.lasso <- glmnet(x,y, lambda = lambda.grid, alpha=1) 
  plot(fit.lasso,xvar='lambda',label=TRUE, col = rainbow(dim(x)[2]))
  abline(v=log(bestlam.lasso))
  abline(v=log(optlam.lasso))
}








