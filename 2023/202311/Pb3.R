setwd("C:/Users/Giuseppe/Documents/.magistrale/secondo anno/AppStat/Exams/2023/20231107/20231107")

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

# See how data are spread: if lot of point on low values and few on high 
# -> log transform both target and features
plot(data, pch=19)
# If variables are correlated 
# ->  you'll have collinearity 


n   <- dim(data)[[1]]

# Target variable
y   <- data$watchtv



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
fm <- lm(y ~ gender +age +height +distance +siblings +computertime+ exercisehours+ musiccds +playgames,data=data)
summary(fm) 
# Residual standard error: (estimated variance of residual )^/1/2 estimate of sigma
sqrt(sum(residuals(fm)^2)/fm$df)  
coefficients(fm)


# Lasso 
{
  x <- model.matrix(y ~ gender +age +height +distance +siblings +computertime+ exercisehours+ musiccds +playgames,data=data)[,-1] # matrix of predictors
  
  # Get the coefficients 
  lambda = 0.3
  fit.lasso <- glmnet(x,y, lambda = lambda, alpha=1) # alpha=1 -> lasso
  coef.lasso <- predict(fit.lasso, s=lambda, type = 'coefficients')
  coef.lasso 
  selected =  coef.lasso[which(abs(coef.lasso)> 0.01)]
  selected
  
  
  options(digits = 5)  # Set the number of significant digits to be printed
  print(cv.lasso)
  
  # lambda via CV
  {
    set.seed(20231108)
    grid_length = 100
    lambda.grid <- 10^seq(1, -2, length = grid_length) # always set decreasing lambda!
    #lambda.grid <- seq(10, 0.01, length=grid_length) 
    cv.lasso <- cv.glmnet(x,y,alpha=1,lambda=lambda.grid)
    cv.lasso
    
    min(cv.lasso$cvm)
    bestlam.lasso <- cv.lasso$lambda.min
    bestlam.lasso

    
    plot(cv.lasso)
    abline(v=log(bestlam.lasso), lty=1)
    
    
    fit.lasso <- glmnet(x,y, lambda = lambda.grid, alpha=1) 
    plot(fit.lasso,xvar='lambda',label=TRUE, col = rainbow(dim(x)[2]))
    abline(v=log(bestlam.lasso))
  }
  # If we need prediction, rather use best
  # If we need explainability, use opt (if actually reduce more)
}



