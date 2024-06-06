setwd("C:/Users/Giuseppe/Documents/.magistrale/secondo anno/AppStat/Exams/2020/20200717/20200717")
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


data <- read.table("toxicity.txt",header=T)
head(data)

# See how data are spread: if lot of point on low values and few on high 
# -> log transform both target and features
plot(data, pch=19)
# If variables are correlated 
# ->  you'll have collinearity 


n   <- dim(data)[[1]]

# Target variable
y   <- data$tox 


fm <- lm(y ~ C1+C2+C3+C4+C5+C6,data=data)
summary(fm) 
sqrt(sum(residuals(fm)^2)/fm$df)  # s estimate of sigma
coefficients(fm)


par(mfrow=c(2,2))
plot(fm)
shapiro.test(residuals(fm))

z0 <- data.frame(C1=100, C2=0.7,C3=2, C4=4, C5=1.4, C6=3)
alpha = 0.05
# Pred. int. for a new obs
Pred <- predict(fm, z0, interval='prediction', level=1-alpha)  
Pred


# pointwise estimate: 37.40647 
# Inteval for the y_0: [29.932, 44.881]
attach(data)
x <- model.matrix(y ~ C1+C2+C3+C4+C5+C6)[,-1] # matrix of predictors

# lambda via CV
{
  
  grid_length = 100
  lambda.grid <- 10^seq(0,-2,length=grid_length) # always set decreasing lambda!
  set.seed(1)
  cv.lasso <- cv.glmnet(x,y,alpha=1,nfolds=3,lambda=lambda.grid)
  
  bestlam.lasso <- cv.lasso$lambda.min
  bestlam.lasso
  
  optlam.lasso <- cv.lasso$lambda.1se
  optlam.lasso
  
  plot(cv.lasso)
  abline(v=log(bestlam.lasso), lty=1)
  abline(v=log(optlam.lasso), lty=1) # mse is quite high for optlam..maybe is better to use bestlam in this case
  
  par(mfrow=c(1,1))
  your_lambda = bestlam.lasso
  fit.lasso <- glmnet(x,y, lambda = lambda.grid, alpha=1) 
  plot(fit.lasso,xvar='lambda',label=TRUE, col = rainbow(dim(x)[2]))
  abline(h=0)
  abline(v=log(bestlam.lasso))
  abline(v=log(optlam.lasso)) # we don't have a significative removal of coefficients with opt, we use best
}
fit.lasso <- glmnet(x,y, lambda = bestlam.lasso, alpha=1) 
coefficients(fit.lasso)

# Predict new data
{

  predict(fit.lasso, as.matrix(z0), s=bestlam.lasso, type = 'response')
}









