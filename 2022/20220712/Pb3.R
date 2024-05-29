setwd("C:/Users/Giuseppe/Documents/.magistrale/secondo anno/AppStat/Exams/2022/20220712/20220712")

library(nlmeU) 
library(nlme) 

library(corrplot)
library(lattice)
library(insight)
library(plot.matrix)
library(MASS)
library(car)
library(rgl)
library(glmnet)

options(rgl.printRglwidget = TRUE)


data <- read.table("rent.txt",header=T)
head(data)
plot(data) #we observe correlatino of price with footage and supermarket 
y <- data$price

n   <- dim(data)[[1]]
fm <- lm(y ~ footage*two.bathrooms + age*two.bathrooms + renovation*two.bathrooms + transport*two.bathrooms
            + center*two.bathrooms + supermarket*two.bathrooms + park*two.bathrooms,
            data=data)
summary(fm)


coefficients(fm)

# Residual standard error: (estimated variance of residual )^/1/2
sqrt(sum(residuals(fm)^2)/fm$df)


# Model diagnostic, Verify assumptions
{
  par(mfrow=c(2,2))
  plot(fm)
 
  
  
  shapiro.test(residuals(fm))
  par(mfrow=c(1,1))
 
}
# Assumptions satisfied

vif(fm)
# -> We have (lot of) collinearity




# Lasso
attach(data)
x <- model.matrix(y ~ footage*two.bathrooms + age*two.bathrooms + renovation*two.bathrooms + transport*two.bathrooms
                  + center*two.bathrooms + supermarket*two.bathrooms + park*two.bathrooms)[,-1] # matrix of predictors

lambda = 45
{
  fit.lasso <- glmnet(x,y, lambda = lambda, alpha=1) # alpha=1 -> lasso
  summary(fit.lasso)
}

coef.lasso <- predict(fit.lasso, s=lambda, type = 'coefficients')
coef.lasso 
coef.lasso[which(abs(coef.lasso)> 0.1)]

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
  
  
  plot(fit.lasso,xvar='lambda',label=TRUE, col = rainbow(dim(x)[2]))
  abline(v=log(bestlam.lasso))
  abline(v=log(optlam.lasso))
}
# We chose lambda = 100, in this way we significantly reduce the number of variables


# Plot L1 norm
{
  norm_l1 <- NULL
  fr <- glmnet(x,y, lambda = lambda.grid, alpha=0) 
  for(i in 1:grid_length)
    norm_l1 <- c(norm_l1,sum(abs(fr$beta[,i])))
  
  plot(log(lambda.grid),norm_l1)
  
}
abline(v=log(bestlam.lasso))
abline(v=log(optlam.lasso))


data.Z0 <- data.frame(footage=30,age=5,renovation=5,transport=300,center=1000,
                       supermarket=500,park=100,two.bathrooms=FALSE, footage.two.bathroomsTRUE = 0,
                       age.two.bathroomsTRUE = 0,
                       renovation.two.bathroomsTRUE = 0,
                       transport.two.bathroomsTRUE = 0,
                       center.two.bathroomsTRUE = 0,
                       supermarket.two.bathroomsTRUE = 0,
                       park.two.bathroomsTRUE = 0)
predict(fit.lasso, as.matrix(data.Z0), s=optlam.lasso, type = 'response')

