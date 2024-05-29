setwd("C:/Users/Giuseppe/Documents/.magistrale/secondo anno/AppStat/Exams/2022/20220909/20220909")

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


data <- read.table("cameo.txt",header=T)
head(data)

plot(data, pch=19)

n   <- dim(data)[[1]]


# Target variable
y   <- data$price

fm <- lm(y ~ processing + processing:dimension + processing:weight + processing:dimension:weight,data=data)
summary(fm) 
coefficients(fm)

# Residual standard error: (estimated variance of residual )^/1/2
sqrt(sum(residuals(fm)^2)/fm$df)  


## Model diagnostic, Verify assumptions
{
  par(mfrow=c(2,2))
  plot(fm)
 
  
  shapiro.test(residuals(fm))
}


linearHypothesis(fm, rbind(c(1,0,0,0,0,0,0,0),
                           c(0,1,0,0,0,0,0,0), 
                           c(0,0,0,0,1,0,0,0),
                           c(0,0,0,0,0,1,0,0),
                           c(0,0,0,0,0,0,0,1)), c(0,0,0,0,0))
# yes, has significant impact

fm <- lm(y ~ processing + processing:dimension + processing:weight + processing:dimension:weight,data=data)
summary(fm) 
linearHypothesis(fm, rbind(c(0,0,0,0,0,0,1,0),
                           c(0,0,0,0,0,0,0,1)), c(0,0))


# we can remove with any significant confidence the last term, so the interaction between the 3 features
fm <- lm(y ~ processing + processing:dimension + processing:weight,data=data)
summary(fm) 

# the model has a fitting of around 97%

## Prediction for a new point
{
  # Use same names used in fm
  Z0.new <- data.frame(processing="handmade", dimension=10, weight=80)
  
  alpha = 0.05
  # Pred. int. for a new obs
  Pred <- predict(fm, Z0.new, interval='prediction', level=1-alpha)  
  Pred[2]
}
 # Point wise estimate for price: 366.4534 euro
  
  # it is hard to say if is reliable since the range of weights in the dataset for 
  # handmade are under 45. So the prices are all under 250.
  # The new datum is very different from the population on which the model was trained










