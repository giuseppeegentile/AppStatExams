library(MASS)
library(car)
library(rgl)
library(glmnet)

set.seed(20231108)
data <- read.table("students.txt", header=TRUE)
names(data)
attach(data)

fit <- lm(watchtv ~ gender + age + height + distance + siblings + computertime + 
          exercisehours + musiccds + playgames)
summary(fit)

coefficients <- fit$coefficients

variance <- (fit$residuals %*% fit$residuals)/(fit$df.residual)

# Build the matrix of predictors
x <- model.matrix(watchtv ~ gender + age + height + distance + siblings + computertime + 
                    exercisehours + musiccds + playgames)[,-1]
# Build the vector of response
y <- watchtv
fit.lasso <- glmnet(x,y, lambda = 0.3)

coefficients <- predict(fit.lasso, s=0.3, type="coefficients")
coefficients
plot(coefficients)
abline(h=0)

# the significant coefficients are the intercept,the age, distance, siblings, computertime,
# musiccds, and playgames

# it does not specify a number of folds, so i assume leave one out cross validation
grid <- seq(0.01,10, length=100) # grid of lambda
grid
n<-dim(data)[1]
n
cv.out <- cv.glmnet(x,y,alpha=1,nfold=n,lambda=grid) 

plot(cv.out)

bestlam.lasso <- cv.out$lambda.min
bestlam.lasso

cv.out # 2.748 MSE

fit.best <- glmnet(x,y, lambda = bestlam.lasso)

cofficients <- coef(fit.best)
coefficients

