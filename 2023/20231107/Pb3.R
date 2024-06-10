###-------------------------------------------------------------###
### Problem 3: Time spent by students on watching TV (20231107) ###
###-------------------------------------------------------------###

rm(list = ls())
graphics.off()

library(MASS)
library(car)
library(rgl)
library(glmnet)

options(rgl.printRglwidget = TRUE)

data <- read.table("students.txt", header = T)
head(data)
dim(data)

n <- dim(data)[1]
p <- dim(data)[2]


# a) -------------------------

m0 <- lm(watchtv ~ gender + age + height + distance + siblings + computertime + exercisehours + musiccds + playgames, data = data)
summary(m0)

rbind("Sigma", sqrt(sum((m0$residuals)^2)/m0$df))
print("Coefficients"); m0$coefficients


# b) ----------------------------------------------------------------------

x <- model.matrix(watchtv ~ gender + age + height + distance + siblings + computertime + exercisehours + musiccds + playgames, data = data)[,-1]
y <- data$watchtv

lambda.lasso <- 0.3
fit.lasso <- glmnet(x, y, lambda = lambda.lasso)

coef.lasso <- predict(fit.lasso, type = 'coefficients')[1:p, ]
coef.lasso[which(coef.lasso != 0)]


# c) ----------------------------------------------------------------------

set.seed(20231108)

lambda.grid <- seq(0.01, 10, by = 0.01)
fit.lasso <- glmnet(x, y, lambda = lambda.grid)  

par(mfrow=c(1,1))
plot(fit.lasso, xvar='lambda',label=TRUE, col = rainbow(dim(x)[2]))
legend('topright', dimnames(x)[[2]], col =  rainbow(dim(x)[2]), lty=1, cex=1)

cv.lasso <- cv.glmnet(x, y, lambda = lambda.grid) 
cv.lasso

bestlam.lasso <- cv.lasso$lambda.min
bestlam.lasso

plot(cv.lasso)

fit.best <- glmnet(x, y, lambda = bestlam.lasso)

mse.min <- mean((y - predict(fit.best, x))^2) # Wrong
mse.min

mse.min <- mean((y - predict(cv.lasso, x, s = bestlam.lasso))^2) # Wrong (same as before)
mse.min

mse.min <- cv.lasso$cvm[cv.lasso$lambda == bestlam.lasso] # Right
mse.min

mse.min <- min(cv.lasso$cvm) # Right (same as before)
mse.min

# The discrepancy arises because the mean squared error (MSE) you are 
# calculating using mean((y - fitted_values)^2) is based on the entire dataset, 
# while the MSE reported in the cv.glmnet output is obtained through cross-validation

coef.best <- predict(fit.best, type = 'coefficients')[1:p, ]
coef.best[which(coef.best != 0)]
