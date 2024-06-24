###------------------------------------------------------###
### Problem 3: Employee’s monthly expenditure (20230707) ###
###------------------------------------------------------###

library(car)
library(glmnet)

rm(list = ls())
graphics.off()

data <- read.table('expenditure.txt', header = T)
head(data)

n <- dim(data)[1]


# a) ----------------------------------------------------------------------

m0 <- lm(avg_exp ~ income + age + perc_taxes + owns_house, data = data)
summary(m0)

m0$coefficients
sum((m0$residuals)^2)/m0$df


# b) ----------------------------------------------------------------------

par(mfrow=c(2,2))
plot(m0)
par(mfrow=c(1,1))

shapiro.test(m0$residuals)
# Assumptions are not respected: no Gaussianity on the residuals and heteroschedasticity 

# Residuals vs. Regressors

r <- 4  # number of regressors
par(mfrow=c(2,r/2+r%%2))

for(i in 1:r)
{
  plot(data[, i], m0$residuals, xlab = colnames(data)[i], pch = 19)
  abline(h = 0)
}
# We can clearly see a quadratic shape by plotting the residuals against the regressor "age"
# -> let's introduce a new regressor

age2 <- data$age^2

m1 <- lm(avg_exp ~ income + age + age2 + perc_taxes + owns_house, data = data)
summary(m1)

par(mfrow=c(2,2))
plot(m1)
par(mfrow=c(1,1))

shapiro.test(m1$residuals)
# Now it's a beauty!


# c) ----------------------------------------------------------------------

alpha <- 0.05

CI <- cbind(confint(m1, level = 1-alpha)["owns_house", 1],
            m1$coefficients[6],
            confint(m1, level = 1-alpha)["owns_house", 2])
colnames(CI) <- c("inf", "center", "sup")
CI

# or

linearHypothesis(m1,
                 rbind(c(0,0,0,0,0,1)),
                 c(0))

r <- 5  # number of regressors

CI <- cbind(m1$coefficients[6] - sqrt(vcov(m1)[6, 6])*qt(1-alpha/2, n-(r+1)),
            m1$coefficients[6],
            m1$coefficients[6] + sqrt(vcov(m1)[6, 6])*qt(1-alpha/2, n-(r+1)))
colnames(CI) <- c("inf", "center", "sup")
CI
# People who own a house spend 48.98812 euros more than who rent one


# d) ----------------------------------------------------------------------

vif(m0)
# There's a lot of uncertainty in the estimates of the beta coefficients of 
# "income" and "perc_taxes" (vif ~ 1600)
# -> collinearity

# e) ----------------------------------------------------------------------

set.seed(20230707)

x <- model.matrix(avg_exp ~ income + age + perc_taxes + owns_house, data = data)[, -1]
y <- data$avg_exp

lambda.grid <- 10^seq(1, -2, length = 100)
fit.regularized <- glmnet(x, y, lambda = lambda.grid)

par(mfrow=c(1,1))
plot(fit.regularized, xvar = 'lambda', label = TRUE, col = rainbow(dim(x)[2]))
legend('topright', dimnames(x)[[2]], col = rainbow(dim(x)[2]), lty = 1, cex = 1)

# Let's set lambda via cross validation
cv.regularized <- cv.glmnet(x, y, lambda = lambda.grid)
cv.regularized

bestlam.regularized <- cv.regularized$lambda.min
bestlam.regularized

plot(cv.regularized)
abline(v = log(bestlam.regularized), lty = 1)

fit.best <- glmnet(x, y, lambda = bestlam.regularized)

r <- 4  # number of regressors

coef.best <- predict(fit.best, type = 'coefficients')[1:(r+1), ]
coef.best[which(coef.best != 0)]
