library(MASS)
library(glmnet)
set.seed(20230707)

data <- read.table("expenditure.txt", header =TRUE)
head(data)

attach(data)
names(data)

fit <- lm(avg_exp ~ income + age +  perc_taxes + owns_house )
summary(fit)

coef(fit)
sqrt((fit$residuals%*%fit$residuals)/fit$df.residual)

par(mfrow=c(2,2))
plot(fit)

par(mfrow=c(2,2))
plot(fit$residuals ~ income)
plot(fit$residuals ~ age)
plot(fit$residuals ~ perc_taxes)
plot(fit$residuals ~ owns_house)

# i would introduce the square of the age as a variable

age2 <- age^2
fit2 <- lm(avg_exp ~ income +age+ age2 +  perc_taxes + owns_house )
summary(fit2)
par(mfrow=c(2,2))
plot(fit2)

plot(fit2$residuals ~ age2 )

# confidence interval for the mean of those who owns and thos who rent.
# the average difference in avg_exp is the value of the coefficient for the dummy variable
# owns_house -> -48,98811. Meaning those who rent spend more.

confint(fit2)
# we have strong evidence of collinearity in income and perc_taxes
vif(fit)
# this is a good reason to perform a penalized regression.

# fit with lasso
lambda = 10^seq(10,-2,length.out=100)

# Build the matrix of predictors
x <- model.matrix(avg_exp ~ income + age +  perc_taxes + owns_house )[,-1]
# Build the vector of response
y <- avg_exp


cv.lasso <- cv.glmnet(x,y,lambda=lambda) # default: 10-fold CV

bestlam.lasso <- cv.lasso$lambda.min
bestlam.lasso

optlam.lasso <- cv.lasso$lambda.1se
optlam.lasso

par(mfrow=c(1,1))
plot(cv.lasso)
abline(v=log(bestlam.lasso), lty=1)

# i would choose the lambda.1se since it corresponds to less complexity and almost the same 
# mean squared error


