library(MASS)
library(nlmeU)
library(corrplot)
library(nlme)
library(lattice)
library(plot.matrix)
library(lme4)
library(insight)
library(car)
library(rgl)
library(glmnet)

data <- read.table("rent.txt", header=TRUE)

attach(data)
names(data)
two.bathrooms <- factor(two.bathrooms)
# model fitting
lm1 <- lm(price ~ -1 + two.bathrooms+ 
            footage + age + renovation + transport + center + supermarket + park +
            two.bathrooms:footage + two.bathrooms:age + two.bathrooms:renovation +
            two.bathrooms:transport + two.bathrooms:center + two.bathrooms:supermarket + two.bathrooms:park)
summary(lm1)

par(mfrow=c(2,2))
plot(lm1)

# coefficients estimation
coefficients(lm1) # betas
sqrt(sum(lm1$residuals^2)/lm1$df.residual)
summary(lm1) # residual standard error is sigma

shapiro.test(lm1$residuals) # normality assumptions verified


# Build the matrix of predictors
x <- model.matrix(price ~ -1 + two.bathrooms+ 
                    footage + age + renovation + transport + center + supermarket + park +
                    two.bathrooms:footage + two.bathrooms:age + two.bathrooms:renovation +
                    two.bathrooms:transport + two.bathrooms:center + two.bathrooms:supermarket + two.bathrooms:park,
                  data=data)[,-1]
# Build the vector of response
y <- price

lambda <-45
fit.lasso <- glmnet(x,y, lambda = lambda) # default: alpha=1 -> lasso
summary(fit.lasso)
coefficients(fit.lasso)

lambdagrid <-seq(1,1000,1)
cv.lasso <- cv.glmnet(x,y,lambda=lambdagrid) # default: 10-fold CV

par(mfrow=c(1,1))
plot(cv.lasso)

optlam.lasso <- cv.lasso$lambda.1se
optlam.lasso

coef.lasso <- predict(cv.lasso, s=optlam.lasso, type = 'coefficients')
coef.lasso 

x <- data.frame(footage = 30 , age = 5, renovation= 5, transport = 300, center = 1000, 
                supermarket = 500, park = 100, two.bathrooms=as.factor(FALSE))

prediction <- predict(lm1,x,interval="prediction",level=0.95)
prediction

# following the coefficients coef.lasso
xlasso <- c(0,30,5,5,300,1000,500,100,0,0,0,0,0,0,0)

prediction <- predict(cv.lasso, xlasso,s=optlam.lasso) 
prediction
