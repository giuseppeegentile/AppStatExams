###----------------------###
### Problem 3 (20220712) ###
###----------------------###

library(glmnet)

rm(list = ls())
graphics.off()

data <- read.table('rent.txt', header = T)
head(data)

n <- dim(data)[1]


# a) ----------------------------------------------------------------------

m0 <- lm(price ~ two.bathrooms + footage + age + renovation + transport + center + supermarket + park +
                 footage:two.bathrooms + age:two.bathrooms + renovation:two.bathrooms + transport:two.bathrooms + 
                 center:two.bathrooms + supermarket:two.bathrooms + park:two.bathrooms, 
         data = data)
summary(m0)

m0$coefficients
sqrt(sum((m0$residuals)^2)/m0$df)
# 462.8797


# b) ----------------------------------------------------------------------

par(mfrow=c(2,2))
plot(m0)
par(mfrow=c(1,1))

shapiro.test(m0$residuals)
# p-value = 0.6531


# c) ----------------------------------------------------------------------

# Build the matrix of predictors
x <- model.matrix(price ~ two.bathrooms + footage + age + renovation + transport + center + supermarket + park +
                          footage:two.bathrooms + age:two.bathrooms + renovation:two.bathrooms + transport:two.bathrooms + 
                          center:two.bathrooms + supermarket:two.bathrooms + park:two.bathrooms, 
                  data = data)[, -1]
# Build the vector of response
y <- data$price

fit.regularized <- glmnet(x, y, alpha = 1, lambda = 45)

r <- 15

coef <- predict(fit.regularized, type = 'coefficients')[1:(r+1), ]
coef[which(coef != 0)]
#                   (Intercept)             two.bathroomsTRUE                       footage                           age                        center 
#                 2825.42655848                  112.92550012                   22.88245011                   -2.11449587                   -0.08447942 
#                          park     two.bathroomsTRUE:footage         two.bathroomsTRUE:age two.bathroomsTRUE:supermarket 
#                   -0.14994251                    2.59903076                    6.41260461                    0.12367819 


# d) ----------------------------------------------------------------------

set.seed(20220712)

lambda.grid <- 10^seq(2, 0, length = 100)
fit.regularized <- glmnet(x, y, alpha = 1, lambda = lambda.grid)

par(mfrow=c(1,1))
plot(fit.regularized, xvar = 'lambda', label = TRUE, col = rainbow(dim(x)[2]))
legend('topright', dimnames(x)[[2]], col = rainbow(dim(x)[2]), lty = 1, cex = 0.5)

cv.regularized <- cv.glmnet(x, y, alpha = 1, lambda = lambda.grid) # default: 10-fold CV
cv.regularized

bestlam.regularized <- cv.regularized$lambda.min
bestlam.regularized
# 19.63041

plot(cv.regularized)
abline(v = log(bestlam.regularized), lty = 1)

fit.best <- glmnet(x, y, alpha = 1, lambda = bestlam.regularized)

coef.best <- predict(fit.best, type = 'coefficients')[1:(r+1), ]
coef.best[which(coef.best != 0)]
#                   (Intercept)             two.bathroomsTRUE                       footage                           age                     transport 
#                 2894.57265077                   63.01514830                   23.70487952                   -6.54240243                    0.13232943 
#                        center                          park     two.bathroomsTRUE:footage         two.bathroomsTRUE:age      two.bathroomsTRUE:center 
#                   -0.09645096                   -0.20044494                    1.90959474                   11.51353636                    0.01354308 
# two.bathroomsTRUE:supermarket 
#                    0.04061022 


# e) ----------------------------------------------------------------------

new.datum <- data.frame(footage = 30,
                        age = 5,
                        renovation = 5,
                        transport = 300,
                        center = 1000,
                        supermarket = 500,
                        park = 100,
                        two.bathrooms = as.logical("FALSE"))

Pred <- predict(m0, new.datum, interval = 'prediction', level = 1-0.05) 
Pred

Pred[1, "fit"]
# 3532.432
