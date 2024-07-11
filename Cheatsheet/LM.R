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
library(ISLR)
library(leaps)
library(heplots)

data <- read.table("data.txt", header=TRUE)
head(data)
attach(data)
names(data)
## SIMPLE LINEAR MODELS -----------------------------------------------------------------------
# model fitting
lm1 <- lm( ~ )
summary(lm1)

# Assumptions:
# 1) Parameter estimation: E(Eps) = 0  and  Var(Eps) = sigma^2 
# 2) Inference:            Eps ~ N(0, sigma^2)
# for estimating parameters you don't need the gaussianity assumption
par(mfrow=c(2,2))
plot(lm1)
shapiro.test(lm1$residuals)
par(mfrow=c(1,1))

# plot residuals against variables (to detect pattern, indicating possible transformations to do)
plot(lm1$residuals ~ )


# coefficients estimation
coefficients(lm1) # betas
sqrt(sum(lm1$residuals^2)/lm1$df.residual) # this is sigma not sigma squared
summary(lm1) # residual standard error is sigma

AIC(lm1)
BIC(lm1)

fitted(lm1)        # y hat
residuals(lm1)     # eps hat
coefficients(lm1)  # beta_i
vcov(lm1)          # cov(beta_i)
lm1$rank # order of the model [r+1]
lm1$df   # degrees of freedom of the residuals [n-(r+1)]
hatvalues(lm1) # h_ii (or sometimes called "leverage")
rstandard(lm1) # standardized residuals: eps_j / sqrt(s^2*(1-h_ii))

### Coefficients Test ----------------------------------------------------

summary(lm1)

# does a factor have an effect on the mean value of the response variable?
# verify assumptions of anova
factor <- factor(data$)
response <- data$
ps <-NULL
for(i in 1:length(levels(factor))){
  ps <- c(ps,shapiro.test(response[which(factor==levels(factor)[i])])$p)
}
ps

vars <- NULL
for(i in 1:length(levels(factor))){
  vars <- c(vars, var(response[which(factor==levels(factor)[i],)]))
}
vars

bartlett.test(response, factor) 

fit <- aov(response ~ factor)
summary(fit)

# ncol = nparameters
# nrow tested linear combinations
linearHypothesis(lm1, rbind(c(0,0,0,0),
                            c(0,0,0,0),
                            c(0,0,0,0)),   c(0,0,0) )

p <- 2  # number of tested coefficients
r <- dim(data)[2]-1  # CHANGE IT IF YOU REDUCED number of regressors, -1 still gonna be there

# Plot confidence region for the tested coefficients
i1 <- 1 # must be the smallest of the two
i2 <- 3 # must be the biggest
testcoeff1 <- coefficients(lm1)[i1] #change it
testcoeff2 <- coefficients(lm1)[i2] #change it
covbeta <- vcov(lm1)
testcov <-rbind(cbind(covbeta[i1,i1],covbeta[i1,i2]),cbind(covbeta[i2,i1],covbeta[i2,i2]))

c(tescoeff1, tescoeff2) # point estimates
eigen(testcov)$vectors #direction fof the axes

plot(tescoeff1, tescoeff2, asp=1) # increse or decrease asp is ellips is out
ellipse(cbind(testcoeff1,testcoeff2), testcov, sqrt(p*qf(1-0.05,p,n-(r+1))))
abline(v=0)
abline(h=0)

# Bonferroni on the test
Bf <- rbind(
  beta1=c(testcoeff1 - sqrt(vcov(fm)[i1,i1])*qt(1-0.05/(2*p), n-(r+1)),
          testcoeff1 + sqrt(vcov(fm)[i1,i1])*qt(1-0.05/(2*p), n-(r+1))),
  beta2=c(testcoeff2 - sqrt(vcov(fm)[i2,i2])*qt(1-0.05/(2*p), n-(r+1)),
          testcoeff2 + sqrt(vcov(fm)[i2,i2])*qt(1-0.05/(2*p), n-(r+1)))
)
Bf
# or 
confint(fm, level= 1-0.05/p)# you have to include Bonferroni correction if you
# do a global test, otherwise it's one at a time

### Prediction ---------------------------------------------------------------------
newobs <- data.frame()

# Conf. int. for the mean
Conf <- predict(lm1, newobs, interval='confidence', level=1-0.05)  
Conf
# Pred. int. for a new obs
Pred <- predict(lm1, newobs, interval='prediction', level=1-0.05)  
Pred

## COLLINEARITY----------------------------------------------------------------------
# detect collinearity

vif(lm1) # Rule of thumb -> problem when VIF exceeds 10 (or 5 sometimes)

### PCA regression -----------------------------------------------------------------------------
head(data)
target <- data$
regressors <- data[,]
r <- dim(regressors)[2]

pca <- princomp(cbind(speed1,speed2), scores=TRUE)
# if the regressors have different scales it's not a big problem since we are only
# interested in finding two orthogonal directions

# do it for each regressor
scores1 <- pca$scores[,1]
scores2 <- pca$scores[,2]

lm.pca <- lm(distance ~ scores1 + scores2)

summary(lm.pca) 
# Note: we could have performed dimensionality reduction before
# estimating the model and then considered only the first PC.
# but be careful in doing so, you may discard some useful regressors

# We can re-write the model as:
# Model:
# y= b0 + b1*PC1 + b2*PC2 + eps =
#  = b0 + b1*(e11*(reg1-m1)+e21*(reg2-m2)+e31*(reg3-m3)+e41*(regr4-m4)) +
#       + b2*(e12*(reg1-m1)+e22*(reg2-m2)+e32*(reg3-m3)+e42*(reg4-m4)) + eps =
#  = b0 - b1*e11*m1 - b2*e12*m1 - b1*e21*m2 - b2*e22*m2 +
#       - b1*e31*m3 - b2*e32*m3 - b1*e41*m4 - b2*e42*m4 +
#       + (b1*e11+b2*e12)*Alluminium + (b1*e21+b2*e22)*Silicate +
#       + (b1*e31+b2*e32)*Alluminium_ferrite + (b1*e41+b2*e42)*Silicate_bicalcium + eps
# where e.ij are the loadings, i=1,2,3,4, j=1,2.
# => We can compute the coefficients of the model which used the original
#    regressors
m1 <- mean(reg1)
m2 <- mean(reg2)
m3 <- mean(reg3)
m4 <- mean(reg4)
beta0 <- coefficients(lm.pca)[1] -
  coefficients(lm.pca)[2]*pca$load[1,1]*m1 -
  coefficients(lm.pca)[3]*pca$load[1,2]*m1 -
  coefficients(lm.pca)[2]*pca$load[2,1]*m2 -
  coefficients(lm.pca)[3]*pca$load[2,2]*m2 -
  coefficients(lm.pca)[2]*pca$load[3,1]*m3 -
  coefficients(lm.pca)[3]*pca$load[3,2]*m3 - 
  coefficients(lm.pca)[2]*pca$load[4,1]*m4 -
  coefficients(lm.pca)[3]*pca$load[4,2]*m4
beta1 <- coefficients(lm.pca)[2]*pca$load[1,1] +
  coefficients(lm.pca)[3]*pca$load[1,2]
beta2 <- coefficients(lm.pca)[2]*pca$load[2,1] +
  coefficients(lm.pca)[3]*pca$load[2,2]
beta3 <- coefficients(lm.pca)[2]*pca$load[3,1] +
  coefficients(lm.pca)[3]*pca$load[3,2]
beta4 <- coefficients(lm.pca)[2]*pca$load[4,1] +
  coefficients(lm.pca)[3]*pca$load[4,2]

c(beta0=as.numeric(beta0),beta1=as.numeric(beta1),beta2=as.numeric(beta2),beta3=as.numeric(beta3),beta4=as.numeric(beta4))

# diagnostics of the residuals
par(mfrow=c(2,2))
plot(lm.pca)

shapiro.test(residuals(lm.pca))


### Ridge regression ---------------------------------------------------------------------------
lambda <- .5
fit.ridge <- lm.ridge(target ~ , lambda = lambda)
# Note: to fit the model, R automatically centers X and y 
# with respect to their mean.

coef.ridge <- coef(fit.ridge)
fm$coefficients

yhat.r <- cbind(rep(1,n), regressor1, regressor2,)%*%coef.ridge # ridge fitted values

# Choice of the optimal lambda, e.g., via cross-validation
# GCV -> Generalized Cross Validation
lambda.c <- seq(0,10,0.01)
fit.ridge <- lm.ridge(target ~  + , lambda = lambda.c)
# also with glmnet
glm.ridge <- glmnet(x,y, lambda = lambda.grid, alpha=0) # alpha=0 -> ridge

plot(glm.ridge,xvar='lambda',label=TRUE, col = rainbow(dim(x)[2]))
legend('topright', dimnames(x)[[2]], col =  rainbow(dim(x)[2]), lty=1, cex=1)

select(fit.ridge)

lambda.opt <- lambda.c[which.min(fit.ridge$GCV)]
lambda.opt

coef.ridge <- coef(fit.ridge)[which.min(fit.ridge$GCV),]
coef.ridge

# set lambda via CV
set.seed(1)
cv.ridge <- cv.glmnet(x,y,alpha=0,nfolds=3,lambda=lambda.grid)

bestlam.ridge <- cv.ridge$lambda.min
bestlam.ridge

optlam.ridge <- cv.ridge$lambda.1se
optlam.ridge

plot(cv.ridge)
abline(v=log(bestlam.ridge), lty=1)
abline(v=log(optlam.ridge), lty=1)
# coefficients for optimal lambda
coef.ridge <- predict(fit.ridge, s=bestlam.ridge, type = 'coefficients')[1:5,]
coef.ridge 

# l2 norm
sqrt(sum((coef(lm(y~x))[-1])^2)) # LS
sqrt(sum((coef.ridge[-1])^2))    # ridge

### Lasso regression (also variable selection)---------------------------------------------- 
# Build the matrix of predictors
x <- model.matrix(target ~ ,
                  data=data)[,-1]
# Build the vector of response
y <- target

# simple lasso
lambda <-45
fit.lasso <- glmnet(x,y, lambda = lambda) # default: alpha=1 -> lasso
summary(fit.lasso)
coefficients(fit.lasso)

# choose lambda with CV
lambdagrid <-seq(1,1000,1)
cv.lasso <- cv.glmnet(x,y,lambda=lambdagrid) # default: 10-fold CV

par(mfrow=c(1,1))
plot(cv.lasso)

bestlam.lasso <- cv.lasso$lambda.min
bestlam.lasso

optlam.lasso <- cv.lasso$lambda.1se
optlam.lasso

# coefficients for optimal lambda ( you can do it also for best lambda)
coef.lasso <- predict(cv.lasso, s=optlam.lasso, type = 'coefficients')
coef.lasso 

# l1 norm
sum(abs(coef(lm(y~x))[-1])) # LS
sum(abs(coef.lasso[-1]))    # lasso

# MODEL/VARIABLE SELECTION --------------------------------------------------------------------

### Best Subset Selection: Exhaustive Search-------------------------------------------------
# computationally expensive
regfit.full <- regsubsets(target~., data=data)
summary(regfit.full)
# In the table:
# the i-th row represent a subset of i variable and tells you what are the best variables to put in
# a subset of that dimension.

regfit.full <- regsubsets(Salary~., data=Hitters, nvmax=19)
# nvmax specificies the max amount of variables in my subset

reg.summary <- summary(regfit.full)

reg.summary$rsq   # r-squared
reg.summary$adjr2 # adjusted r-squared
reg.summary$rss   # residual sum of squares

which.max(reg.summary$adjr2)
coef(regfit.full,which.max(reg.summary$adjr2))

### Forward and Backward Stepwise Selection----------------------------------------------
regfit.fwd <- regsubsets(target~., data=data, nvmax=19, method="forward")
regfit.bwd <- regsubsets(target~.,data=data, nvmax=19, method="backward")
summary(regfit.bwd)
# you add the variable one at a time. you keep the first and you choose the best second. you
# keep the first two and you choose the third and so on.
# Less expensive than before
summary(regfit.fwd)

# Plot of metrics vs number of variables
par(mfrow=c(1,3))
plot(summary(regfit.fwd)$rsq,xlab="Number of Variables",ylab="R-squared",type="b")
plot(summary(regfit.fwd)$adjr2,xlab="Number of Variables",ylab="Adjusted RSq",type="b")
plot(summary(regfit.fwd)$rss,xlab="Number of Variables",ylab="RSS",type="b")
par(mfrow=c(1,1))

### Choosing among models using the k-fold cross-validation approach------------
# (exhaustive search)
k <- 10
maxvar <- 19
target <-

set.seed(1)
folds <- sample(1:k, nrow(data), replace=TRUE)
folds
table(folds)

# function that performs the prediction for regsubsets
predict.regsubsets <- function(object, newdata, id) {
  form <- as.formula(object$call[[2]])
  mat <- model.matrix(form, newdata)
  coefi <- coef(object, id=id)
  xvars <- names(coefi)
  mat[,xvars]%*%coefi
}

cv.errors <- matrix(NA, k, maxvar, dimnames=list(NULL, paste(1:maxvar)))

for(j in 1:k) {
  best.fit <- regsubsets(target~., data=data[folds!=j,], nvmax=maxvar)
  for(i in 1:maxvar) {
    pred <- predict.regsubsets(best.fit, data[folds==j,], id=i)
    cv.errors[j,i] <- mean( (target[folds==j]-pred)^2 )
  }
}

cv.errors
root.mean.cv.errors <- sqrt(apply(cv.errors,2,mean)) # average over the columns
root.mean.cv.errors

plot(root.mean.cv.errors,type='b')

which.min(root.mean.cv.errors)
points(which.min(root.mean.cv.errors),root.mean.cv.errors[which.min(root.mean.cv.errors)], col='red',pch=19)

# estimation on the full dataset
reg.best <- regsubsets(target~., data=data, nvmax=19)
coef(reg.best,which.min(root.mean.cv.errors)) # coefficients of the best model


