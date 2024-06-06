###---------------------------###
### SUPERVISED CLASSIFICATION ###-----------------------------------------------
###---------------------------###

library(MASS)
load('mcshapiro.test.RData')

data <- read.table('data.txt', header=T)

head(data)
#   incidence      tilt norm_abnorm
# 1  38.52069 18.119655          NO
# 2  37.49241  4.393684          NO
# 3  26.88492  5.300174          NO
# 4  46.05444  8.057451          NO
# 5  51.66142 15.125515          NO
# 6  38.54718  2.633603          NO

n <- dim(data)[1]
p <- dim(data)[2]

names(data) # // same as colnames(data)
# [1] "incidence"   "tilt"        "norm_abnorm"

names(data)[length(names(data))] # // if the label is in the last column
# [1] "norm_abnorm"

data$<label_name> = (((as.)))factor(data$<label_name>)
# [73] "NO" "NO" "NO" "NO" "NO" "NO" "NO" "NO" "AB" "AB" "AB" "AB" "AB" "AB" "AB" "AB" "AB" "AB"
# (returns the column vector of the groups)

livelli = levels(as.factor(data$<label_name>))
# [1] "AB" "NO"
# (returns the name of the groups)

factor.name <- factor(data$<label_name>)
# (returns the two above as a unique object: I think we can use this instead of data$<label_name>)

boxplot(data[c(1,2)])
# (visualizes the boxplot of feature 1 and 2)

boxplot(data[,1:2]) 
# (visualizes the boxplot of the features from 1 to 2)

A <- which(data$<label_name> == livelli[1])
# [1]  81  82  83  84  85  86  87  88  89  90  91  92  93  94  95  96  97  98  99 100 101 102 103
# (returns the vector containing the units belonging to group livelli[1])
B <- which(data$<label_name> == livelli[2])

nA <- length(A)
nB <- length(B)
n <- nA + nB

# Prior probabilities (estimated from the data, no prior knowledge)
PA <- nA / n
PB <- nB / n

# Prior probabilities (with prior knowledge)
pa <- 0.35
pb <- 1-0.35
prior <- c(pa,pb)

# Prior probabilities (taking into account misclassification costs of the form c(i|k) = c_k)
c.ba <- 10
c.ab <- 0.05
prior.c <- c(pb*c.ab/(c.ba*pa+c.ab*pb), pa*c.ba/(c.ba*pa+c.ab*pb))

# Jittering
set.seed(1)
data[,1:2] <- data[,1:2] + cbind(rnorm(n, sd=0.025)) # // to add some noise to the data

# 2D plot
plot(data[,1:2], main='data', xlab='feature1', ylab='feautre2', pch=19)
points(data[A,1:2], col='red', pch=19)
points(data[B,1:2], col='blue', pch=19)
legend("topright", legend=levels(data$<label_name>), fill=c('red','blue'))

### LDA (QDA works as the same but doesn't assume equal covariances and doesn't have Fisher scores)
###------------------
# Assumptions:
# 1) if L = i, X.i ~ N(mu.i, sigma.i^2), i = A,B
# 2) sigma.A = sigma.B
# 3) c(A|B) = c(B|A) (equal misclassification costs)

shapiro.test(data[A,1]) # // taking into considerations only 1 feature (the first)
# p-value = 0.4223
# (big p-value -> normality)
shapiro.test(data[B,1])

mcshapiro.test(data[A, 1:2]) # // to test bivariate normality
# $pvalue
# [1] 0.5676
# (big p-value -> normality)
mcshapiro.test(data[B, 1:2])

var.test(data[A,1], data[B,1])$p.value # // to test univariate variances when we have 2 groups
# [1] 8.000733e-06
# (small p-value -> different variances)

bartlett.test(data[A, 1:2], data[B, 1:2])$p.value // # to test covariances equality
  # [1] 6.951498e-06
  # (small p-value -> different covariance matrices)
  
  S1 <- cov(data[A,1:2]) # // to qualitatively compare covariance matrices (also with more than 2 groups!)
#           incidence      tilt
# incidence 314.53723  95.15106
# tilt       95.15106 103.33664
S2 <- cov(data[B,1:2])

par(mfrow=c(1,2)) # // to set the display to 1 row 2 columns
image(S1, col=heat.colors(100),main='Cov. S1', asp=1, axes = FALSE, breaks = quantile(rbind(S1,S2), (0:100)/100, na.rm=TRUE))
image(S2, col=heat.colors(100),main='Cov. S2', asp=1, axes = FALSE, breaks = quantile(rbind(S1,S2), (0:100)/100, na.rm=TRUE))
# (visual comparison of the matrices)

# We can check if there is a significant effect of the groups on the features
fit <- manova(as.matrix(data[,1:2]) ~ data$<label_name>)
summary.manova(fit, test="Wilks")
#                   Df   Wilks approx F num Df den Df    Pr(>F)    
# data$norm_abnorm   1 0.81622   16.549      2    147 3.295e-07 ***
# Residuals        148                                             
# ---
# Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1
# (small Pr(>F) -> yes, there is => it makes sense to perform classification)

lda.data <- lda(data[,1:2], data$<label_name>(((, prior=prior)))) # // inputs: matrix of features + column vector of the labels (+ priors if given)
# ...
# Coefficients of linear discriminants: # // directions of Fisher analysis 
#                    LD1        LD2
# Sepal.Length -2.137114 -0.8197481
# Sepal.Width   2.789529 -2.0844258

# Proportion of trace: # // ratio between each eigenvalue and the trace of the matrix used to obtain this direction
#    LD1    LD2 
# 0.9644 0.0356 
# (performs lda)

# Perform classification on one new datum
new <- data.frame(incidence = 60, tilt = 0) # // also more than one: incidence = seq(40, 80, 1), tilt = seq(-5, 15, 0.5)
answer <- predict(lda.data, new)
# $class
# [1] AB
# Levels: AB NO
# 
# $posterior
#             AB        NO
# [1,] 0.7607823 0.2392177
# (in lda there are also the coordinates of the Fisher scores)

Lda.data <- predict(lda.data, data[,1:2]) # // to retrieve assigned classes

# Compute APER
# !!! Remark: the following is correct only if we estimate the prior with the empirical frequencies!  
#             Otherwise we have to correct the sum taking to account the priors:
# prior <- c(0.35, 0.65)
# G <- 2
# misc <- table(class.true=factor.name, class.assigned=Lda.data$class)
# APER <- 0
# for(g in 1:G)
#   APER <- APER + sum(misc[g,-g])/sum(misc[g,]) * prior[g]  
# ------------------

table(class.true=factor.name, class.assigned=Lda.data$class)
#           class.assigned
# class.true AB NO
#         AB 38 32
#         NO  8 72
# (misclassification table)

errors <- (Lda.data$class != factor.name)
# [1] FALSE FALSE FALSE FALSE FALSE FALSE FALSE FALSE FALSE FALSE  TRUE FALSE FALSE FALSE FALSE
# (returns a vector of 0/1 objects)

APER <- sum(errors)/length(factor.name)
# [1] 0.2666667

# (WITH PRIORS)
APER <- (32*pa+8*pb)/150 (???)

# Compute the estimate of the AER by leave-one-out cross-validation
# ------------------

LdaCV.data <- lda(data[,1:2], data$<label_name>, CV=TRUE) # // new lda
table(class.true=factor.name, class.assignedCV=LdaCV.data$class)
errorsCV <- (LdaCV.data$class != factor.name)

AERCV <- sum(errorsCV)/length(factor.name)

# Taking priors into account...
LdaCV.data <- lda(data[,1:2], data$<label_name>, CV=TRUE, prior = prior)
misc <- table(class.true=data$<label_name>, class.assignedCV=LdaCV.data$class)

AERCV  <- misc[1,2]*pa/sum(misc[1,]) + misc[2,1]*pb/sum(misc[2,])

# Plot the partition induced by LDA (on the previous 2D plot)
# ------------------

points(lda.data$means, col=c('red','blue'), pch=4, lwd=2, cex=1.5)
# (plots the means)

x  <- seq(min(data[,1]), max(data[,1]), length=200)
y  <- seq(min(data[,2]), max(data[,2]), length=200)
xy <- expand.grid(X1=x, X2=y) # replace X1 and X2 with the name of the two features
#     incidence      tilt
# 1    13.06724 -8.482105
# 2    13.50335 -8.482105
# 3    13.93946 -8.482105
# 4    14.37557 -8.482105
# 5    14.81169 -8.482105
# 6    15.24780 -8.482105
# (gives the coordinates of all the generated points)

z  <- predict(lda.data, xy)$post
#                 AB           NO
#     [1,] 0.7103773 2.896227e-01
#     [2,] 0.6979768 3.020232e-01
#     [3,] 0.6856739 3.143261e-01
#     [4,] 0.6735057 3.264943e-01
#     [5,] 0.6615077 3.384923e-01
#     [6,] 0.6497135 3.502865e-01 
# (gives the posterior probabilities of all the generated points) 

z1 <- z[,1] - pmax(z[,2], (((z[,3])))) # // if more than 2 groups...    
z2 <- z[,2] - pmax(z[,1])
# (??? dunno what is this ???)

contour(x, y, matrix(z1, 200), levels=0, drawlabels=F, add=T)  
contour(x, y, matrix(z2, 200), levels=0, drawlabels=F, add=T)  
# (plots boundaries)  


###---------------###
### LINEAR MODELS ###-----------------------------------------------------------
###---------------###

# TYPICAL EXERCISES W/ DUMMIES
# ----------------------------

# Problem 3 of 19/1/22

library(MASS)
library(car)
library(rgl)

options(rgl.printRglwidget = TRUE)

data <- read.table("tattoo.txt", header = T)
head(data)

dummymet = ifelse(data$method == 'handmade', 1, 2)

data = data[,-4]
data = cbind.data.frame(data, dummymet)

fm = lm(price ~ dimension + ncolors + dummymet + dimension:dummymet + ncolors:dummymet, data=data)
summary(fm)
sigma(fm)
coefficients(fm)
vcov(fm)
vif(fm)

par(mfrow=c(2,2))
plot(fm)

shapiro.test(residuals(fm))

linearHypothesis(fm, rbind(c(0,0,0,1,0,0), 
                           c(0,0,0,0,1,0), 
                           c(0,0,0,0,0,1)),
                 c(0,0,0))

linearHypothesis(fm, c(0,0,0,1,0,0), 0)

linearHypothesis(fm, rbind(c(0,0,1,0,0,0), 
                           c(0,0,0,0,0,1)),
                 c(0,0))

linearHypothesis(fm, rbind(c(0,0,0,1,0,0), 
                           c(0,0,0,0,0,1)),
                 c(0,0))

fit2 = lm(price ~ dimension + ncolors + dimension:dummymet, data=data)
summary(fit2)
vcov(fit2)
vif(fit2)

par(mfrow=c(2,2))
plot(fit2)

shapiro.test(residuals(fit2))

hatvalues(fit2)
fitted.values(fit2)

par(mfcol=c(1,2))

plot(fitted.values(fm),residuals(fm), main='Fit1')
abline(h=0)

plot(fitted.values(fit2),residuals(fit2), main='Fit2')
abline(h=0)

data.variable <- data.frame(dimension=6.5,ncolors=1,dummymet=1)
alpha = 0.05
k = 2

predict(fit2,data.variable,interval='confidence',level=1-alpha/k)

data.fixed <- data.frame(dimension=0,ncolors=0,dummymet=0)
alpha = 0.05
k = 2

predict(fit2,data.fixed,interval='confidence',level=1-alpha/k)

# ----------------------------

# Problem 3 of 9/2/22

library(MASS)
library(car)
library(rgl)
load('mcshapiro.test.RData')

data <- read.table('wine.txt', header=TRUE)
head(data)
names(data)

n <- dim(data)[1]
p <- dim(data)[2]

dummyRed=ifelse(data$type=='Red',1,0)
dummyRose=ifelse(data$type=='Rose',1,0)

mod1 <- lm(alcohol ~ sugar+dummyRed+dummyRose+sugar:dummyRed + sugar:dummyRose, data = data)
summary(mod1)

mod1$coefficients
sigma2=sum(mod1$residuals^2)/mod1$df
sigma2

shapiro.test(mod1$residuals) # p-value = 0.7562 gaussiani

par(mfrow = c(2,2))
plot(mod1) 
vif(mod1)

linearHypothesis(mod1, rbind(c(0,0,1,0,0,0),
                             c(0,0,0,1,0,0),
                             c(0,0,0,0,1,0),
                             c(0,0,0,0,0,1)), c(0,0,0,0))   #2.2e-16 *** influisce

linearHypothesis(mod1, rbind(c(0,1,0,0,0,0),
                             c(0,0,0,0,1,0),
                             c(0,0,0,0,0,1)), c(0,0,0))   #2.2e-16 *** influisce

linearHypothesis(mod1, rbind(c(0,0,1,0,0,0),
                             c(0,0,0,1,0,0)), c(0,0)) 

# remove dummyRose and dummyRed

mod2 <- lm(alcohol ~ sugar + sugar:dummyRed + sugar:dummyRose, data = data)
summary(mod2)

mod2$coefficients
sigma2=sum(mod2$residuals^2)/mod2$df
sigma2

shapiro.test(mod2$residuals) # p-value = 0.7223 gaussiani

par(mfrow = c(2,2))
plot(mod2) 
vif(mod2) 

Z0.new = data.frame(sugar=20, dummyRed=1, dummyRose=0)
alpha = 0.01

# PI(Y): prediction
Pred = predict(mod2,Z0.new,interval='prediction',level=1-alpha)
Pred

# LASSO EXERCISE (maybe the model is not that right)
# ----------------------------

data <- read.table("students.txt",header=T)
head(data)

high <- which(data$gender == "male")
g <- rep(0,nrow(data))
g[high] <- 1
fm <- lm(watchtv ~ g:1+age+height+distance+siblings+computertime+exercisehours+musiccds+playgames, data = data)
summary(fm)

coef  =coefficients(fm)
coef

sqrt(sum(residuals(fm)^2)/fm$df)

par(mfrow=c(2,2))
plot(fm)

library(glmnet)

# matrix of predictors
x <- model.matrix(watchtv ~ g:1+age+height+distance+siblings+computertime+exercisehours+musiccds+playgames, data = data)[,-1]

# vector of responses
y <- data$watchtv
lambda = 0.3
fit.lasso <- glmnet(x,y,lambda = lambda, alpha = 1)
coef(fit.lasso) # puoi rimuovere quelli nulli -> variable selection

fm <- lm(watchtv ~ g:1+age+distance+siblings+computertime+musiccds+playgames, data = data)
summary(fm)

lambda.grid <- 10^seq(-2,0,length=100)
# set lambda via CV
cv.lasso <- cv.glmnet(x,y,alpha=1,nfolds=3,lambda = lambda.grid)

bestlam.lasso <- cv.lasso$lambda.min
bestlam.lasso

fit.lasso <- glmnet(x,y,lambda = bestlam.lasso, alpha = 1)
coef(fit.lasso) # puoi rimuovere quelli nulli -> variable selection

fm <- lm(watchtv ~ g:1+distance+siblings+computertime, data = data)
summary(fm)

Z0.new <- data.frame(g=1,age=21,height=76,distance=100,siblings=1,computertime=10,exercisehours=2,musiccsd=35,playgames=4)
alpha = 0.05
# PI(Y): prediction
Pred = predict(fm,Z0.new,interval='prediction',level=1-alpha)
Pred

Z0.new <- data.frame(g=1,distance=100,siblings=1,computertime=10)
alpha = 0.05
# PI(Y): prediction
Pred = predict(fm,Z0.new,interval='prediction',level=1-alpha)
Pred