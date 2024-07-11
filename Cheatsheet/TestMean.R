library(MVN) # Canneed separate installation of "gsl" software library
library(car)
library(mvtnorm)

data <- read.table("data.txt", header=TRUE)
n <- dim(data)[1]
p <- dim(data)[2]

## PLOTS --------------
# multivariate plot
plot(data)

# univariate plots
par(mfcol = c(2*((p%%4) +1), p))
for (i in 1:p) {
  hist(
    data[, i],
    prob = T,
    main = paste('Histogram of V', i, sep = ''),
    xlab = paste('V', i, sep = '')
  )
  lines(900:2800,
        dnorm(900:2800, mean(data[, i]), sd(data[, i])),
        col = 'blue',
        lty = 2)
  qqnorm(data()[, i], main = paste('QQplot of V', i, sep = ''))
  qqline(data[, i])
  print(shapiro.test(data[, i])$p)
}

## TESTS FOR GAUSSIANITY ---------

# Different multivariate normality tests are implemented but the default one is the Henze-Zirkler's
# this test is not based on a statistics, but on the distance between the sample distribution
# and the real distribution
result <- mvn(data = X)
result$multivariateNormality

# Royston’s test (multivariate extension of the Shapiro-Wilk test)
result <- mvn(data = X, mvnTest = "royston")
result$multivariateNormality

# Univariate tests
for(i in 1:p){
  shapiro.test(data[,i])
}
## TRANSFORMATIONS -----

# Remove outliers
M <- colMeans(data)
S <- cov(data)

d2 <- matrix(mahalanobis(data, M, S))

data_wo_outliers <- data[which(d2 <= 8),]

# Box Cox
# compute optimal lambda
lambda.bc <- powerTransform(data)
lambda.bc

for (i in 1:p){
  bc.data[i] <- bcPower(data[,i], lambda.bc$lambda)
}

## TEST FOR THE MEAN OF A MULTIVARIATE GAUSSIAN POPULATION ----------------------------------
alpha <- .01
mu0 <- c(1, 0)

# T2 Statistics
x.T2       <- n * (x.mean - mu0) %*% x.invcov %*% (x.mean - mu0)
# Radius of the ellipsoid
cfr.fisher <- ((n - 1) * p / (n - p)) * qf(1 - alpha, p, n - p)
# Test:
x.T2 < cfr.fisher   # no statistical evidence to reject H0 at level alpha
# Rejection region: {x.T2 > cfr.fisher}
# (we reject for large values of the T2 statistics)

# Compute the p-value
P <- 1 - pf(x.T2 * (n - p) / ((n - 1) * p), p, n - p)
P

data.mean   <- sapply(data, mean)
data.cov    <- cov(data)
data.invcov <- solve(data.cov)

# plot of the confidence region
plot(data)

# Center:
data.mean
# Directions of the principal axes:
eigen(data.cov/n)$vectors
# Length of the semi-axes of the ellipse:
r <- sqrt(cfr.fisher)
r*sqrt(eigen(data.cov/n)$values)

ellipse(data.mean, data.cov/n, sqrt(cfr.fisher), col = 'red', lty = 2, lwd=2, center.cex=1)

# Simultaneous T2 confidence intervals on the coordinate directions:
# Recall: these are projections of the ellipsoidal confidence region
T2 <- cbind(inf = data.mean - sqrt(cfr.fisher*diag(data.cov)/n),
            center = data.mean, 
            sup = data.mean + sqrt(cfr.fisher*diag(data.cov)/n))
T2

# Bonferroni intervals
k <- p # number of intervals I want to compute (set in advance)
cfr.t <- qt(1-alpha/(2*k), n-1)
Bf <- cbind(inf = data.mean - cfr.t*sqrt(diag(data.cov)/n),
            center = data.mean, 
            sup = data.mean + cfr.t*sqrt(diag(data.cov)/n))
Bf

# Plot of the simultaneous T2 intervals in the direction of X1,...,X4 
matplot(1:p,1:p, pch='',ylim=range(data), xlab='Variables', ylab='T2 for a component', 
        main='Simultaneous T2 conf. int. for the components')
for(i in 1:p) segments(i, T2[i,1], i, T2[i,3], lwd=3, col=i)
points(1:p, T2[,2], pch=16, col=1:p)
# plot the hypithesys mean
points(1:p, mu0, lwd=3, col='orange')

# you can do the above also with Bonferronis

# TEST FOR THE MEAN OF PAIRED MULTIVARIATE GAUSSIAN POPULATIONS ------------------------------
# n stat units, each observed twice, for each unit we observe p variables two times

# dataset of the differences
# complete with the differences you want to examine
D <- data.frame(D1 = data$feature1 - data$feature2,
                D2 = data$feature3 - data$feature4)
D

plot(D)

# Test the Gaussian assumption (on D!)
# you just need gaussianity on you samples !
result <- mvn(data = D)
result$multivariateNormality

plot(D)

n <- dim(D)[1]
p <- dim(D)[2]

D.mean   <- sapply(D, mean)
prova <- colMeans(D) # ok è la stessa cosa
rm(prova)
D.cov    <- cov(D)
D.invcov <- solve(D.cov)

alpha   <- .05
delta.0 <- c(0, 0)

D.T2 <- n * (D.mean - delta.0) %*% D.invcov %*% (D.mean - delta.0)
D.T2

cfr.fisher <- ((n - 1) * p / (n - p)) * qf(1 - alpha, p, n - p)
cfr.fisher

D.T2 < cfr.fisher # FALSE: we reject H0 at level 5% -> there is difference
# there is at least one measurements for which there is a difference

# we compute the p-value
P <- 1 - pf(D.T2 * (n - p) / (p * (n - 1)), p, n - p)
P
# reject H0 at 5% (don't reject at 1%)

# Ellipsoidal confidence region with confidence level 95%
plot(D, asp=1, pch=1, main='Dataset of the Differences', ylim=c(-15,60))
ellipse(center=D.mean, shape=D.cov/n, radius=sqrt(cfr.fisher), lwd=2)

# Adding delta.0 and the quadrants
points(delta.0[1], delta.0[2], pch=16, col='grey35', cex=1.5)
abline(h=delta.0[1], v=delta.0[2], col='grey35')

# Simultaneous CI T2
T2 <- cbind(inf = D.mean - sqrt(cfr.fisher*diag(D.cov)/n),
            center = D.mean, 
            sup = D.mean + sqrt(cfr.fisher*diag(D.cov)/n))
T2

# Bonferroni intervals
k <- p # number of intervals I want to compute (set in advance)
cfr.t <- qt(1-alpha/(2*k), n-1)
Bf <- cbind(inf = D - cfr.t*sqrt(diag(D.cov)/n),
            center = D, 
            sup = D + cfr.t*sqrt(diag(D.cov)/n))
Bf

#or 
k <- p # number of intervals I want to compute (set in advance)
cfr.t <- qt(1-alpha/(2*k), n-1)
Bf1 <- c(inf = D.mean[1] - cfr.t*sqrt(D.cov[1,1]/n),
         center = D.mean[1], 
         sup = D.mean[1] + cfr.t*sqrt(D.cov[1,1]/n))
Bf1
Bf2 <- c(inf = D.mean[2] - cfr.t*sqrt(D.cov[2,2]/n),
         center = D.mean[2], 
         sup = D.mean[2] + cfr.t*sqrt(D.cov[2,2]/n))
Bf2
Bf <- rbind(Bf1,Bf2)
Bf

# variance intervals
Var.I <- cbind((n-1) * diag(D.cov) / qchisq(1 - (alpha)/(2*k), n-1),
               diag(D.cov),
               (n-1) * diag(D.cov) / qchisq(alpha/(2*k), n-1))
dimnames(Var.I)[[2]] <- c('inf', 'center', 'sup')
Var.I

# Ellipsoidal confidence region with confidence level 95%
plot(D, asp=1, pch=1, main='Dataset of the Differences', ylim=c(-15,60))
ellipse(center=D.mean, shape=D.cov/n, radius=sqrt(cfr.fisher), lwd=2)

# Adding delta.0 and the quadrants
points(delta.0[1], delta.0[2], pch=16, col='grey35', cex=1.5)
abline(h=delta.0[1], v=delta.0[2], col='grey35')

# Simultaneous confidence intervals in the direction of DSS and DBOD
abline(v = T2[1,1], col='red', lwd=1, lty=2)
abline(v = T2[1,3], col='red', lwd=1, lty=2)
abline(h = T2[2,1], col='red', lwd=1, lty=2)
abline(h = T2[2,3], col='red', lwd=1, lty=2)

# Bonferroni intervals in the direction of DSS and DBOD
abline(v = Bf[1,1], col='blue', lwd=1, lty=2)
abline(v = Bf[1,3], col='blue', lwd=1, lty=2)
abline(h = Bf[2,1], col='blue', lwd=1, lty=2)
abline(h = Bf[2,3], col='blue', lwd=1, lty=2)


# TEST FOR REPEATED MEASURE -----------------------------------------------------------

data <-read.table("data.txt", header=TRUE)
n <- dim(data)[1]
p <- dim(data)[2]

# pairs plot, seems gaussian?
pairs(data)

# Test for multivariate normality (Henze-Zirkler test by default)
mvn(data)$multivariateNormality

# Plotting each observation as a line
matplot(t(data), type='l', lty = 1)

M <- sapply(data, mean) # sample mean
M
S <- cov(data) # covariance matrix
S

# contrast matrix (p-1)x p, modify it according to the exercise
p
C <- matrix(c(-1, 1, 0, 0,
              -1, 0, 1, 0,
              -1, 0, 0, 1), p-1, p, byrow=T)
C

# Test: H0: C %*% mu == c(0, 0, 0) vs H1: C %*% mu != c(0, 0, 0)
alpha   <- .05
delta.0 <- c(0, 0, 0)

Md <- C %*% M # Sample mean of the "contrasted" observations
Sd <- C %*% S %*% t(C) # Sample covariance of the contrasted observations
Sdinv <- solve(Sd)

# Hotelling T2 statistics
T2 <- n * t(Md - delta.0) %*% Sdinv %*% (Md - delta.0)

# (q-1)*(n-1)/(n-(q-1)) times the 1-alpha Fisher quantile with q-1 and n-q+1 df
cfr.fisher <- ((q - 1) * (n - 1) / (n - (q - 1))) * qf(1 - alpha, (q - 1), n - (q - 1)) 

T2 < cfr.fisher # Testing if we are in the rejection region
T2
cfr.fisher

# p value
P <- 1 - pf(T2 * (n - (q - 1)) / ((q - 1) * (n - 1)), (q - 1), n - (q - 1))
P

# Simultaneous T2 intervals in the direction of the contrasts
IC.T2 <- cbind(Md - sqrt(cfr.fisher*diag(Sd)/n),
               Md,
               Md + sqrt(cfr.fisher*diag(Sd)/n))
IC.T2

# Bonferroni intervals 
k <- p-1   # number of increments (i.e., dim(C)[1])
cfr.t <- qt(1-alpha/(2*k), n-1)

IC.BF <- cbind(Md - cfr.t*sqrt(diag(Sd)/n),
               Md,
               Md + cfr.t*sqrt(diag(Sd)/n))
IC.BF # each row is an interval

plot.bonf.T2 <- function() {
  
  matplot(t(matrix(1:(p-1), p-1, p-1)), t(IC.BF), type='b', pch='', xlim=c(0,p), xlab='',
          ylab='', main='Confidence intervals')
  
  # Plotting the Bonferroni intervals
  segments(matrix(1:3,3, 1), IC.BF[,1], matrix(1:3, 3, 1), IC.BF[,3],
           col='orange', lwd=2)
  points(1:3, IC.BF[,2], col='orange', pch=16)
  
  # Plotting delta.0 under H0 (delta.0 == c(0, 0, 0))
  points(1:3+.05, delta.0, col='black', pch=16)
  
  # Plotting the simultaneous T2
  segments(matrix(1:3+.1,3,1),IC.T2[,1],matrix(1:3+.1,3,1),IC.T2[,3], col='blue', lwd=2)
  points(1:3+.1,IC.T2[,2], col='blue', pch=16)
  
  legend('topright', c('Bonf. IC', 'Sim-T2 IC'), col=c('orange', 'blue'), lty=1, lwd=2)
}
plot.bonf.T2()


# MANOVA with g=2 ---------------------------------------
data1 <- read.table("data1.txt", header=TRUE)
data2 <- read.table("data2.txt", header=TRUE)

n1 <- dim(data1)[1] 
n2 <- dim(data2)[1] 
p  <- dim(data1)[2] 

# Test for multivariate normality
mvn(data1)$multivariateNormality
mvn(data2)$multivariateNormality


data1.mean <- sapply(data1, mean)
data2.mean <- sapply(data2, mean)
data1.cov  <-  cov(data1)
data2.cov  <-  cov(data2)
Sp      <- ((n1 - 1) * data1.cov + (n2 - 1) * data2.cov) / (n1 + n2 - 2)

# We compare the matrices -> here, using rule of thumb:
# we don't reject equality of covariance matrices if s1_ii and s2_ii differ from
# less than a factor ~4 (see J-W p.291)
list(S1 = data1.cov, S2 = data2.cov, Spooled = Sp)
# you can also compute
data1.cov/data2.cov
# and then check if any element of this matrix is > 4


# Test H0: mu1 == mu2  vs  H1: mu1 != mu2
# i.e.,
# Test H0: mu1 - mu2 == c(0, 0)  vs  H1: mu1 - mu2 != c(0, 0)

alpha   <- .01
delta.0 <- c(0, 0)
Spinv   <- solve(Sp)

T2 <- n1 * n2 / (n1 + n2) * (data1.mean - data2.mean - delta.0) %*% Spinv %*% (data1.mean - data2.mean - delta.0)

cfr.fisher <- (p * (n1 + n2 - 2) / (n1 + n2 - 1 - p)) * qf(1 - alpha, p, n1 + n2 - 1 - p)
T2 < cfr.fisher # TRUE: no statistical evidence to reject H0 at level 1%

P <- 1 - pf(T2 / (p * (n1 + n2 - 2) / (n1 + n2 - 1 - p)), p, n1 + n2 - 1 - p)
P
# P-value high (we don't reject at 1%,5%,10%)

# Simultaneous T2 intervals
IC.T2 <- c()
# first is first variable and so on
for(i in 1:p){
  IC.T2.X1 <- c(
    data1.mean[i] - data2.mean[i] - sqrt(cfr.fisher * Sp[i, i] * (1 / n1 + 1 / n2)),
    data1.mean[i] - data2.mean[i] + sqrt(cfr.fisher * Sp[i, i] * (1 / n1 + 1 / n2))
  )
  IC.T2 <- rbind(IC.T2, IC.T2.X1)
}

dimnames(IC.T2)[[1]] <- c('P1', 'P2')
IC.T2

# Bonferroni confidence intervals
k <- p
cfr.t <- qt(1-alpha/(2*k), n1+n2-2)

BF.IC <- c()
# first is first variable and so on
for(i in 1:k){
  BF.IC.X1 <- c(
    data1.mean[i] - data2.mean[i] - sqrt(cfr.t * Sp[i, i] * (1 / n1 + 1 / n2)),
    data1.mean[i] - data2.mean[i] + sqrt(cfr.t * Sp[i, i] * (1 / n1 + 1 / n2))
  )
  BF.IC <- rbind(BF.IC, BF.IC.X1)
}

## LARGE SCALE HYPOTHESIS TESTING -------------------------------------------------

# collect p values into this vector
p.i <-c()

which(p.i<.01) # looking at all the mutations for which i have a p value less than 0.1%
# this answers question considering one at a time

# Bonferroni correction (controlling FWER)
k <- 520
which(p.i*k<.01)
# or
p.Bf <- p.adjust(p.i, method='bonferroni')

which(p.Bf<.01)  

# Benjamini-Hockberg (control the false discovery rate)  
p.BH <- p.adjust(p.i, method='BH')

which(p.BH<.01)


