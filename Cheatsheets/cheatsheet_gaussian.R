###-----------------------###
### GAUSSIAN DISTRIBUTION ###-------------------------------------------
###-----------------------###

#### Univariate Case ----

##### Probability Density Function (pdf) -----

dnorm(0)                      # density function at 0 for a distribution N(0,1)
dnorm(0, mean = 1, sd = 2)    # density function at 0 for a distribution N(1,4)


##### Cumulative Distribution Function (cdf) -----

pnorm(0)       # P(Z<0), with Z ~ N(0,1)
pnorm(0, 1, 2) # P(X<0), with X ~ N(1,4)


##### Quantiles (inverse of cdf) -----

qnorm(0.95)        #  = z s.t.  P(Z<z)=0.95, with Z ~ N(0,1)
qnorm(0.95, 1, 2)  #  = z s.t.  P(Z<z)=0.95, with Z ~ N(1,4)


##### Random Generation -----

set.seed(8321)
rnorm(10)       # X_1,..,X_10 ~ N(0,1) i.i.d.
rnorm(10, 1, 2) # X_1,..,X_10 ~ N(1,4) i.i.d.


##### Visualization -----

par(mfrow=c(3,1))

s <- seq(-2, 2, by = 0.01)
plot(s, dnorm(s, 0, 1), main = 'Gaussian N(0,1)', type = 'l', ylim = c(0, 1))
plot(s, pnorm(s, 0, 1), main = 'Gaussian N(0,1)', type = 'l', ylim = c(0, 1))

w <- seq(0, 1, by = 0.01)
plot(w, qnorm(w, 0, 1), main = 'Gaussian N(0,1)', type = 'l')

par(mfrow=c(1,2))

set.seed(8321)
z <- rnorm(n = 1000, 0, 1)
plot(z, main = 'Gaussian N(0,1)')
hist(z, main = '', col = 'grey', xlab = 'x', prob = T, ylim = c(0,.45))
lines(seq(-4, 4, length = 100), dnorm(seq(-4, 4, length = 100)), col = 'blue', lty = 2, lwd = 2)
box()


##### QQPlot -----

par(mfrow=c(1,1))

qqplot(qnorm((1:1000/1000-1/2000)), z, xlab = 'Theoretical quantile N(0,1)',
       ylab = 'Sample quantile', asp =1, ylim = c(-5,5)*2, main  ='N(0,1)')
abline(0, 1)
qqline(z, col = 'red')
# If the data are Gaussian, the slope of the qqline is an estimate of 
# the standard deviation, the intercept is an estimate of the mean


##### Tests of Gaussianity -----
# H0: X ~ N     vs    H1=H0^c

shapiro.test(z)


#### Multivariate Case ----

##### Random Generation -----

set.seed(20230320)
X <- rmvnorm(n = 150, mean = c(1, 2), sigma = matrix(c(1, 1, 1, 2), 2)) #!library(mvtnorm)


##### Tests of Gaussianity -----

mvn(X)$multivariateNormality #!library(MVN)
mvn(X)$multivariateNormality$'p value'

mvn(data = X, mvnTest = "royston")$multivariateNormality
# Royston’s test is a multivariate extension of the Shapiro-Wilk test

mvn(X, mvnTest = "hz", multivariatePlot = "qq")$multivariateNormality
# With Q-Q plot of the squared Mahalanobis distance over chi-square


##### Identify Outliers -----

d2 <- matrix(mahalanobis(data, colMeans(data), cov(data)))

hist(d2, prob = TRUE, main = " Histogram of the Mahalanobis Distance", ylab = "density")
lines(0:2000/100, dchisq(0:2000/100, 2), col = 'blue', lty = 2, lwd = 2)
# M. Dist, in presence of normal data, follows a chi-squared distribution (df = #features or #positive_eigenvalues)

threshold <- 12
data_wo_outliers <- data[which(d2 <= threshold), ]

plot(d2)
abline(h = threshold, col = 'grey', lty = 2, lwd = 1)
points(d2, col = ifelse(d2 <= threshold, 'black', 'red'), pch = 19)


##### Box-Cox Transformations -----

box_cox <- function(x, lambda = 0) {
  if (lambda != 0)
    return((x ^ lambda - 1) / lambda)
  return(log(x))
}
# Remember: these transformations can be applied only to positive data
# For lambda<1: observations <1 are "spread", observations >1 are "shrinked"
# For lambda>1: observations <1 are "shrinked", observations >1 are "spread"

x <- seq(0.01, 25, by = 0.01)

hist(x)
hist(box_cox(x))

lambda.x <- powerTransform(x) #!library(car)
bc.x <- bcPower(x, lambda.x$lambda[1]) #!library(car)

hist(bc.x)

exp(log(lambda.x$lambda[1] * bc.x + 1) / lambda.x$data[1]) # Inverse Transformation (lambda != 0)
exp(bc.x) # Inverse Transformation (lambda = 0)

lambda.data <- powerTransform(data)
BC1 <- bcPower(data[, "feature1_name"], lambda.data$lambda[1])
BC2 <- bcPower(data[, "feature2_name"], lambda.data$lambda[2])

mvn(cbind(BC1, BC2))$multivariateNormality
# Note: the higher the dimensionality, the more difficult to recover the normality


##### Tests and Confidence Regions for the Mean -----

### Try It Out! ###
rm(list = ls())
graphics.off()
par(mfrow=c(1,1))
cat("\014")
data <- read.table('datasets/data_manova.txt', header = T)
data <- data[, 1:2]
plot(data)
### Try It Out! ###

n <- dim(data)[1]
p <- dim(data)[2]

M <- sapply(data, mean)
S <- cov(data)
S.inv <- solve(S)

mvn(data)$multivariateNormality

alpha <- 0.10
mu0 <- c(95, 75)

###### Inference Relying on Normality ------

T2 <- n * (M - mu0) %*% S.inv %*% (M - mu0)
T2

cfr.fisher <- ((n - 1) * p / (n - p)) * qf(1 - alpha, p, n - p)
cfr.fisher

T2 < cfr.fisher

P <- 1 - pf(T2 * (n - p) / ((n - 1) * p), p, n - p)
P


###### Inference Relying on Asymptotics ------

T2A <- n * (M - mu0) %*%  S.inv  %*% (M - mu0)
T2A

cfr.chisq <- qchisq(1 - alpha, p)
cfr.chisq

T2A < cfr.chisq

PA <- 1 - pchisq(T2A, p)
PA


##### Confidence Intervals ------

###### Simultaneous T2 Confidence Intervals -------

T2.I <- cbind(M - sqrt(cfr.fisher * diag(S)/n), 
              M, 
              M + sqrt(cfr.fisher * diag(S)/n))
dimnames(T2.I)[[2]] <- c('inf', 'center', 'sup')
T2.I

# or

T2.I.F1 <- c(M[1] - sqrt(cfr.fisher * S[1, 1] / n),
             M[1],
             M[1] + sqrt(cfr.fisher * S[1, 1] / n))

T2.I.F2 <- c(M[2] - sqrt(cfr.fisher * S[2, 2] / n),
             M[2],
             M[2] + sqrt(cfr.fisher * S[2, 2] / n))

T2.I <- rbind(T2.I.F1, T2.I.F2)
dimnames(T2.I)[[2]] <- c('inf', 'center', 'sup')
dimnames(T2.I)[[1]] <- c(dimnames(data)[[2]][1], dimnames(data)[[2]][2])
T2.I

rect(T2.I[1,1], T2.I[2,1], T2.I[1,3], T2.I[2,3], border = 'orange', lwd = 2)

matplot(1:p, 1:p, pch = '', ylim = range(data), xlab = 'Variables', ylab = 'T2 for a component', 
        main =' Simultaneous T2 conf. int. for the components')
for(i in 1:p) segments(i, T2.I[i, 1], i, T2.I[i, 3], lwd = 3, col = i)
points(1:p, T2.I[, 2], pch = 16, col = 1:p)
points(1:p, mu0, lwd = 3, col = 'red')

# Generic Combinations

A <- rbind(c(1,1), # sum of the variables
           c(1,-1)) # difference of the variables

center <- A %*% M
shape <- A %*% S %*% t(A)

T2.I <- cbind(center - sqrt(cfr.fisher * diag(shape)/n),
              center,
              center + sqrt(cfr.fisher * diag(shape)/n))
colnames(T2.I) <- c('inf', 'center', 'sup')
T2.I

# or

a1 <- c(1, 1)
a2 <- c(1, -1)

T2.I.a1 <- c(a1 %*% M - sqrt(cfr.fisher * t(a1) %*% S %*% a1 / n),
             a1 %*% M,
             a1 %*% M + sqrt(cfr.fisher * t(a1) %*% S %*% a1 / n))

T2.I.a2 <- c(a2 %*% M - sqrt(cfr.fisher * t(a2) %*% S %*% a2 / n),
             a2 %*% M,
             a2 %*% M + sqrt(cfr.fisher * t(a2) %*% S %*% a2 / n))

T2.I <- rbind(T2.I.a1, T2.I.a2)
dimnames(T2.I)[[2]] <- c('inf', 'center', 'sup')
T2.I


###### Bonferroni Confidence Intervals -------

k <- p 
cfr.t <- qt(1 - alpha/(2*k), n-1)

BF.I <- cbind(M - cfr.t * sqrt(diag(S)/n),
              M, 
              M + cfr.t * sqrt(diag(S)/n))
dimnames(BF.I)[[2]] <- c('inf', 'center', 'sup')
BF.I

# or

BF.I.F1 <- c(M[1] - cfr.t * sqrt(S[1, 1] / n),
             M[1],
             M[1] + cfr.t * sqrt(S[1, 1] / n))

BF.I.F2 <- c(M[2] - cfr.t * sqrt(S[2, 2] / n),
             M[2],
             M[2] + cfr.t * sqrt(S[2, 2] / n))

BF.I <- rbind(BF.I.F1, BF.I.F2)
dimnames(BF.I)[[2]] <- c('inf', 'center', 'sup')
dimnames(BF.I)[[1]] <- c(dimnames(data)[[2]][1], dimnames(data)[[2]][2])
BF.I

rect(BF.I[1,1], BF.I[2,1], BF.I[1,3], BF.I[2,3], border = 'purple', lwd = 2)

matplot(1:p, 1:p, pch = '', ylim = range(data), xlab = 'Variables', ylab = 'Bonferroni for a component', 
        main =' Bonferroni conf. int. for the components')
for(i in 1:p) segments(i, BF.I[i, 1], i, BF.I[i, 3], lwd = 3, col = i)
points(1:p, BF.I[, 2], pch = 16, col = 1:p)
points(1:p, mu0, lwd = 3, col = 'red')

# Generic Combinations

A <- rbind(c(1,1), # sum of the variables
           c(1,-1)) # difference of the variables

k <- dim(A)[1]
cfr.t <- qt(1 - alpha/(2*k), n-1)

center <- A %*% M
shape <- A %*% S %*% t(A)

BF.I <- cbind(center - cfr.t * sqrt(diag(shape)/n),
              center,
              center + cfr.t * sqrt(diag(shape)/n))
colnames(BF.I) <- c('inf', 'center', 'sup')
BF.I

# or

a1 <- c(1, 1)
a2 <- c(1, -1)

BF.I.a1 <- c(a1 %*% M - cfr.t * sqrt(t(a1) %*% S %*% a1 / n),
             a1 %*% M,
             a1 %*% M + cfr.t * sqrt(t(a1) %*% S %*% a1 / n))

BF.I.a2 <- c(a2 %*% M - cfr.t * sqrt(t(a2) %*% S %*% a2 / n),
             a2 %*% M,
             a2 %*% M + cfr.t * sqrt(t(a2) %*% S %*% a2 / n))

BF.I <- rbind(BF.I.a1, BF.I.a2)
dimnames(BF.I)[[2]] <- c('inf', 'center', 'sup')
BF.I


###### Confidence Intervals for the Variance -------

k <- 1 # eventual Bonferroni correction

Var.I <- cbind((n-1) * diag(S) / qchisq(1 - (alpha)/(2*k), n-1),
               diag(S),
               (n-1) * diag(S) / qchisq(alpha/(2*k), n-1))
dimnames(Var.I)[[2]] <- c('inf', 'center', 'sup')
Var.I


##### Confidence Region ------

# Axes identified by the two original variables

plot(data, asp = 1, pch = 1, 
     xlim = c(min(data[, 1]) - 2, max(data[, 1]) + 2))

abline(h = mu0[2], v = mu0[1], col = 'grey35', lty = 2)
points(mu0[1], mu0[2], col = 'red', pch = 9, cex = 1)

ellipse(center = M, shape = S/n, radius = sqrt(cfr.fisher), lwd = 2, lty = 2, col = 'blue')


# Axes identified by two linear combinations

semiaxes.length <- sqrt(cfr.fisher) * sqrt(eigen(shape/n)$values)

plot(center[1], center[2], 
     xlim = c(center[1] - 1.5 * max(semiaxes.length), center[1] + 1.5 * max(semiaxes.length)), asp = 1, 
     xlab = 'feat1+feat2', ylab = 'feat1-feat2')

ellipse(center = c(center), shape = shape/n, radius = sqrt(cfr.fisher), lty = 2, col = 'blue')


###### Plot Intervals Along Cartesian Axes ------

abline(v = T2.I[1, 1], col = 'orange', lwd = 1, lty = 2)
abline(v = T2.I[1, 3], col = 'orange', lwd = 1, lty = 2)
abline(h = T2.I[2, 1], col = 'orange', lwd = 1, lty = 2)
abline(h = T2.I[2, 3], col = 'orange', lwd = 1, lty = 2)

segments(T2.I[1, 1], mu0[2], T2.I[1, 3], mu0[2], lty = 1, lwd = 2, col = 'orange')
segments(mu0[1], T2.I[2, 1], mu0[1], T2.I[2, 3], lty = 1, lwd = 2, col = 'orange')

abline(v = BF.I[1, 1], col = 'purple', lwd = 1, lty = 2)
abline(v = BF.I[1, 3], col = 'purple', lwd = 1, lty = 2)
abline(h = BF.I[2, 1], col = 'purple', lwd = 1, lty = 2)
abline(h = BF.I[2, 3], col = 'purple', lwd = 1, lty = 2)

segments(BF.I[1, 1], mu0[2], BF.I[1, 3], mu0[2], lty = 1, lwd = 2, col = 'purple')
segments(mu0[1], BF.I[2, 1], mu0[1], BF.I[2, 3], lty = 1, lwd = 2, col = 'purple')

legend('topright', c('Bonf. CI', 'Sim-T2 CI'), col = c('purple', 'orange'), lty = 1, lwd = 2)


###### Plot Intervals Along Worst Direction ------

worst <- S.inv %*% (M - mu0)
worst <- worst / sqrt(sum(worst^2))
worst

T2
n * (t(worst) %*% (M - mu0))^2 / (t(worst) %*% S %*% worst)
(mean(as.matrix(data) %*% worst) - (mu0 %*% worst))^2 / (var(as.matrix(data) %*% worst) / n)

CI.worst <- c(M %*% worst - sqrt(cfr.fisher * (t(worst) %*% S %*% worst) / n),
              M %*% worst,
              M %*% worst + sqrt(cfr.fisher * (t(worst) %*% S %*% worst) / n))
CI.worst
mu0 %*% worst
(CI.worst[1] < mu0 %*% worst) & (mu0 %*% worst < CI.worst[3])   

x.min <- CI.worst[1] * worst # (x,y) coords of the lower bound of the interval
x.max <- CI.worst[3] * worst # (x,y) coords of the upper bound of the interval
m1.ort <- - worst[1] / worst[2] # Slope of the line orthogonal to worst
q.min.ort <- x.min[2] - m1.ort * x.min[1] # Intercept of line of slope m1.ort passing by x.min
q.max.ort <- x.max[2] - m1.ort * x.max[1] # Intercept of line of slope m1.ort passing by x.max
abline(q.min.ort, m1.ort, col = 'forestgreen', lty = 2, lwd = 1)
abline(q.max.ort, m1.ort, col = 'forestgreen', lty = 2, lwd = 1)

m1 = worst[2] / worst[1] # worst direction
q1 <- mu0[2] - m1 * mu0[1]
abline(q1, m1, col = 'grey35')

x.min.plot <- c((q1 - q.min.ort) / (m1.ort - m1), m1 * (q1 - q.min.ort) / (m1.ort - m1) + q1)
x.max.plot <- c((q1 - q.max.ort) / (m1.ort - m1), m1 * (q1 - q.max.ort) / (m1.ort - m1) + q1)

segments(x.min.plot[1], x.min.plot[2], x.max.plot[1], x.max.plot[2], lty = 1, lwd = 2, col = 'forestgreen')


###### Plot Intervals Along Generic Direction ------

dir <- rbind(1, 2)
dir <- dir / sqrt(sum(dir^2))
dir

CI.dir <- c(M %*% dir - sqrt(cfr.fisher * (t(dir) %*% S %*% dir) / n),
            M %*% dir,
            M %*% dir + sqrt(cfr.fisher * (t(dir) %*% S %*% dir) / n))
CI.dir
mu0 %*% dir
(CI.dir[1] < mu0 %*% dir) & (mu0 %*% dir < CI.dir[3])   

x.min <- CI.dir[1] * dir # (x,y) coords of the lower bound of the interval
x.max <- CI.dir[3] * dir # (x,y) coords of the upper bound of the interval
m1.ort <- - dir[1] / dir[2] # Slope of the line orthogonal to dir
q.min.ort <- x.min[2] - m1.ort * x.min[1] # Intercept of line of slope m1.ort passing by x.min
q.max.ort <- x.max[2] - m1.ort * x.max[1] # Intercept of line of slope m1.ort passing by x.max
abline(q.min.ort, m1.ort, col = 'light blue', lty = 2, lwd = 1)
abline(q.max.ort, m1.ort, col = 'light blue', lty = 2, lwd = 1)

m1 = dir[2] / dir[1] # dir direction
q1 <- mu0[2] - m1 * mu0[1]
abline(q1, m1, col = 'grey35')

x.min.plot <- c((q1 - q.min.ort) / (m1.ort - m1), m1 * (q1 - q.min.ort) / (m1.ort - m1) + q1)
x.max.plot <- c((q1 - q.max.ort) / (m1.ort - m1), m1 * (q1 - q.max.ort) / (m1.ort - m1) + q1)

segments(x.min.plot[1], x.min.plot[2], x.max.plot[1], x.max.plot[2], lty = 1, lwd = 2, col = 'light blue')


###### Characterization -------

# Center:
M

# Directions of the principal axes:
eigen(S/n)$vectors
eigen(S/n)$vectors[, 1]
eigen(S/n)$vectors[, 2]

abline(a = M[2] - eigen(S)$vectors[2, 1] / eigen(S)$vectors[1, 1] * M[1], 
       b = eigen(S)$vectors[2, 1] / eigen(S)$vectors[1, 1], 
       lty = 2, col = 'dark red', lwd = 2)
abline(a = M[2] - eigen(S)$vectors[2, 2] / eigen(S)$vectors[1, 2] * M[1], 
       b = eigen(S)$vectors[2, 2] / eigen(S)$vectors[1, 2], 
       lty = 2, col = 'red', lwd = 2)

# Length of the semi-axes of the ellipse:
r <- sqrt(cfr.fisher)
r * sqrt(eigen(S/n)$values)


###### Extra: Elliptical Region for the Entire Population -------

p <- dim(data)[2]
alpha <- 0.01

# First: check that the population is Gaussian
mvn(data)$multivariateNormality

M <- sapply(data, mean)
S <- cov(data)

# Take the radius^2 of the region
cfr.chisq <- qchisq(1 - alpha, p)

# Ellipse Characterization (we need to use asymptotics, LLN)
eigen(S)$vectors # axes directions
M # center
r <- sqrt(cfr.chisq) # radius of the ellipse
r * sqrt(eigen(S)$values) # length of the semi-axes

plot(data, asp = 1, pch = 1, 
     xlim = c(min(data[, 1]) - 5, max(data[, 1]) + 5))

ellipse(center = M, shape = S, radius = sqrt(cfr.chisq), lwd = 2, lty = 2, col = 'blue', center.pch = 3)