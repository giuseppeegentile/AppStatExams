# Marco Scarpelli

# Exam 04/09/2023

# Exercise 1

library(MASS)
library(car)
library(rgl)   #3D plots
library(leaps) #best subset selection
library(tree)  #decision trees
library(corrplot) #correlation
library(glmnet)
library(mvnormtest)
library(MVN)
library(heplots)
library(ggplot2)
library(mvtnorm)

rm(list=ls())
graphics.off()

df1 <- read.table('cyclecraft.txt', header=T)
df2 <- read.table('wheelworks.txt', header=T)

head(df1)
head(df2)

n1 <- dim(df1)[1] # n1
n2 <- dim(df2)[1] # n2
p  <- dim(df1)[2]-1 # p=2 (same for t1 and t2)

##########################################
# Point A + B

# We check assumptions on the difference.

df1.mean <- sapply(df1[,2:3], mean)
df2.mean <- sapply(df2[,2:3], mean)

df1.cov  <-  cov(df1[,2:3])
df2.cov  <-  cov(df2[,2:3])

# Plot difference
plot(df1[,2:3]-df2[,2:3])

# Check multivariate normality for the difference
mvn(data = df1[,2:3] - df2[,2:3])$multivariateNormality

# Covariance structure
df1.cov
df2.cov

#                   price condition
# price     124486.903997 -7.539425
# condition     -7.539425  2.387363
# OK
n <- 20
x.mean <- sapply(df1[,2:3]-df2[,2:3], mean)
x.cov <- cov(df1[,2:3]-df2[,2:3])
x.invcov <- solve(x.cov)

alpha<-.05

mu0<-c(0,0)

cfr.fisher <- ((n-1)*p/(n-p))*qf(1-alpha,p,n-p)

#T2 statistic
x.T2 <- n * t(x.mean-mu0) %*% x.invcov %*% (x.mean-mu0) 

# Test
x.T2 < cfr.fisher # Rejection region: {x.T2>cfr.fisher}

# P-value 
P <- 1-pf(x.T2*(n-p)/((n-1)*p), p, n-p)
P # 0.1234051

# The means are equal with a p-value at 12%.

##########################################
# Point C + D

cfr.fisher <- ((n-1)*p/(n-p))*qf(1-alpha,p,n-p)

IC.T2 <- cbind(inf = x.mean - sqrt(cfr.fisher*diag(x.cov)/n),
               center = x.mean, 
               sup = x.mean + sqrt(cfr.fisher*diag(x.cov)/n))
IC.T2

direction.axes<-eigen(x.cov)$vector
radius<-sqrt(cfr.fisher)
length.semi.axes<-radius*sqrt(eigen(x.cov/n)$values)
# Print all together
c(mean=x.mean, semi.axes.length=length.semi.axes,radius=radius) 

# Bonferroni intervals
num_comparisons <- 4
cfr.t <- qt(1-alpha/(2*num_comparisons),n-1)
Bf.IC.mean <- cbind(inf = x.mean - cfr.t*sqrt(diag(x.cov)/n),
                    center = x.mean, 
                    sup = x.mean + cfr.t*sqrt(diag(x.cov)/n))
Bf.IC.mean
IC.T2

# Variance
Bf.IC.var<-cbind(inf=diag(x.cov)*(n-1) / qchisq(1 - alpha/(2*num_comparisons), n-1),
                 center=diag(x.cov),
                 sup=diag(x.cov)*(n-1) / qchisq(alpha/(2*num_comparisons), n-1))
Bf.IC.var


plot(df1[,2:3]-df2[,2:3], asp = 1, main='Confidence region for the mean',xlab=colnames(df1)[1],ylab=colnames(df1)[2]) # Plots the data points; cex is the size of the points
#cov is divided by n in the case of confidence region of the mean!
ellipse(center=x.mean, shape= x.cov/n, radius=sqrt(cfr.fisher), col = 'red', lty = 1, lwd=2, center.cex=1)

points(x.mean[1],x.mean[2], col='red') #sample mean

## The area within the ellipse is the *acceptance region* where if the mean lands, we cannot refute the null hypothesis.
## The area outside the ellipse is the *region of rejection*

# Region of rejection: uguale, ma invece di essere centrata in x.mean, è centrata in mu0, che è il valore ipotizzato per il test
# Rejection region: {x.T2 > cfr.fisher} (optional)
ellipse(center=mu0, shape= x.cov/n, radius=sqrt(cfr.fisher), col = 'blue', lty = 2, lwd=2, center.cex=1)
# Plot hypothesis point to see if it is included
# (optional)
points(mu0[1], mu0[2], col='blue', pch=4, cex=1.5)

# Adding T2 intervals (projections on the axes)
abline(v = IC.T2[1,1], col='red', lwd=1, lty=2)
abline(v = IC.T2[1,3], col='red', lwd=1, lty=2)
abline(h = IC.T2[2,1], col='red', lwd=1, lty=2)
abline(h = IC.T2[2,3], col='red', lwd=1, lty=2)
# Adding Bonferroni intervals 
abline(v = Bf.IC.mean[1,1], col='orange', lwd=1, lty=2)
abline(v = Bf.IC.mean[1,3], col='orange', lwd=1, lty=2)
abline(h = Bf.IC.mean[2,1], col='orange', lwd=1, lty=2)
abline(h = Bf.IC.mean[2,3], col='orange', lwd=1, lty=2)

# Legend
legend('topleft', c('Conf. Reg','T2-sim', 'Bonferroni'),
       col=c('red','red','orange'),lty=c(2,2,2),lwd=c(2,1,1))