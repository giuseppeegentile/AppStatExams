library(MVN) # Canneed separate installation of "gsl" software library
library(car)
library(mvtnorm)

data1 <- read.table("acoruna.txt", header=TRUE)
data2 <- read.table("pontevedra.txt", header=TRUE)

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

alpha   <- .05
delta.0 <- c(0, 0)
Spinv   <- solve(Sp)

T2 <- n1 * n2 / (n1 + n2) * (data1.mean - data2.mean - delta.0) %*% Spinv %*% (data1.mean - data2.mean - delta.0)

cfr.fisher <- (p * (n1 + n2 - 2) / (n1 + n2 - 1 - p)) * qf(1 - alpha, p, n1 + n2 - 1 - p)
T2 < cfr.fisher # FALSE

P <- 1 - pf(T2 / (p * (n1 + n2 - 2) / (n1 + n2 - 1 - p)), p, n1 + n2 - 1 - p)
P

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
BF.IC
# there is difference in the first taster -> he prefers acoruna dishes

# point c

avg1 <- (data1$T1 + data1$T2)/2
avg2 <- (data2$T1 + data2$T2)/2

# -> one way anova with g=2

m1 <- mean(avg1)
m2 <- mean(avg2)

# verify assumption -> gaussianity and same variance
shapiro.test(avg1)
shapiro.test(avg2)

# same variance
var1 <- var(avg1)
var2 <- var(avg2)
var1/var2

p <- 1
alpha   <- .01
delta.0 <- c(0)
Sp      <- ((n1 - 1) * var1 + (n2 - 1) * var2) / (n1 + n2 - 2)
Spinv <- solve(Sp)

T2 <- n1 * n2 / (n1 + n2) * (m1 - m2 - delta.0) %*% Spinv %*% (m1 - m2 - delta.0)

cfr.fisher <- (p * (n1 + n2 - 2) / (n1 + n2 - 1 - p)) * qf(1 - alpha, p, n1 + n2 - 1 - p)
T2 < cfr.fisher
# no difference

P <- 1 - pf(T2 / (p * (n1 + n2 - 2) / (n1 + n2 - 1 - p)), p, n1 + n2 - 1 - p)
P

# why not do it with anova?
group1 <- rep(1,n1)
group2 <- rep(2,n2)

newdata <- c(avg1,avg2)
groups <- factor(c(group1,group2))

fit <- aov(newdata ~ groups)
summary(fit)
# here it's saying there is no difference also 
