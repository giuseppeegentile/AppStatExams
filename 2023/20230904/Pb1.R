###--------------------------------------------------------###
### Problem 1: Comparing second-hand bike shops (20230904) ###
###--------------------------------------------------------###

rm(list = ls())
graphics.off()

library(car)
library(MVN)

data1 <- read.table('wheelworks.txt', header = T)
data2 <- read.table('cyclecraft.txt', header = T)

head(data1)
head(data2)

data1 <- data1[, -1]
data2 <- data2[, -1]

n <- dim(D)[1]
p <- dim(D)[2]

plot(data1, pch = 19, col = 'red')
points(data2, pch = 19, col = 'blue')


# a) ----------------------------------------------------------------------

D <- data.frame(data1 - data2) 
D

plot(D, asp=1, pch=19, main='Dataset of Differences')
abline(h=0, v=0, col='grey35')
points(0,0, pch=19, col='grey35')

D.mean   <- sapply(D, mean)
D.cov    <- cov(D)
D.invcov <- solve(D.cov)

alpha   <- .05
delta.0 <- c(0, 0)

D.T2 <- n * (D.mean - delta.0) %*% D.invcov %*% (D.mean - delta.0)
D.T2

cfr.fisher <- ((n - 1) * p / (n - p)) * qf(1 - alpha, p, n - p)
cfr.fisher

D.T2 > cfr.fisher 

# FALSE: we don't reject H0 at level 5%

P <- 1 - pf(D.T2 * (n - p) / (p * (n - 1)), p, n - p)
P

# We don't reject H0 even at 10% (even more so at 5%)


# b) ----------------------------------------------------------------------

mvn(D)$multivariateNormality

# Gaussianity verified


# c) ----------------------------------------------------------------------

plot(D, asp=1, pch=1, main='Dataset of the Differences', ylim=c(-4,4))
ellipse(center=D.mean, shape=D.cov/n, radius=sqrt(cfr.fisher), lwd=2)

points(delta.0[1], delta.0[2], pch=16, col='grey35', cex=1.5)
abline(h=delta.0[1], v=delta.0[2], col='grey35')


# d) ----------------------------------------------------------------------

k <- 2 * p
cfr.t <- qt(1-alpha/(2*k), n-1)

BF.M.Pr <- c(D.mean[1] - cfr.t * sqrt(D.cov[1, 1]/n),
             D.mean[1],
             D.mean[1] + cfr.t * sqrt(D.cov[1, 1]/n))
BF.M.Cd <- c(D.mean[2] - cfr.t * sqrt(D.cov[2, 2]/n),
             D.mean[2],
             D.mean[2] + cfr.t * sqrt(D.cov[2, 2]/n))
BF.V.Pr <- c((n-1)*D.cov[1, 1] / qchisq(1 - (alpha)/(2*k), n-1),
             D.cov[1, 1],
             (n-1)*D.cov[1, 1] / qchisq(alpha/(2*k), n-1))
BF.V.Cd <- c((n-1)*D.cov[2, 2] / qchisq(1 - (alpha)/(2*k), n-1),
             D.cov[2, 2],
             (n-1)*D.cov[2, 2] / qchisq(alpha/(2*k), n-1))

BF <- rbind(BF.M.Pr, BF.M.Cd, BF.V.Pr, BF.V.Cd)
dimnames(BF)[[2]] <- c('inf','center','sup')
BF

# We confirm what we've seen in point a)
