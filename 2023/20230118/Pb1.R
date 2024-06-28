###----------------------###
### Problem 1 (20230118) ###
###----------------------###

library(MVN)

rm(list = ls())
graphics.off()

data1 <- read.table('red.txt', header = T)
data2 <- read.table('white.txt', header = T)

n1 <- dim(data1)[1] 
n2 <- dim(data2)[1]
p  <- dim(data1)[2] 


# a) ----------------------------------------------------------------------

data1.mean <- sapply(data1, mean)
data2.mean <- sapply(data2, mean)
data1.cov <-  cov(data1)
data2.cov <-  cov(data2)
Sp <- ((n1 - 1) * data1.cov + (n2 - 1) * data2.cov) / (n1 + n2 - 2)

list(S1 = data1.cov, S2 = data2.cov, Spooled = Sp)

alpha   <- .05
delta.0 <- c(0, 0, 0)
Spinv   <- solve(Sp)

T2 <- n1 * n2 / (n1 + n2) * (data1.mean - data2.mean - delta.0) %*% Spinv %*% (data1.mean - data2.mean - delta.0)
cfr.fisher <- (p * (n1 + n2 - 2) / (n1 + n2 - 1 - p)) * qf(1 - alpha, p, n1 + n2 - 1 - p)

T2 < cfr.fisher
# The mean of the three variables differ for red and white wines


# b) ----------------------------------------------------------------------

# Rejection region:
# {mu in R^2 | (1/n1 + 1/n2)^-1 * (delta.0 - mu) * (Sp)^-1 * (delta.0 - mu) < F*}
#     where mu = mu1 - mu2
#       and F* = (p*(n1+n2-2) / (n1+n2-1-p)) * F(1-alpha, p, n1+n2-1-p)

delta.0 # center of the rejection region
# alcohol       density       acidity
#       0             0             0

data1.mean - data2.mean # center of the confidence region
#       alcohol       density       acidity 
# -0.1687878788  0.0008324675  2.0677056277

T2
# 40.90507


# c) ----------------------------------------------------------------------

P <- 1 - pf(T2 / (p * (n1 + n2 - 2) / (n1 + n2 - 1 - p)), p, n1 + n2 - 1 - p)
P
# 1.945724e-06


# d) ----------------------------------------------------------------------

mvn(data1)$multivariateNormality
mvn(data2)$multivariateNormality
# Normality assumption respected

list(S1 = data1.cov, S2 = data2.cov)
abs(data1.cov/data2.cov)
# Factor of ~ 41.5 for alcohol-acidity covariance between the two populations
# -> same covariance structure assumption not respected


# e) ----------------------------------------------------------------------

k <- p
n <- n1 + n2
cfr.t <- qt(1 - alpha/(2*k), n-1)

BF.I.F1 <- c(data1.mean[1] - data2.mean[1] - cfr.t * sqrt(Sp[1, 1] * (1 / n1 + 1 / n2)),
             data1.mean[1] - data2.mean[1],
             data1.mean[1] - data2.mean[1] + cfr.t * sqrt(Sp[1, 1] * (1 / n1 + 1 / n2)))
BF.I.F2 <- c(data1.mean[2] - data2.mean[2] - cfr.t * sqrt(Sp[2, 2] * (1 / n1 + 1 / n2)),
             data1.mean[2] - data2.mean[2],
             data1.mean[2] - data2.mean[2] + cfr.t * sqrt(Sp[2, 2] * (1 / n1 + 1 / n2)))
BF.I.F3 <- c(data1.mean[3] - data2.mean[3] - cfr.t * sqrt(Sp[3, 3] * (1 / n1 + 1 / n2)),
             data1.mean[3] - data2.mean[3],
             data1.mean[3] - data2.mean[3] + cfr.t * sqrt(Sp[3, 3] * (1 / n1 + 1 / n2)))

BF.I <- rbind(BF.I.F1, BF.I.F2, BF.I.F3)
dimnames(BF.I)[[2]] <- c('inf', 'center', 'sup')
dimnames(BF.I)[[1]] <- colnames(data1)
BF.I
#                  inf        center        sup
# alcohol -0.848673689 -0.1687878788 0.51109793
# density -0.001103425  0.0008324675 0.00276836
# acidity  1.190589482  2.0677056277 2.94482177
# -> there's a significant difference in the mean for the acidity variable (red wine > white wine)

# or 

BF.I <- cbind(inf = data1.mean - data2.mean - cfr.t * sqrt(diag(Sp) * (1 / n1 + 1 / n2)),
              center = data1.mean - data2.mean,
              sup = data1.mean - data2.mean + cfr.t * sqrt(diag(Sp) * (1 / n1 + 1 / n2)))
BF.I
