###----------------------###
### Problem 1 (20220119) ###
###----------------------###

library(MVN)
library(heplots)

rm(list = ls())
graphics.off()

data1 <- read.table('acoruna.txt', header = T)
data2 <- read.table('pontevedra.txt', header = T)
head(data1)
head(data2)


# a) ----------------------------------------------------------------------

n1 <- dim(data1)[1] 
n2 <- dim(data2)[1] 
p  <- dim(data1)[2] 

mvn(data1)$multivariateNormality
mvn(data2)$multivariateNormality
# p-values: 0.151378 and 0.2651369 -> OK

list(S1 = cov(data1), S2 = cov(data2))
abs(cov(data1)/cov(data2))

# Box's M test for homogeneity of covariance matrices
# Should be done only if n_i > ~20 for all i, p < 5 and g < 5
# WARNING: Very sensitive to departure from normality and too restrictive for MANOVA
# especially if we have a high number of samples

target <- rbind(as.matrix(data1), as.matrix(data2))
groups <- factor(rbind(cbind(rep(1, n1)), cbind(rep(2, n2))))

summary(boxM(target, groups))
boxM(target, groups)$p.value
# 0.2765609 -> OK

data1.mean <- sapply(data1, mean)
data2.mean <- sapply(data2, mean)
data1.cov <-  cov(data1)
data2.cov <-  cov(data2)
Sp <- ((n1 - 1) * data1.cov + (n2 - 1) * data2.cov) / (n1 + n2 - 2)

alpha   <- .01
delta.0 <- c(0, 0)
Spinv   <- solve(Sp)

T2 <- n1 * n2 / (n1 + n2) * (data1.mean - data2.mean - delta.0) %*% Spinv %*% (data1.mean - data2.mean - delta.0)
T2

cfr.fisher <- (p * (n1 + n2 - 2) / (n1 + n2 - 1 - p)) * qf(1 - alpha, p, n1 + n2 - 1 - p)
cfr.fisher

T2 < cfr.fisher
# FALSE -> the mean evaluations in the two cities differ


# b) ----------------------------------------------------------------------

k <- p
cfr.t <- qt(1 - alpha/(2*k), n1 + n2 - 2)

BF.I <- cbind(data1.mean - data2.mean - cfr.t * sqrt(diag(Sp) * (1 / n1 + 1 / n2)),
              data1.mean - data2.mean,
              data1.mean - data2.mean + cfr.t * sqrt(diag(Sp) * (1 / n1 + 1 / n2)))
dimnames(BF.I)[[2]] <- c('inf', 'center', 'sup')
BF.I
#           inf    center      sup
# T1  0.2135749 0.9956667 1.777758
# T2 -0.5243082 0.2596667 1.043642

# We can see that the taster 1 definitely gives higher evaluations for dishes of Acoruna


# c) ----------------------------------------------------------------------

data1 <- (data1[, 1] + data1[, 2])/2
data2 <- (data2[, 1] + data2[, 2])/2

n1 <- length(data1) 
n2 <- length(data2)
p <- 1

shapiro.test(data1)
shapiro.test(data2)
# p-values: 0.09915 and 0.659 -> OK

list(S1 = var(data1), S2 = var(data2))
abs(var(data1)/var(data2))
# 0.7873503 (< 4 or 10) -> OK

var.test(data1, data2)
# p-value = 0.5238 -> OK

target <- rbind(as.matrix(data1), as.matrix(data2))
groups <- factor(rbind(cbind(rep(1, n1)), cbind(rep(2, n2))))

bartlett.test(target, groups)
# p-value = 0.5238 -> OK

data1.mean <- mean(data1)
data2.mean <- mean(data2)
data1.cov <-  var(data1)
data2.cov <-  var(data2)
Sp <- ((n1 - 1) * data1.cov + (n2 - 1) * data2.cov) / (n1 + n2 - 2)

alpha   <- .01
delta.0 <- 0
Spinv   <- solve(Sp)

T2 <- n1 * n2 / (n1 + n2) * (data1.mean - data2.mean - delta.0) %*% Spinv %*% (data1.mean - data2.mean - delta.0)
T2

cfr.fisher <- (p * (n1 + n2 - 2) / (n1 + n2 - 1 - p)) * qf(1 - alpha, p, n1 + n2 - 1 - p)
cfr.fisher

T2 < cfr.fisher
# TRUE -> the average evaluations of Acoruna’s octopus dishes are not in mean higher than those of Pontevedra’s octopus dishes