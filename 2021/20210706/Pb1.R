###----------------------###
### Problem 1 (20210706) ###
###----------------------###

library(MVN)

rm(list = ls())
graphics.off()

data <- read.table('chicca.txt', header = T)
head(data)

plot(data)


# a) ----------------------------------------------------------------------

n <- dim(data)[1]
p <- dim(data)[2]

M <- sapply(data, mean)
S <- cov(data)
S.inv <- solve(S)

mvn(data)$multivariateNormality
# p-value = 0.7066245 -> OK

alpha <- 0.01
mu0 <- c(0, 90)

T2 <- n * (M - mu0) %*% S.inv %*% (M - mu0)
T2

cfr.fisher <- ((n - 1) * p / (n - p)) * qf(1 - alpha, p, n - p)
cfr.fisher

T2 < cfr.fisher
# FALSE -> clients are not in mean on time or the mean duration of a stay is not 90 minutes (or both) (or a linear combination of them)


# b) ----------------------------------------------------------------------

P <- 1 - pf(T2 * (n - p) / ((n - 1) * p), p, n - p)
P
# 4.304335e-13


# c) ----------------------------------------------------------------------

k <- p 
cfr.t <- qt(1 - alpha/(2*k), n-1)

BF.I <- cbind(M - cfr.t * sqrt(diag(S)/n),
              M, 
              M + cfr.t * sqrt(diag(S)/n))
dimnames(BF.I)[[2]] <- c('inf', 'center', 'sup')
BF.I
#             inf    center      sup
# delay  3.004227  4.433333  5.86244
# stay  89.491055 91.233333 92.97561

# The stay can be 90 minutes, but clients are in mean at least 3 minutes late


# d) ----------------------------------------------------------------------

data <- data[, 2] - data[, 1]

n <- length(data)
p <- 1

M <- mean(data)
S <- var(data)
S.inv <- solve(S)

shapiro.test(data)
# p-value = 0.6234 -> OK

alpha <- 0.10
mu0 <- 90

T2 <- n * (M - mu0) %*% S.inv %*% (M - mu0)
T2

cfr.fisher <- ((n - 1) * p / (n - p)) * qf(1 - alpha, p, n - p)
cfr.fisher

T2 < cfr.fisher
# FALSE ->  the scheduling policy is not appropriate