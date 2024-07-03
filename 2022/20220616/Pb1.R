###----------------------###
### Problem 1 (20220616) ###
###----------------------###

library(MVN)
library(car)

rm(list = ls())
graphics.off()

data1 <- read.table('discomaniac.txt', header = T)
head(data1)

data2 <- read.table('lipsticks.txt', header = T)
head(data2)


# a) ----------------------------------------------------------------------

D <- data.frame(DF1 = data1$price - data2$price,
                DF2 = data1$media.condition - data2$media.condition) 
D

plot(D)

n <- dim(D)[1]
p <- dim(D)[2]

D.mean <- sapply(D, mean)
D.cov <- cov(D)
D.invcov <- solve(D.cov)

alpha <- 0.05
delta.0 <- c(0, 0)

D.T2 <- n * (D.mean - delta.0) %*% D.invcov %*% (D.mean - delta.0)
D.T2

cfr.fisher <- ((n - 1) * p / (n - p)) * qf(1 - alpha, p, n - p)
cfr.fisher

D.T2 < cfr.fisher
# FALSE -> there is evidence (at 95%) that the mean prices and conditions of the two stores differ

P <- 1 - pf(D.T2 * (n - p) / (p * (n - 1)), p, n - p)
P
# 0.01625523


# b) ----------------------------------------------------------------------

mvn(data = D)$multivariateNormality
# p-value = 0.1881085


# c) ----------------------------------------------------------------------

plot(D, asp = 1, pch = 19)

ellipse(center = D.mean, shape = D.cov/n, radius = sqrt(cfr.fisher), lwd = 2, col = 'blue', center.cex = 1.25)
points(delta.0[1], delta.0[2], col = 'red', pch = 9, cex = 1)
abline(h = delta.0[1], v = delta.0[2], col = 'grey35')


# d) ----------------------------------------------------------------------

k <- 2 * p
cfr.t <- qt(1-alpha/(2*k), n-1)

BF.I.DF1 <- c(D.mean[1] - cfr.t * sqrt(D.cov[1, 1] / n),
              D.mean[1],
              D.mean[1] + cfr.t * sqrt(D.cov[1, 1] / n))

BF.I.DF2 <- c(D.mean[2] - cfr.t * sqrt(D.cov[2, 2] / n),
              D.mean[2],
              D.mean[2] + cfr.t * sqrt(D.cov[2, 2] / n))

BF.V.DF1 <- c((n-1)*D.cov[1, 1] / qchisq(1 - (alpha)/(2*k), n-1),
              D.cov[1, 1],
              (n-1)*D.cov[1, 1] / qchisq(alpha/(2*k), n-1))

BF.V.DF2 <- c((n-1)*D.cov[2, 2] / qchisq(1 - (alpha)/(2*k), n-1),
              D.cov[2, 2],
              (n-1)*D.cov[2, 2] / qchisq(alpha/(2*k), n-1))

BF.I <- rbind(BF.I.DF1, BF.I.DF2, BF.V.DF1, BF.V.DF2)
dimnames(BF.I)[[2]] <- c('inf', 'center', 'sup')
BF.I
#                 inf   center      sup
# BF.I.DF1  0.0480422 0.683000 1.317958
# BF.I.DF2 -0.6597456 0.200500 1.060746
# BF.V.DF1  0.5322847 1.059612 2.841861
# BF.V.DF2  0.9770104 1.944921 5.216247

# -> The difference in price is the responsible for what we've seen in the test