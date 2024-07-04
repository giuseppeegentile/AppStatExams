library(MVN) # Canneed separate installation of "gsl" software library
library(car)
library(mvtnorm)

data1 <- read.table("discomaniac.txt",header=TRUE)
data2 <- read.table("lipsticks.txt", header=TRUE)
head(data1)
head(data2)
# n stat units, each observed twice, for each unit we observe p variables two times

# dataset of the differences
# complete with the differences you want to examine
D <- data.frame(Dprice = data1$price - data2$price,
                Dcondition = data1$media.condition - data2$media.condition)
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
plot(D, asp=1, pch=1, main='Dataset of the Differences')
ellipse(center=D.mean, shape=D.cov/n, radius=sqrt(cfr.fisher), lwd=2)

# Adding delta.0 and the quadrants
points(delta.0[1], delta.0[2], pch=16, col='grey35', cex=1.5)
abline(h=delta.0[1], v=delta.0[2], col='grey35')


# Bonferroni intervals
k <- 4 # number of intervals I want to compute (set in advance)
cfr.t <- qt(1-alpha/(2*k), n-1)
Bf1 <- c(inf = D.mean[1] - cfr.t*sqrt(diag(D.cov[1,1])/n),
            center = D.mean[1], 
            sup = D.mean[1] + cfr.t*sqrt(diag(D.cov[1,1])/n))
Bf1
Bf2 <- c(inf = D.mean[2] - cfr.t*sqrt(diag(D.cov[2,2])/n),
             center = D.mean[2], 
             sup = D.mean[2] + cfr.t*sqrt(diag(D.cov[2,2])/n))
Bf2
Bf <- rbind(Bf1,Bf2)
Bf

# variance intervals
Var.I <- cbind((n-1) * diag(D.cov) / qchisq(1 - (alpha)/(2*k), n-1),
               diag(D.cov),
               (n-1) * diag(D.cov) / qchisq(alpha/(2*k), n-1))
dimnames(Var.I)[[2]] <- c('inf', 'center', 'sup')
Var.I



