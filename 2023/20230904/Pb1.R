library(car)
library(MVN)

wheelworks <- read.table("wheelworks.txt", header=TRUE)
cyclecraft <- read.table("cyclecraft.txt", header=TRUE)

feat.wheel <- wheelworks[,2:3]
feat.cycle <- cyclecraft[,2:3]


D <- data.frame(feat.wheel - feat.cycle)

# I will compute the confidence region for the mean of the two populations


D.mean <- colMeans(D)
D.cov <- cov(D)
D.invcov <- solve(D.cov)

n <- dim(wheelworks)[1]
p <- dim(feat.wheel)[2]

alpha <- 0.05
delta0 <- c(0,0)

stat<- n * (D.mean - delta0) %*% D.invcov %*% (D.mean - delta0)
stat

cfr.fisher <- ((n - 1) * p / (n - p)) * qf(1 - alpha, p, n - p)
cfr.fisher

stat < cfr.fisher
# TRUE meaning there is no statistical evidence to assume there is a difference between
# the two

P <- 1 - pf(stat * (n - p) / (p * (n - 1)), p, n - p)
P

# This test assume gaussianity for the two population and same covariance
mvn(feat.wheel)$multivariateNormality
mvn(feat.cycle)$multivariateNormality

S1 <- cov(feat.wheel)
S2 <- cov(feat.cycle)
S1
S2
# none of the assumptions are met

mvn(D)$multivariateNormality
# however the matrix of the differences is gaussian

# Plot
plot(D, asp=1, pch=1, main='Dataset of the Differences')
ellipse(center=D.mean, shape=D.cov/n, radius=sqrt(cfr.fisher), lwd=2)
points(delta0[1], delta0[2], pch=16, col='grey35', cex=1.5)

# Bonferroni CI
k <- 4
alpha <- 0.05
S <- cov(D)

qT <- qt(1-(alpha)/(2*k), n-1)

BCI <- cbind(D.mean - qT*sqrt(diag(S)),
         D.mean + qT*sqrt(diag(S)))
BCI


BCI.var <-cbind((n-1)*diag(S)/qchisq(1-(alpha)/(2*k),n-1),
            (n-1)*diag(S)/qchisq((alpha)/(2*k),n-1))
BCI.var

# the bonferroni confidence intervals supports my evidence.
# Even taken separately the mean of the two features is not statiscally significant different.




