setwd("C:/Users/Giuseppe/Documents/.magistrale/secondo anno/AppStat/Exams/2019/20190719/20190719")
library(car)
library(MVN)

# CASE 1: Test for the mean of paired multivariate Gaussian observations
# Multiple features measured by two laboratories
# -> you want to prove if they measure not differently


data <- read.table('pines.txt', header = T)

head(data)
dim(data)

D <- data
n <- dim(D)[1]
p <- dim(D)[2]

result <- mvn(D)
result$multivariateNormality


# T2 Hotelling Test H0:  delta.0 = (0,0)
{
  D.mean   <- sapply(D, mean) 
  # if D.mean and delta.0 incompatible -> D.mean   <- colMeans(D)
  D.cov    <- cov(D)
  D.invcov <- solve(D.cov)
  
  alpha   <- .01
  delta.0 <- c(14.235, 42.425)
  
  D.T2 <- n * (D.mean - delta.0) %*% D.invcov %*% (D.mean - delta.0)
  D.T2
  cfr.fisher <- ((n - 1) * p / (n - p)) * qf(1 - alpha, p, n - p)
  cfr.fisher
  
  D.T2 < cfr.fisher # FALSE: we reject H0 at level alpha -> the two labs measure differently
  
  P <- 1 - pf(D.T2 * (n - p) / (p * (n - 1)), p, n - p)
  P
  # low -> reject
}



# Ellipse containing 99% of the units
{
  M <- sapply(data, mean)
  S <- cov(data)
  alpha <- 0.01 # 99%
  cfr.chisq <- qchisq(1-alpha,p)
  
  # Axes directions:
  eigen(S)$vectors
  # Center:
  M
  # Radius of the ellipse:
  r <- sqrt(cfr.chisq)
  # Length of the semi-axes:
  r*sqrt(eigen(S)$values)  
  
  plot(data, asp = 1, pch=19)
  points(M[1], M[2], pch = 4, cex = 1.5, lwd = 2)
  
  ellipse(center=M, shape=S, radius=sqrt(cfr.chisq), col = 'orange',   center.pch = 4)
}











