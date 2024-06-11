setwd("C:/Users/Giuseppe/Documents/.magistrale/secondo anno/AppStat/Exams/2017/20170718/20170718")
data <- read.table("castle.txt",header=T)
D <- data





n <- dim(D)[1]
p <- dim(D)[2]

# Assumption: normality for the new dataset (not for the old one!)
result <- mvn(D)
result$multivariateNormality
# high p-val -> normality
# However, even if the pval is not so low, is ok to proceed assuming normality
# Discuss about issues: there must be independence between features, if you assume there isn't, say it


# T2 Hotelling Test H0:  delta.0 = (0,0)
{
  D.mean   <- sapply(D, mean) 
  # if D.mean and delta.0 incompatible -> D.mean   <- colMeans(D)
  D.cov    <- cov(D)
  D.invcov <- solve(D.cov)
  
  alpha   <- .05
  delta.0 <- c(45.733, 7.333)
  
  D.T2 <- n * (D.mean - delta.0) %*% D.invcov %*% (D.mean - delta.0)
  D.T2
  cfr.fisher <- ((n - 1) * p / (n - p)) * qf(1 - alpha, p, n - p)
  cfr.fisher
  
  D.T2 < cfr.fisher # FALSE: we reject H0 at level alpha
  
  P <- 1 - pf(D.T2 * (n - p) / (p * (n - 1)), p, n - p)
  P
  # yes, it is
}


# Ellipse containing 99% of the units
{
  M <- sapply(data, mean)
  S <- cov(data)
  alpha <- 0.05 # 95%
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






