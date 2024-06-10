setwd("C:/Users/Giuseppe/Documents/.magistrale/secondo anno/AppStat/Exams/2018/20180710/20180710")
data1 <- read.table("Presales.txt",header=T)
data2 <- read.table("Sales.txt",header=T)
head(data1)
head(data2)

library(MVN)
library(car)
dim(data1)
dim(data2)



# Discount
{
  D <- data.frame(
    D_feature1 = (data1$Flip.Flops - data2$Flip.Flops)/data1$Flip.Flops,
    D_feature2  = (data1$Swimsuit - data2$Swimsuit) / data1$Swimsuit
  ) 
  head(D)
  n <- dim(D)[1]
  p <- dim(D)[2]
  
  
  plot(D, asp=1, pch=19, main='Dataset of Differences')
  abline(h=0.2, v=0.2, col='grey35')
}


# T2 Hotelling Test H0:  delta.0 = (0,0)
{
  D.mean   <- sapply(D, mean) 
  # if D.mean and delta.0 incompatible -> D.mean   <- colMeans(D)
  D.cov    <- cov(D)
  D.invcov <- solve(D.cov)
  
  alpha   <- .01
  delta.0 <- c(0.2, 0.2)
  
  D.T2 <- n * (D.mean - delta.0) %*% D.invcov %*% (D.mean - delta.0)
  D.T2
  cfr.fisher <- ((n - 1) * p / (n - p)) * qf(1 - alpha, p, n - p)
  cfr.fisher
  
  D.T2 < cfr.fisher # TRUE
  
  P <- 1 - pf(D.T2 * (n - p) / (p * (n - 1)), p, n - p)
  P
  # 0.175 -> can't reject
}
# Yes, they have disconted of 20%

# Assumptions:
mvn(D)$multivariateNormality



# estimates
D.mean # 0.1910582  0.2144863
D.cov  
#             D_feature1  D_feature2
# D_feature1 0.013989653 0.004755409
# D_feature2 0.004755409 0.005611812


# Ellipse containing 99% of the units
{
  data <- D
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
  
  plot(data, asp = 1, pch=19, xlim = c(-0.3,.55))
  points(M[1], M[2], pch = 4, cex = 1.5, lwd = 2)
  
  ellipse(center=M, shape=S, radius=sqrt(cfr.chisq), col = 'orange',   center.pch = 4)
}








