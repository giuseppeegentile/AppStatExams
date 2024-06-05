setwd("C:/Users/Giuseppe/Documents/.magistrale/secondo anno/AppStat/Exams/2020/20200615/20200615")
D <- read.table("pollution.txt",header=T)


library(car)
library(MVN)

n <- dim(D)[1]
p <- dim(D)[2]

# Assumption: normality for the new dataset (not for the old one!)
result <- mvn(D)
result$multivariateNormality



# T2 Hotelling Test H0:  delta.0 = (0,0)
{
  D.mean   <- sapply(D, mean) 
  # if D.mean and delta.0 incompatible -> D.mean   <- colMeans(D)
  D.cov    <- cov(D)
  D.invcov <- solve(D.cov)
  
  alpha   <- .05
  delta.0 <- c(50, 50)
  
  D.T2 <- n * (D.mean - delta.0) %*% D.invcov %*% (D.mean - delta.0)
  D.T2
  cfr.fisher <- ((n - 1) * p / (n - p)) * qf(1 - alpha, p, n - p)
  cfr.fisher
  
  D.T2 < cfr.fisher # FALSE: we reject H0 at level alpha -> the two labs measure differently
  
  P <- 1 - pf(D.T2 * (n - p) / (p * (n - 1)), p, n - p)
  P
  # low -> reject
}


# Confidence region for true mean difference with confidence level 95%
{
  alpha   <- .05
  cfr.fisher <- ((n - 1) * p / (n - p)) * qf(1 - alpha, p, n - p)
  plot(D, asp=1, pch=19, main='Dataset of the Differences')
  ellipse(center=D.mean, shape=D.cov/n, radius=sqrt(cfr.fisher), lwd=2)
  
  points(delta.0[1], delta.0[2], pch=16, col='red', cex=1.5)
  # is outside the ellipse-> reject H0
  
}

# Confidence region characterization
{
  # RR for the mean (ellipsoidal region) 
  # { m \in R^p t.c. n * (m-x_bar)' %*% (x.cov)^-1 %*% (m-x_bar) > cfr.fisher }
  # Center:
  D.mean
  
  # Directions of the principal axes:
  eigen(D.cov/n)$vectors
  
  # Length of the semi-axes of the ellipse:
  r <- sqrt(cfr.fisher)
  r*sqrt(eigen(D.cov/n)$values)
}

# Confidence intervals for linear combination of the components of the mean vector
# Simultaneous T2 intervals in the direction of feature_1 and feature_2
{
  IC.T2.feature_1 <-
    c(D.mean[1] - sqrt(cfr.fisher * D.cov[1, 1] / n),
      D.mean[1],
      D.mean[1] + sqrt(cfr.fisher * D.cov[1, 1] / n))
  
  IC.T2.feature_2  <-
    c(D.mean[2] - sqrt(cfr.fisher * D.cov[2, 2] / n),
      D.mean[2],
      D.mean[2] + sqrt(cfr.fisher * D.cov[2, 2] / n))
  
  T2 <- rbind(IC.T2.feature_1, IC.T2.feature_2)
  dimnames(T2)[[2]] <- c('inf', 'center', 'sup')
  T2
}

