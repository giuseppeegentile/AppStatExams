setwd("C:/Users/Giuseppe/Documents/.magistrale/secondo anno/AppStat/Exams/2021/20210706/20210706")
data <- read.table("chicca.txt")
head(data)


D <- data

n <- dim(D)[1]
p <- dim(D)[2]


result <- mvn(D)
result$multivariateNormality
# we have normality

# T2 Hotelling Test H0:  delta.0 = (0, 90)
{
  D.mean   <- sapply(D, mean) 
  D.cov    <- cov(D)
  D.invcov <- solve(D.cov)
  
  alpha   <- .01
  delta.0 <- c(0, 90)
  
  D.T2 <- n * (D.mean - delta.0) %*% D.invcov %*% (D.mean - delta.0)
  D.T2
  cfr.fisher <- ((n - 1) * p / (n - p)) * qf(1 - alpha, p, n - p)
  cfr.fisher
  
  D.T2 < cfr.fisher # FALSE: we reject H0 at level alpha 
  #Test statistic
  D.T2
  
  P <- 1 - pf(D.T2 * (n - p) / (p * (n - 1)), p, n - p)
  P
  # low -> reject
}
# no, we can't assess that they are on time and stay 90 min 

par(mfrow=c(1,1))
# Rejection region 
{
  cfr.fisher <- ((n - 1) * p / (n - p)) * qf(1 - alpha, p, n - p)
  plot(D, asp=1, pch=19, main='Dataset of the Differences')
  ellipse(center=c(delta.0[1], delta.0[2]), shape=D.cov/n, radius=sqrt(cfr.fisher), lwd=2)
  
  points(D.mean[1], D.mean[2], pch = 16, col ='red', cex = 1.5)
  abline(h=delta.0[1], v=delta.0[2], col='grey35')
  
  
  points(delta.0[1], delta.0[2], pch=16, col='green', cex=1.5)
}
# sample mean (red dot) falls outside rej region (centred in mu0)
#     -> i can reject the true hypothesis at level alpha

# Rej region characterization
{
  # Center:
  delta.0
  
  # Directions of the principal axes:
  eigen(D.cov/n)$vectors
  
  # Length of the semi-axes of the ellipse:
  r <- sqrt(cfr.fisher)
  r*sqrt(eigen(D.cov/n)$values)
}

## Bonferroni intervals 
{
  k <- p  # Number of Bonferroni intervals
  cfr.t <- qt(1-alpha/(2*k), n-1) # Student quantile
  
  # Bonferroni confidence intervals in the direction of DBOD and DSS
  IC.BF.feature_1 <- c(D.mean[1] - cfr.t*sqrt(D.cov[1,1]/n),
                       D.mean[1],
                       D.mean[1] + cfr.t*sqrt(D.cov[1,1]/n))
  IC.BF.feature_2 <- c(D.mean[2] - cfr.t*sqrt(D.cov[2,2]/n),
                       D.mean[2],
                       D.mean[2] + cfr.t*sqrt(D.cov[2,2]/n))
  # Bonferroni region defined by the cartesian product of the Bf intervals
  Bf <- rbind(IC.BF.feature_1, IC.BF.feature_2)
  dimnames(Bf)[[2]] <- c('inf','center','sup')
  Bf
  # we reject because of the delay: the mean 0 is not included
  
  # Plot bonferroni
  {
    par(mfrow=c(1,1))
    plot(D, asp=1, pch=1, main='Dataset of the Differences')
    
    # Adding the 95% confidence region for the true mean of the differences
    ellipse(center=D.mean, shape=D.cov/n, radius=sqrt(cfr.fisher), lwd=2, col='grey',
            center.cex=1.25)
    
    # Adding quadrant lines and delta.0
    abline(h=0, v=0, col='grey', lty=1, lwd=2)
    points(delta.0[1], delta.0[2], pch=16, col='grey35', cex=1.25)
    
    # Bonferroni intervals in the direction of feature_1 and feature_2
    abline(v = Bf[1,1], col='blue', lwd=1, lty=2)
    abline(v = Bf[1,3], col='blue', lwd=1, lty=2)
    abline(h = Bf[2,1], col='blue', lwd=1, lty=2)
    abline(h = Bf[2,3], col='blue', lwd=1, lty=2)
    segments(IC.BF.feature_1[1], 0, IC.BF.feature_1[3], 0, lty=1, lwd=2, col='blue')
    segments(0, IC.BF.feature_2[1], 0, IC.BF.feature_2[3], lty=1, lwd=2, col='blue')
  }  
  
}


# we want the linear combination of the two features to be < 90
# -> stat101 test
d <- data[,1] + data[, 2]
t.test(d,mu=90,alternative="greater")







