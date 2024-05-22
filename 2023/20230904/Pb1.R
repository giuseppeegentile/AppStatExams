setwd("C:/Users/Giuseppe/Documents/.magistrale/secondo anno/AppStat/Exams/2023/20230904/20230904")
library(car)
library(MVN)


data1 <- read.table('wheelworks.txt', header = T)
data2 <- read.table('cyclecraft.txt', header = T)

head(data1)
dim(data1)
head(data2)
dim(data2)

D <- data.frame(
  D_feature1 = data1$price  - data2$price,
  D_feature2 = data1$condition - data2$condition
) 
head(D)
n <- dim(D)[1]
p <- dim(D)[2]
plot(D,pch=19)
abline(h=0,v=0)

# cyclecraft tends to have higher prices and higher condition
# prove it statistically:
D.mean   <- sapply(D, mean) 
D.cov    <- cov(D)
D.invcov <- solve(D.cov)

alpha   <- .05 /2 # one sided test
delta.0 <- c(0, 0)

D.T2 <- n * (D.mean - delta.0) %*% D.invcov %*% (D.mean - delta.0)
D.T2
cfr.fisher <- ((n - 1) * p / (n - p)) * qf(1 - alpha, p, n - p)
cfr.fisher

D.T2 < cfr.fisher # True: we can't reject H0 at level alpha 

P <- 1 - pf(D.T2 * (n - p) / (p * (n - 1)), p, n - p)
P
# No, there isn't enough evidence to support the statement



# b)
# Assumption: normality for the difference
result <- mvn(D)
result$multivariateNormality # normality satisfied

# c)
# Confidence region for true mean difference with confidence level 95%
{
  alpha   <- .05
  cfr.fisher <- ((n - 1) * p / (n - p)) * qf(1 - alpha, p, n - p)
  plot(D, asp=1, pch=1, main='Dataset of the Differences')
  ellipse(center=D.mean, shape=D.cov/n, radius=sqrt(cfr.fisher), lwd=2)
  
  points(delta.0[1], delta.0[2], pch=16, col='grey35', cex=1.5)
  abline(h=delta.0[1], v=delta.0[2], col='grey35')
  # inside the ellipse-> can't reject H0
}

## Bonferroni intervals 
{
  k <- 2*p  # Number of Bonferroni intervals (2*p because also variance asked)
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
  ICvar <- cbind(inf=diag(D.cov)*(n-1) / qchisq(1 - alpha/(2*k), n-1),
                 center=diag(D.cov),
                 sup=diag(D.cov)*(n-1) / qchisq(alpha/(2*k), n-1))
  
  ICvar
  # 0 contained in both intervals
  # we can't reject even with bonferroni correction
  # The variance is very high for both the feature, this is due to the low dimension
  # of the dataset (only 20 entries)
  
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




















