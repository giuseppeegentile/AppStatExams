setwd("C:/Users/Giuseppe/Documents/.magistrale/secondo anno/AppStat/Exams/2019/20190628/20190628")
library(car)
library(MVN)

# CASE 1: Test for the mean of paired multivariate Gaussian observations
# Multiple features measured by two laboratories
# -> you want to prove if they measure not differently


data1 <- read.table('girona.txt', header = T)
data2 <- read.table('terrassa.txt', header = T)
head(data1)
dim(data1)
head(data2)
dim(data2)

# Paired gaussian: tasters taste in two different cities
# Depends on what he asks:
# 1:  Compute the differences FEATURE WISE (can be more than 2 features pairs!)
{
  D <- data.frame(
    D_feature1 = data1$T1 - data2$T1,
    D_feature2  = data1$T2  - data2$T2
  ) 
  head(D)
  n <- dim(D)[1]
  p <- dim(D)[2]
  
  
  plot(D, asp=1, pch=19, main='Dataset of Differences')
  abline(h=0, v=0, col='grey35')
}

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
  delta.0 <- c(0, 0)
  
  D.T2 <- n * (D.mean - delta.0) %*% D.invcov %*% (D.mean - delta.0)
  D.T2
  cfr.fisher <- ((n - 1) * p / (n - p)) * qf(1 - alpha, p, n - p)
  cfr.fisher
  
  D.T2 < cfr.fisher # FALSE: we reject H0 at level alpha -> the two labs measure differently
  
  P <- 1 - pf(D.T2 * (n - p) / (p * (n - 1)), p, n - p)
  P
  # low -> reject: they are different
}
# Bonferroni intervals 
{
  k <- p  # Number of Bonferroni intervals
  # If he asks for variance also, must be 2*p
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
  
}
# Girona has better tapas


data1 <- (data1[,1] + data1[,2]) /2
data2 <- (data2[,1] + data2[,2]) /2

# H0: d1 < d2
t.test(data1-data2, alternative='greater')
# yes, we can say with any confidence


