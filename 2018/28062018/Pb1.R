setwd("C:/Users/Giuseppe/Documents/.magistrale/secondo anno/AppStat/Exams/2018/20180628/20180628")
library(car)
library(MVN)

# CASE 1: Test for the mean of paired multivariate Gaussian observations
# Multiple features measured by two laboratories
# -> you want to prove if they measure not differently


data1 <- read.table('Morning.txt', header = T)
data2 <- read.table('Evening.txt', header = T)

head(data1)
dim(data1)
head(data2)
dim(data2)


{
  D <- data.frame(
    D_feature1 = data1$MEX.OAX - data2$MEX.OAX,
    D_feature2  = data1$OAX.MEX  - data2$OAX.MEX
  ) 
  head(D)
  n <- dim(D)[1]
  p <- dim(D)[2]
  
  
  plot(D, asp=1, pch=19, main='Dataset of Differences')
  abline(h=0, v=0, col='grey35')
}

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
  
  alpha   <- .01
  delta.0 <- c(0, 0)
  
  D.T2 <- n * (D.mean - delta.0) %*% D.invcov %*% (D.mean - delta.0)
  D.T2
  cfr.fisher <- ((n - 1) * p / (n - p)) * qf(1 - alpha, p, n - p)
  cfr.fisher
  
  D.T2 < cfr.fisher # FALSE: we reject H0 at level alpha
  
  P <- 1 - pf(D.T2 * (n - p) / (p * (n - 1)), p, n - p)
  P
  # low -> reject-> there is difference
}





## Bonferroni intervals for morning flights
k <- 4 # Number of Bonferroni intervals
{
  D.mean   <- sapply(data1, mean) 
  # if D.mean and delta.0 incompatible -> D.mean   <- colMeans(D)
  D.cov    <- cov(data1)
  D.invcov <- solve(D.cov)

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
  # IC.BF.feature_1 21.37156 30.56290 39.75425
  # IC.BF.feature_2 29.59741 40.84161 52.08581
}
## Bonferroni intervals for evening flights
{
  D.mean   <- sapply(data2, mean) 
  # if D.mean and delta.0 incompatible -> D.mean   <- colMeans(D)
  D.cov    <- cov(data2)
  D.invcov <- solve(D.cov)
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
  
  # IC.BF.feature_1 36.04064 48.41194 60.78323
  # IC.BF.feature_2 45.26486 67.47258 89.68031
}


head(data1)
#we must test that the mean of data1[,2] is less than 90 min

t.test(data1[,2],mu=90, alternative="less")

# pval very low -> we reject the H0 that we won't be able to take that flight
# -> i'll arrive before the closure of the gate for sure
