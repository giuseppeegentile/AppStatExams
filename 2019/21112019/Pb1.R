setwd("C:/Users/Giuseppe/Documents/.magistrale/secondo anno/AppStat/Exams/2019/20191121/20191121")

data <- read.table("pigeons.txt",header=T)
head(data)

{
  D <- data.frame(
    D_feature1 = data$weightM - data$weightF,
    D_feature2  = data$wingM   - data$wingF
  ) 
  head(D)
  n <- dim(D)[1]
  p <- dim(D)[2]
  
  
  plot(D, asp=1, pch=19, main='Dataset of Differences')
  abline(h=0, v=0, col='grey35')
  # Interpretation:
  # More points on the upper part: lab 1 tends to have higher values of feature 2. 
  # More points on the right part: lab 1 tends to have higher values of feature 1. 
  
}


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
  delta.0 <- c(0, 0)
  
  D.T2 <- n * (D.mean - delta.0) %*% D.invcov %*% (D.mean - delta.0)
  D.T2
  cfr.fisher <- ((n - 1) * p / (n - p)) * qf(1 - alpha, p, n - p)
  cfr.fisher
  
  D.T2 < cfr.fisher # FALSE: we reject H0 at level alpha -> the two labs measure differently
  
  P <- 1 - pf(D.T2 * (n - p) / (p * (n - 1)), p, n - p)
  P
  # low -> reject: there is a lot of difference, we reject with any confidence
}


## Bonferroni intervals 
{
  k <- p 
  alpha = 0.1
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

# want to test if
# mean(data$wingM) -  mean(data$wingF) > + mean(data$wingF)*0.2

d1 <- (data1[,1] + data1[,2])/2 
d2 <- (data2[,1] + data2[,2])/2 
shapiro.test(d1)
shapiro.test(d2)

# H0: d1 < d2
t.test(data$wingM- data$wingF-  data$wingF*0.2, alternative='greater')
# 7.643e-06: we reject with any confidence -> is true the hypothesis of the ethologists



