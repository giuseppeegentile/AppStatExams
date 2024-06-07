setwd("C:/Users/Giuseppe/Documents/.magistrale/secondo anno/AppStat/Exams/2019/20190117/20190117")
data <- read.table("luggage.txt",header=T)
dim(data)
head(data)

D <- data.frame(
  D_feature1 = data$M.out - data$F.out,
  D_feature2  = data$M.ret  - data$F.ret
) 
D1 <- D
head(D)
n <- dim(D)[1]
p <- dim(D)[2]


plot(D, asp=1, pch=19, main='Dataset of Differences')
abline(h=0, v=0, col='grey35')
# Assumption: normality for the new dataset (not for the old one!)
result <- mvn(D)
result$multivariateNormality
# high p-val -> normality


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
  # low -> reject, there is difference
}

# b

D <- data.frame(
  D_feature1 = data$M.out - data$M.ret,
  D_feature2  = data$F.out  - data$F.ret
) 
D2 <- D
head(D)
n <- dim(D)[1]
p <- dim(D)[2]


plot(D, asp=1, pch=19, main='Dataset of Differences')
abline(h=0, v=0, col='grey35')
# Assumption: normality for the new dataset (not for the old one!)
result <- mvn(D)
result$multivariateNormality
# high p-val -> normality


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
  # low -> reject, there is difference
}


D <- data.frame(D1,D2)
D.mean   <- sapply(D, mean) 
D.cov    <- cov(D)

{
  n
  p <- dim(D)[2]
  alpha = 0.1
  cfr.fisher <- ((n - 1) * p / (n - p)) * qf(1 - alpha, p, n - p)
  
  
  IC.T2.feature_1 <-
    c(D.mean[1] - sqrt(cfr.fisher * D.cov[1, 1] / n),
      D.mean[1],
      D.mean[1] + sqrt(cfr.fisher * D.cov[1, 1] / n))
  
  IC.T2.feature_2  <-
    c(D.mean[2] - sqrt(cfr.fisher * D.cov[2, 2] / n),
      D.mean[2],
      D.mean[2] + sqrt(cfr.fisher * D.cov[2, 2] / n))
  
  IC.T2.feature_21  <-
    c(D.mean[3] - sqrt(cfr.fisher * D.cov[3, 3] / n),
      D.mean[3],
      D.mean[3] + sqrt(cfr.fisher * D.cov[3, 3] / n))
  IC.T2.feature_22  <-
    c(D.mean[4] - sqrt(cfr.fisher * D.cov[4,4] / n),
      D.mean[4],
      D.mean[4] + sqrt(cfr.fisher * D.cov[4,4] / n))
  
  T2 <- rbind(IC.T2.feature_1, IC.T2.feature_2,IC.T2.feature_21, IC.T2.feature_22)
  dimnames(T2)[[2]] <- c('inf', 'center', 'sup')
  T2
}
# no intervals contains the 0: difference in every direction of the features


# Single direction interval, on direction F.ret

shapiro.test(data$F.ret)

# H0: d1 < d2
t.test(data[, 4], mu = 23, alternative='greater')
# pval is 1, we can't reject -> no, I won't believe the passenger will get a charge





