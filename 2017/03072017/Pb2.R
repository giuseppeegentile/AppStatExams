setwd("C:/Users/Giuseppe/Documents/.magistrale/secondo anno/AppStat/Exams/2017/20170703/20170703")
library(car)
library(MVN)
library(MVN)
library(car)
library(heplots)
data <- read.table('bento.txt',header=T)
head(data)
dim(data)

# 1:  Compute the differences FEATURE WISE (can be more than 2 features pairs!)
{
  D <- data.frame(
    D_feature1 = data$rice_hanami - data$rice_nohanami,
    D_feature2  = data$sashimi_hanami - data$sashimi_nohanami,
    D_feature3 = data$vegetables_hanami - data$vegetables_nohanami,
    D_feature4  = data$okashi_hanami  - data$okashi_nohanami
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
# T2 Hotelling Test H0:  delta.0 = (0,0)
{
  D.mean   <- sapply(D, mean) 
  # if D.mean and delta.0 incompatible -> D.mean   <- colMeans(D)
  D.cov    <- cov(D)
  D.invcov <- solve(D.cov)
  
  alpha   <- .05
  delta.0 <- c(0, 0,0,0)
  
  D.T2 <- n * (D.mean - delta.0) %*% D.invcov %*% (D.mean - delta.0)
  D.T2
  cfr.fisher <- ((n - 1) * p / (n - p)) * qf(1 - alpha, p, n - p)
  cfr.fisher
  
  D.T2 < cfr.fisher # FALSE: we reject H0 at level alpha -> the two labs measure differently
  
  P <- 1 - pf(D.T2 * (n - p) / (p * (n - 1)), p, n - p)
  P
  # low -> reject
}
# yes, there is significant difference


{
  
  IC.T2.feature_1 <-
    c(D.mean[1] - sqrt(cfr.fisher * D.cov[1, 1] / n),
      D.mean[1],
      D.mean[1] + sqrt(cfr.fisher * D.cov[1, 1] / n))
  
  IC.T2.feature_2  <-
    c(D.mean[2] - sqrt(cfr.fisher * D.cov[2, 2] / n),
      D.mean[2],
      D.mean[2] + sqrt(cfr.fisher * D.cov[2, 2] / n))
  
  IC.T2.feature_3 <-
    c(D.mean[3] - sqrt(cfr.fisher * D.cov[3, 3] / n),
      D.mean[3],
      D.mean[3] + sqrt(cfr.fisher * D.cov[3, 3] / n))
  
  IC.T2.feature_4  <-
    c(D.mean[4] - sqrt(cfr.fisher * D.cov[4, 4] / n),
      D.mean[4],
      D.mean[4] + sqrt(cfr.fisher * D.cov[4, 4] / n))
  
  
  
  T2 <- rbind(IC.T2.feature_1, IC.T2.feature_2,IC.T2.feature_3, IC.T2.feature_4)
  dimnames(T2)[[2]] <- c('inf', 'center', 'sup')
  T2
}








