setwd("C:/Users/Giuseppe/Documents/.magistrale/secondo anno/AppStat/Exams/2020/20200907/20200907")
library(car)
library(MVN)

data1 <- read.table('candle.txt', header = T)
data2 <- read.table('sunshine.txt', header = T)
head(data1)
dim(data1)
dim(data2)

cov(data1)
cov(data2)

# similar -> paired data

{
  D <- data.frame(
    D_feature1 = data1$LM1 - data2$LM1,
    D_feature2  = data1$LM2  - data2$LM2
  ) 
  head(D)
  n <- dim(D)[1]
  p <- dim(D)[2]
  
  par(mfrow=c(1,1))
  plot(D, asp=1, pch=19, main='Dataset of Differences')
  abline(h=0, v=0, col='grey35')
}


n <- dim(D)[1]
p <- dim(D)[2]

# Assumption: normality for the new dataset (not for the old one!)
result <- mvn(D)
result$multivariateNormality
# gaussian, ok


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



## Bonferroni intervals 
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
  # there is difference: sunshine has lower measurment of LM2 and higher LM1
  
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
  # For k=2 the Bonferroni rectangle is smaller than the T2 rectangle. 
  # Increasing the value of k, the Bonferroni region is larger.
}

# decrease from LM1 to LM2: take pos value since is easier to interpret
candle <- data1$LM1 - data1$LM2
candle

sunshine<- data2$LM1 - data2$LM2
sunshine
# meancandle > meansunshine


shapiro.test(candle)
shapiro.test(sunshine)

# H0: sunshine < candle
t.test(candle-sunshine, alternative='greater')

