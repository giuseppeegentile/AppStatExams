setwd("C:/Users/Giuseppe/Documents/.magistrale/secondo anno/AppStat/Exams/2018/20180913/20180913")
library(car)
library(MVN)

# CASE 1: Test for the mean of paired multivariate Gaussian observations
# Multiple features measured by two laboratories
# -> you want to prove if they measure not differently


data <- read.table('IAMG.txt', header = T)

head(data)
dim(data)


pairs(data, pch=19, main='Dataset')
D <- data
n <- dim(D)[1]
p <- dim(D)[2]

# Assumption: normality for the new dataset (not for the old one!)
result <- mvn(D)
result$multivariateNormality
# high p-val -> normality
# However, even if the pval is not so low, is ok to proceed assuming normality
# Discuss about issues: there must be independence between features, if you assume there isn't, say it


{
  D.mean   <- sapply(D, mean) 
  # if D.mean and delta.0 incompatible -> D.mean   <- colMeans(D)
  D.cov    <- cov(D)
  D.invcov <- solve(D.cov)
  
  alpha   <- .05
  delta.0 <- c(0, 0,0 )
  
  D.T2 <- n * (D.mean - delta.0) %*% D.invcov %*% (D.mean - delta.0)
  D.T2
  cfr.fisher <- ((n - 1) * p / (n - p)) * qf(1 - alpha, p, n - p)
  cfr.fisher
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

# Simultaneous T2 intervals in the direction of feature_1 and feature_2
{
  n
  p <- dim(D)[2]
  alpha = 0.5
  cfr.fisher <- ((n - 1) * p / (n - p)) * qf(1 - alpha, p, n - p)
  
  # check the parameters before
  
  IC.T2.feature_1 <-
    c(D.mean[1] - sqrt(cfr.fisher * D.cov[1, 1] / n),
      D.mean[1],
      D.mean[1] + sqrt(cfr.fisher * D.cov[1, 1] / n))
  
  IC.T2.feature_2  <-
    c(D.mean[2] - sqrt(cfr.fisher * D.cov[2, 2] / n),
      D.mean[2],
      D.mean[2] + sqrt(cfr.fisher * D.cov[2, 2] / n))
  IC.T2.feature_3  <-
    c(D.mean[3] - sqrt(cfr.fisher * D.cov[3,3] / n),
      D.mean[3],
      D.mean[3] + sqrt(cfr.fisher * D.cov[3,3] / n))
  
  T2 <- rbind(IC.T2.feature_1, IC.T2.feature_2,IC.T2.feature_3)
  dimnames(T2)[[2]] <- c('inf', 'center', 'sup')
  T2
}
#                     inf center     sup
# IC.T2.feature_1 244.198 249.44 254.682
# IC.T2.feature_2 117.052 119.04 121.028
# IC.T2.feature_3  21.167  23.12  25.073


# equivalent to say that 10% of the registred disn't showup
d1 <- (data$Registered*.1 - data$No.show)
shapiro.test(d1)


# H0: 0.1*registred = no_show
t.test(d1, mu=0)
# we can't reject the null hp








