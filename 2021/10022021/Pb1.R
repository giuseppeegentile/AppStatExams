setwd("C:/Users/Giuseppe/Documents/.magistrale/secondo anno/AppStat/Exams/2021/20210210/20210210")
library(car)
library(MVN)

data <- read.table('shopping.txt', header = T)

head(data)
dim(data)

D <- data
n <- dim(D)[1]
p <- dim(D)[2]

# Assumption: normality for the new dataset (not for the old one!)
result <- mvn(D)
result$multivariateNormality

# we have normality assumption satisfied

# Confidence region for true mean difference with confidence level 95%
{
  D.mean   <- sapply(D, mean) 
  # if D.mean and delta.0 incompatible -> D.mean   <- colMeans(D)
  D.cov    <- cov(D)
  D.invcov <- solve(D.cov)
  
  alpha   <- .05
  alpha   <- .05
  cfr.fisher <- ((n - 1) * p / (n - p)) * qf(1 - alpha, p, n - p)
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

# Issues: there must be independence between features, here accesses to store and purchases may be dependent
# also purchase of mens and womens (like a couple that go shopping together)


{
  n <- dim(data)[1]
  p <- dim(data)[2]
  data.mean   <- sapply(data,mean)
  data.cov    <- cov(data)
  
  alpha <- 0.10
  cfr.fisher <- (p*(n-1)/(n-p))*qf(1-alpha,p,n-p)
  
  # Interval for mean1, mean2, mean3 and mean2+mean3 
  A <- rbind(c(1,0,0), c(0,1,0), c(0,0,1), c(0,1,1))
  
  ICT2 <- cbind(A %*% data.mean - sqrt(diag(A %*% data.cov %*% t(A))/n * cfr.fisher), 
                A %*% data.mean,
                A %*% data.mean + sqrt(diag(A %*% data.cov %*% t(A))/n * cfr.fisher)) 
  ICT2
  
}
# [,1]      [,2]      [,3]
# [1,] 197.54406 211.12500 224.70594
# [2,]  15.61193  20.79167  25.97140
# [3,]  22.95751  27.04167  31.12582
# [4,]  41.45585  47.83333  54.21082



data$purchases <- data$men + data$women


# H0: more than 20% of the access are purchase
# H0: 0.2*access > purchase
# H0: 0.2*access - purchase > 0

# 

t.test(data$accesses*0.2 - data$purchases, alternative="less")
# We reject the null hp that more than 20% of the access are purchase

# we can say that 0.2*access < purchase

