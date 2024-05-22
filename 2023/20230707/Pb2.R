setwd("C:/Users/Giuseppe/Documents/.magistrale/secondo anno/AppStat/Exams/2023/20230707/20230707")
library(mvtnorm) # to deal with multivariate normal distributions
library(car) # "Companion to Applied Regression" for regression analysis


data <- read.table('consumption.txt', header=T)
head(data)

dim(data)

n <- dim(data)[1]
p <- dim(data)[2]
pairs(data, pch=19)

mvn(data)$multivariateNormality #we have normality satisfied
n <- dim(data)[1]
q <- dim(data)[2]


M <- sapply(data, mean) # sample mean
M
S <- cov(data) # covariance matrix
S


# a)
# Suspect that the pattern is in the times (and not among days)
matplot(t(data), type='l', lty = 1)




#b)
# Contrast matrix: t against t+1
{
  C <- matrix (0, nrow=5, ncol=6)
  for(i in 1:5)
  {
    C[i,c(i,i+1)]<-c(-1,1)
  }
    
}


#H0: C %*% mu == c(0, 0, 0) vs H1: C %*% mu != c(0, 0, 0)
alpha   <- .05
delta.0 <- c(0, 0, 0, 0, 0)

Md <- C %*% M # Sample mean of the "contrasted" observations
Sd <- C %*% S %*% t(C) # Sample covariance of the contrasted observations
Sdinv <- solve(Sd)

# Hotelling T2 statistics
T2 <- n * t(Md - delta.0) %*% Sdinv %*% (Md - delta.0)

# (q-1)*(n-1)/(n-(q-1)) times the 1-alpha Fisher quantile with q-1 and n-q+1 df
cfr.fisher <- ((q - 1) * (n - 1) / (n - (q - 1))) * qf(1 - alpha, (q - 1), n - (q - 1)) 

T2 < cfr.fisher # FALSE ->  reject
T2
cfr.fisher

P <- 1 - pf(T2 * (n - (q - 1)) / ((q - 1) * (n - 1)), (q - 1), n - (q - 1))
P
# pval is zero -> clear pattern of the effect


# d)
{
  C <- matrix(c(1, 0, 0, 0,0,0,
                0, 1, 0, 0,0,0,
                0, 0, 1, 0,0,0,
                0, 0, 0, 1,0,0,
                0, 0, 0, 0,1,0,
                0, 0, 0, 0,0,1), 6, 6, byrow=T)
  C
  Md <- C %*% M # Sample mean of the "contrasted" observations
  Md
  
  
  Sd <- C %*% S %*% t(C) # Sample covariance of the contrasted observations
  Sdinv <- solve(Sd)
  
  # Hotelling T2 statistics
  T2 <- n * t(Md - delta.0) %*% Sdinv %*% (Md - delta.0)
  
  # (q-1)*(n-1)/(n-(q-1)) times the 1-alpha Fisher quantile with q-1 and n-q+1 df
  cfr.fisher <- ((q - 1) * (n - 1) / (n - (q - 1))) * qf(1 - alpha, (q - 1), n - (q - 1)) 
  
  IC.T2 <- cbind(Md - sqrt(cfr.fisher*diag(Sd)/n),
                 Md,
                 Md + sqrt(cfr.fisher*diag(Sd)/n))
  IC.T2
  
  sum(Md)/3 #average consumption per working day
  
}

