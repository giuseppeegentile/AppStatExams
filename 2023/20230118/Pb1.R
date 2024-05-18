setwd("C:/Users/Giuseppe/Documents/.magistrale/secondo anno/AppStat/Exams/2023/20230118/20230118")

library(MVN)

data1 <- read.table("red.txt", header = T)
data2 <- read.table("white.txt", header = T)
head(data1)
head(data2)
dim(data1)
dim(data2)
n1 <- dim(data1)[1]
n2 <- dim(data2)[1]
p  <- dim(data1)[2]



data1.mean <- sapply(data1, mean)
data2.mean <- sapply(data2, mean)

data1.cov <- cov(data1)
data2.cov <- cov(data2)
Sp        <- ((n1 - 1) * data1.cov + (n2 - 1) * data2.cov) / (n1 + n2 - 2)

# We compare the matrices when n1 and n2 are small -> 
# rule of thumb:
#    don't reject equality of covariance if s1_ii and s2_ii differ less than 4
# Do this only if n1 and n2 are small:
list(S1 = data1.cov, S2 = data2.cov, Spooled = Sp)

# We can say they have similar variances



# Test if the mean of the two population differ
alpha   <- .05
delta.0 <- c(0, 0, 0)
Spinv   <- solve(Sp)

T2 <- n1 * n2 / (n1 + n2) * (data1.mean - data2.mean - delta.0) %*% Spinv %*% (data1.mean - data2.mean - delta.0)

cfr.fisher <- (p * (n1 + n2 - 2) / (n1 + n2 - 1 - p)) * qf(1 - alpha, p, n1 + n2 - 1 - p)
T2 < cfr.fisher # H0 is false -> we reject with confidence 95%






#b)
# Rejection region for mean difference
{
  # Directions of the axes
  eigen(Sp)$vector
  
  # Radius
  r <- sqrt(cfr.fisher)
  
  # Length of the semi-axes
  r*sqrt(eigen(Sp)$values*(1/n1+1/n2))
  
  # centre is (0,0,0) since is the rejection region
  # would have been (data1.mean - data2.mean) if it was confidence region
  
  # value of statistic 
  T2
}

# c
P <- 1 - pf(T2 / (p * (n1 + n2 - 2) / (n1 + n2 - 1 - p)), p, n1 + n2 - 1 - p)
P
# We reject at almost any significative level 


# d
# Test for multivariate normality
mvn(data=data1)$multivariateNormality
mvn(data=data2)$multivariateNormality
# high pvals -> normality 

# e
# Bonferroni 
{
  k <- p 
  cfr.t <- qt(1-alpha/(2*k),n1+n2-2)
  Bf1 <- cbind(inf    = data1.mean[1]-data2.mean[1] - cfr.t*sqrt(Sp[1,1]*(1/n1+1/n2)),
               center = data1.mean[1]-data2.mean[1],
               sup    = data1.mean[1]-data2.mean[1] + cfr.t*sqrt(Sp[1,1]*(1/n1+1/n2)))
  Bf2 <- cbind(inf    = data1.mean[2]-data2.mean[2] - cfr.t*sqrt(Sp[2,2]*(1/n1+1/n2)),
               center = data1.mean[2]-data2.mean[2],
               sup    = data1.mean[2]-data2.mean[2] + cfr.t*sqrt(Sp[2,2]*(1/n1+1/n2)))
  Bf3 <- cbind(inf    = data1.mean[3]-data2.mean[3] - cfr.t*sqrt(Sp[3,3]*(1/n1+1/n2)),
               center = data1.mean[3]-data2.mean[3],
               sup    = data1.mean[3]-data2.mean[3] + cfr.t*sqrt(Sp[3,3]*(1/n1+1/n2)))
  Bf <- rbind(Bf1, Bf2, Bf3)
  dimnames(Bf)[[2]] <- c('inf','center', 'sup')    
  Bf
}

# red wine has significantly higher acidity wrt to white wine
# Simultaneously we can't say the same with alcohol and density, which contains 0
# Plot Bonferroni
{
  plot(0,0)
  # Adding quadrant lines and delta.0
  abline(h=0, v=0, col='grey', lty=1, lwd=2)
  points(delta.0[1], delta.0[2], pch=16, col='grey35', cex=1.25)
  
  # Bonferroni intervals in the direction of feature_1 and feature_2
  abline(v = Bf[1,1], col='blue', lwd=1, lty=2)
  abline(v = Bf[1,3], col='blue', lwd=1, lty=2)
  abline(h = Bf[2,1], col='blue', lwd=1, lty=2)
  abline(h = Bf[2,3], col='blue', lwd=1, lty=2)
  segments(Bf1[1], 0, Bf1[3], 0, lty=1, lwd=2, col='blue')
  segments(0, Bf2[1], 0, Bf2[3], lty=1, lwd=2, col='blue')
  segments(0, Bf3[1], 0, Bf3[3], lty=1, lwd=2, col='blue')
}

plot.bonf <- function() {
  matplot(1:p, 1:p, pch='', ylim=c(-1,3),  xlab='Variables',
          ylab='Confidence intervals along a component', main='Confidence intervals')
  
  
  for(i in 1:p) segments(i, Bf[i,1], i, Bf[i,3], lwd=2, col=i)
  points(1:p, Bf[,2], pch=16, col=1:4)
  points(1:p, Bf[,1], pch='-', col=1:4)
  points(1:p, Bf[,3], pch='-', col=1:4)
  
  # Is mu0 inside the Bonferroni confidence region?
  points(1:p, delta.0, lwd=3, col='orange')
  # If it is, is  because it belongs to ALL the intervals along the components  
}
plot.bonf()

