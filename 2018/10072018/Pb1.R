setwd("C:/Users/Giuseppe/Documents/.magistrale/secondo anno/AppStat/Exams/2018/20180710/20180710")
library(car)
library(MVN)


# data <- read.table('pressure.txt', col.names = c('h.0', 'h.8', 'h.16', 'h.24'))
data <- read.table('Revenues.txt', header = T)

head(data)
dim(data)


# Plotting each observation as a line with the time on x-axis
# (in this case h0, h8, h16, h24)
matplot(t(data), type='l', lty = 1)
# drop in september, increase in summer time (Jul Aug the max)


# Test for multivariate normality (Henze-Zirkler test by default)
mvn(data)$multivariateNormality
n <- dim(data)[1]
q <- dim(data)[2]



M <- sapply(data, mean) # sample mean
M
S <- cov(data) # covariance matrix
S


# Contrast matrix: t against t+1
{
  C <- matrix (0, nrow=q-1, ncol=q)
  for(i in 1:(q-1))
    C[i,c(i,i+1)]<-c(-1,1)
}
  
alpha   <- .1
delta.0 <- c(0, 0, 0)

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
# low pval -> reject -> drug has influence



# Bonferroni intervals 
{
  k <- q-1   # number of steps of the time (dim(C)[1])
  cfr.t <- qt(1-alpha/(2*k), n-1)
  
  IC.BF <- cbind(Md - cfr.t*sqrt(diag(Sd)/n),
                 Md,
                 Md + cfr.t*sqrt(diag(Sd)/n))
  IC.BF
  
}

# [,1]       [,2]       [,3]
# [1,]   6209.007   8535.820  10862.633
# [2,]   3277.272   5553.528   7829.784
# [3,] -46612.238 -44201.275 -41790.312

# significant increase from jun to july
#significant increase from july to aug
# significant decrease from  aug to sept



