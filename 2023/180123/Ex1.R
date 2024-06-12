# Marco Scarpelli

# Exam 18/01/2023

# Exercise 1

library(MASS)
library(car)
library(rgl)
library(leaps)
library(tree)
library(corrplot)
library(glmnet)
library(mvnormtest)
library(MVN)
library(heplots)
library(ggplot2)
library(mvtnorm)
library(nlme)
library(lme4)
library(insight)
library(nlmeU)
library(lattice)
library(class)

# ATTENZIONE: questa libreria interferisce con biplot
# library(plot.matrix) 


rm(list=ls())
graphics.off()

df1<-read.table('red.txt',header = T)
df2<-read.table('white.txt',header = T)

head(df1)
head(df2)

n1 <- dim(df1)[1] # n1
n2 <- dim(df2)[1] # n2
p  <- dim(df1)[2] # p=2 (same for t1 and t2)

df1.mean <- sapply(df1,mean)
df2.mean <- sapply(df2,mean)

df1.cov  <-  cov(df1)
df2.cov  <-  cov(df2)
Sp      <- ((n1-1)*df1.cov + (n2-1)*df2.cov)/(n1+n2-2)
Spinv    <-solve(Sp)

#(S1=df1.cov, S2=df2.cov, Spooled=Sp)

##########################################
# Point A
mvn(data = df1)$multivariateNormality
mvn(data = df2)$multivariateNormality
# OK

round(diag(df1.cov/df2.cov), 3)
# Similar covariance structure, OK
delta<-c(0,0,0) #vector in R^p - change accordingly
alpha<-.05
cfr.fisher <- (p*(n1+n2-2)/(n1+n2-1-p))*qf(1-alpha,p,n1+n2-1-p)

#Statistics
T2 <- n1*n2/(n1+n2) * (df1.mean-df2.mean-delta) %*% Spinv %*% (df1.mean-df2.mean-delta)
T2 < cfr.fisher # TRUE: no statistical evidence to reject H0 at confidence level 1-alpha%

#P value
P <- 1 - pf(T2/(p*(n1+n2-2)/(n1+n2-1-p)), p, n1+n2-1-p)
P # 1.945724e-06

# The means are different.

##########################################
# Point B
# The region of rejection is centered in mu0 (i.e. the
#   vector 0) and its shape is identical to that of
#   the confidence region.

# Its shape is: 
#   {m in R^2 | n(mu0 - m)SpooledInv(mu0 - m) < F*}
#     where F* = (p*(n1+n2-2)/(n1+n2-1-p))F(1-alpha, p, n1+n2-1-p)

##########################################
# Point B
T2 # 40.90507

##########################################
# Point C
P # The p-value is 1.945724e-06

##########################################
# Point D
k <- 3
alpha<-.05
cfr.t <- qt(1-alpha/(2*k),n1+n2-2)

Bf <- cbind(inf = df1.mean - df2.mean - cfr.t*sqrt(diag(Sp)*(1/n1+1/n2)),
            center = df1.mean - df2.mean,
            sup = df1.mean - df2.mean + cfr.t*sqrt(diag(Sp)*(1/n1+1/n2)))
Bf
#                  inf        center         sup
# alcohol -0.849101089 -0.1687878788 0.511525332
# density -0.001104642  0.0008324675 0.002769577
# acidity  1.190038096  2.0677056277 2.945373159

# It seems that the difference in the mean
#   is due to acidity, as alcohol and density's
#   95% intervals include 0.