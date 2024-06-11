# Marco Scarpelli

# Exam 07/07/2023

# Exercise 2

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
#library(plot.matrix)
library(nlmeU)
library(lattice)

rm(list=ls())
graphics.off()

df <- read.table('consumption.txt', header=T)

head(df)
n<-dim(df)[1]
p<-dim(df)[2]

x.mean    <- colMeans(df)
x.cov    <- cov(df)
x.invcov <- solve(x.cov)

##########################################
# Point A + B

# Check MVN
mvn(df)$multivariateNormality # p=0.4053346

# Contrast matrix 
C <- matrix(  c(1, 0, -1, 0, 0, 0, # Day0(AM) vs. Day1(AM)
                0, 1, 0, -1, 0, 0, # Day0(PM) vs. Day1(PM)
                0, 0, 1, 0, -1, 0, # Day1(AM) vs. Day2(AM)
                0, 0, 0, 1, 0, -1 # Day1(PM) vs. Day2(PM)
                ), 
              4, 6, #rows, columns
              byrow=T)
C
q<- min (p, 4)
delta.0 <- c(0,0,0,0) #change accordingly

alpha <- 0.05
Md <- C %*% x.mean
Sd <- C %*% x.cov %*% t(C)
Sdinv <- solve(Sd)

T2 <- n * t( Md - delta.0 ) %*% Sdinv %*% ( Md - delta.0 )

cfr.fisher <- (q*(n-1)/(n-q))*qf(1-alpha,q,n-q) 

T2 < cfr.fisher
T2
cfr.fisher


P <- 1-pf(T2*(n-q)/((n-1)*q), q, n-q)
P

boxplot(df, las=2, col='gold',pch=20
        , main='Data Boxplot')  
# We support the hypothesis. Let us look at the boxplot:
#   on average, the cosumption is lower in the day than in
#   the night.

##########################################
# Point C

A <- 1/6 * rbind(c(1,1,1,1,1,1))

q<-min(p, dim(A)[1])

cfr.fisher<-((n-1)*q/(n-q))*qf(1-alpha,q,n-q)


ICT2 <- data.frame(
  inf= A %*% x.mean - sqrt(diag(A %*% x.cov %*% t(A))/n * cfr.fisher), 
  center= A %*% x.mean,
  sup= A %*% x.mean + sqrt(diag(A %*% x.cov %*% t(A))/n * cfr.fisher)) 
ICT2
#      inf   center      sup
# 37.66071 39.57787 41.49503