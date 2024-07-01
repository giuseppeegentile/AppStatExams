# Marco Scarpelli

# Exam 2024/02/06

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
library(dbscan)
library(cluster)

# ATTENZIONE: questa libreria interferisce con biplot
# library(plot.matrix) 


rm(list=ls())
graphics.off()

df <- read.table('diet.txt', header=T)

factor.name.1 = "Vegetarian"
factor.labels.1 = NULL
for (i in 1 : length(levels(factor(df$vegetarian)))) {
     factor.labels.1 = rbind(factor.labels.1, 
                             paste(factor.name.1, 
                                   levels(factor(df$vegetarian))[i],
                                   sep="-"))
}
factor.name.2 = "Vitamin"
factor.labels.2 = NULL
for (i in 1 : length(levels(factor(df$vitamin)))) {
  factor.labels.2 = rbind(factor.labels.2, 
                          paste(factor.name.2, 
                                levels(factor(df$vitamin))[i],
                                sep="-"))
}
     
factor_1   <- factor(df$vegetarian, labels=factor.labels.1) 
factor_2   <- factor(df$vitamin, labels=factor.labels.2) 

combined_factors<-factor_1:factor_2

head(df)
n<-dim(df)[1]
p<-2

g <-  length(levels(factor(df$vegetarian))) #number of levels of the first factor
b <- length(levels(factor(df$vitamin))) #number of levels of the second factor

df.onlyNumerical  <- df[,1:2]
head(df.onlyNumerical)
dim(df.onlyNumerical)

n<-dim(df.onlyNumerical)[1]
ng<-table(df.onlyNumerical)

plot(df.onlyNumerical, main='', pch=19) # plot if p=1 or 2, pairs otherwise

##########################################
# Point A
fit <- manova(as.matrix(df.onlyNumerical) ~ factor_1 + factor_2 + factor_1:factor_2)
summary.manova(fit, test="Wilks")
#                    Df   Wilks approx F num Df den Df    Pr(>F)    
# factor_1            1 0.92317    8.115      2    195 0.0004119 ***
# factor_2            1 0.69540   42.707      2    195 4.148e-16 ***
# factor_1:factor_2   1 0.99509    0.482      2    195 0.6185741    
# Residuals         196       

# The interaction seems not to be significant.

##########################################
# Point B

# Assumptions:
#   1. Multivariate normality in each group (factor + level)
Ps<-NULL
for (i in 1:length(levels(combined_factors)))  #for each level of each possible combination of factors level
  Ps <- c(Ps, mvn(df.onlyNumerical[ combined_factors==levels(combined_factors)[i], ])$multivariateNormality$`p value`) 
Ps # 0.7344040 0.3956949 0.2574353 0.9696521, OK

# 2) homogeneity of the covariance (qualitatively)
## In THIS case we are satisfied if ANY coefficient is at most 10 times of another
##    (usually it should not go above 4, but with MANOVA ***specifically*** we can go up to 10).
S1 <-  cov(df.onlyNumerical[combined_factors==levels(combined_factors)[1], ])
S2 <-  cov(df.onlyNumerical[combined_factors==levels(combined_factors)[2], ])
S3 <-  cov(df.onlyNumerical[combined_factors==levels(combined_factors)[3], ])
S4 <-  cov(df.onlyNumerical[combined_factors==levels(combined_factors)[4], ])

max(S1,S2,S3,S4) / min(S1,S2,S3,S4) # 6.096221, OK

##########################################
# Point C

# The interaction seems not to be significant, so I would remove it.

fit2<- manova( as.matrix(df.onlyNumerical) ~ factor_1 + factor_2)
summary.manova(fit2, test="Wilks")
anova(fit,fit2)

alpha <- 0.05
k <- p*g*(g-1)/2 + p*b*(b-1)/2
p <- dim(df.onlyNumerical)[2] # p features
p
g1 <- length(levels(factor_1))
g1
g2 <- length(levels(factor_2))
g2
n <- dim(df.onlyNumerical)[1]/(g1*g2) # number of  repetitions for every combo of factors
n_tot <- n*g*b
n_tot
W <- summary.manova(fit2)$SS$Residuals
W
qT <- qt(1 - alpha / (2 * k), g1*g2*n-g1-g2+1)
mf1.1  <- sapply(df.onlyNumerical[factor_1==factor.labels.1[1],],mean)
mf1.2  <- sapply(df.onlyNumerical[factor_1==factor.labels.1[2],],mean)

ng = NULL
for (i in 1:length(levels(factor_1))) {
  ng = c(ng, 
         length(which(factor_1==levels(factor_1)[i] 
         )))
}
for (i in 1:length(levels(factor_2))) {
  ng = c(ng, 
         length(which(factor_2==levels(factor_2)[i] 
         )))
}
names(ng) <- c(levels(factor_1), levels(factor_2))
ng
# Vegetarian-FALSE  Vegetarian-TRUE    Vitamin-FALSE     Vitamin-TRUE 
#              100              100              100              100 


mf1.1  <- sapply(df.onlyNumerical[factor_1==factor.labels.1[1],],mean)
mf1.2  <- sapply(df.onlyNumerical[factor_1==factor.labels.1[2],],mean)
inf1 <- mf1.2-mf1.1 - qT * sqrt( diag(W)/(g1*g2*n-g1-g2+1) * (1/ng[1]+1/ng[2]) )
sup1 <- mf1.2-mf1.1 + qT * sqrt( diag(W)/(g1*g2*n-g1-g2+1) * (1/ng[1]+1/ng[2]) )

mf2.1  <- sapply(df.onlyNumerical[factor_2==factor.labels.2[1],],mean)
mf2.2  <- sapply(df.onlyNumerical[factor_2==factor.labels.2[2],],mean)
inf2 <- mf2.2-mf2.1 - qT * sqrt( diag(W)/(g1*g2*n-g1-g2+1) * (1/ng[3]+1/ng[4]) )
sup2 <- mf2.2-mf2.1 + qT * sqrt( diag(W)/(g1*g2*n-g1-g2+1) * (1/ng[3]+1/ng[4]) )

IC2   <- list(cbind(inf1, sup1), cbind(inf2, sup2))
names(IC2) <- c(factor.name.1, factor.name.2)
IC2
# $Vegetarian
#                   inf1      sup1
# pressure     -4.597374  2.337374
# cholesterol -17.437927 -3.660073
# 
# $Vitamin
#                   inf2      sup2
# pressure    -14.079374 -7.144626
# cholesterol  -4.935927  8.841927

# Intervals that include 0 show that that factor has no statistical
#   significance w.r.t. the variable we are considering;
#   this means that being vegetarian has no effect on pressure, and
#   vitamin has no effect on cholesterol. Conversely, being vegetarian
#   is shown as having a negative impact on cholesterol (in the sense
#   that cholesterol is lowered) and the same goes for the impact
#   of vitamin intake on blood pressure.