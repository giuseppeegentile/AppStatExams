# Marco Scarpelli

# Exam 07/02/2023

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

df <- read.table('pressure.txt', header=T)

head(df)

group<-factor(df$pressure)

class.A<- levels(group)[1] # name of class A
class.B<- levels(group)[2] # name of class B

iA <- which(df$pressure==levels(group)[1])  
iB <- which(df$pressure==levels(group)[2])  

df<-df[,1:2] # keep the quantitative columns

n<-dim(df)[1]
p<-dim(df)[2]

# Jittering
# df <- df + cbind(rnorm(n, sd=sapply(df, mean)*0.01)) 

col.lab<-rep(0,n)
col.lab[iA]<-'red'
col.lab[iB]<-'blue'


plot(df, col=col.lab, main='', pch=19) 
# plot if p=1 or 2, pairs otherwise
legend("topright", legend=levels(group), fill=c('red','blue'), lty=c(1,1)) #only in plot

##########################################
# Point A

# Assumptions: multivariate normality and, for LDA, similar
#   covariance structure
p1<-mvn(df[iA,])$multivariateNormality 
p2<-mvn(df[iB,])$multivariateNormality
rbind(p1,p2)

# One of the two has no normality

SA<-cov(df[iA,])
SB<-cov(df[iB,])
# round(SA, 3)
# round(SB, 3)
round(diag(SA/SB), 3)

# The covariance structures are similar,
#   and LDA is more robust to departures
#   to Gaussianity, so I will use LDA.

nA <- length(iA)
nB <- length(iB)
n  <- nA + nB
pA <- nA / n
pB <- nB / n
priors<-c(pA,pB)

# Priors: 0.5, 0.5
priors

df.da <- lda(df, group, prior=priors)

# Parameters
df.da
# Group means:
#   x        y
# H 19.66933 55.62965
# L  9.68124 55.49661
# 
# Coefficients of linear discriminants:
#   LD1
# x -0.17652834
# y  0.06425736

da.on.df  <- predict(df.da, df)

pred.classes<-da.on.df$classes

indexes.pred.A<-which(pred.classes==class.A) #fare check!
indexes.pred.B<-which(pred.classes==class.B)

table(class.true=group, class.assigned=da.on.df$class)

x  <- seq(min(df[,1]), max(df[,1]), length=200)
y  <- seq(min(df[,2]), max(df[,2]), length=200)
xy <- expand.grid(x=x, y=y) #change accordingly

z  <- predict(df.da, xy)$post  # P_i*f_i(x,y)  
# Uno z per ogni gruppo
z1 <- z[,1] - pmax(z[,2]) #, z[,3])  # P_1*f_1(x,y)-max{P_j*f_j(x,y)} #pmax Returns the (regular or parallel) maxima and minima of the input values.
z2 <- z[,2] - pmax(z[,1]) #, z[,3])  # P_2*f_2(x,y)-max{P_j*f_j(x,y)}  
#z3 <- z[,3] - pmax(z[,1], z[,2])    # P_3*f_3(x,y)-max{P_j*f_j(x,y)}

# Plot the contour line of level (levels=0) of z1, z2, z3: 
# P_i*f_i(x,y)-max{P_j*f_j(x,y)}=0 i.e., boundary between R.i and R.j where j realizes the max.

plot(df, col=col.lab,main='', pch=19) # plot if p=1 or 2, pairs otherwise
# Un contour per ogni gruppo
# Plot the contour line of level (levels=0) of z1, z2, z3: 
#   P_i*f_i(x,y)-max{P_j*f_j(x,y)}=0 i.e., boundary between R.i and R.j 
#   where j realizes the max.
contour(x, y, matrix(z1, 200), levels=0, drawlabels=F, add=T)  
contour(x, y, matrix(z2, 200), levels=0, drawlabels=F, add=T)
# Group means
points(df.da$means, pch=4,col=c('red','blue') , lwd=2, cex=1.5)

legend("topright", legend=levels(group), fill=c('red','blue'), lty=c(1,1)) 

##########################################
# Point B
# The model, being linear, fails to account for the curved
#   shape of the clusters.
# Priors estimated from dataset 
daCV<- lda(df, group, CV=TRUE) 

errorsqCV <- (daCV$class != group)
AER_CV   <- sum(errorsqCV)/length(group)
AER_CV # 0.21

#check: sum of first diagonal / total sum
miscCV <- table(class.true=group, class.assigned=daCV$class)
miscCV 

##########################################
# Point C

set.seed(19)

n_obs<- dim(df)[1] #n
k_range<- 10:30 #range of k to test, estremi inclusi

Aerr<-NULL
set.seed(19)  #run seed every time!
for (k in k_range) {
  df.knn <- knn.cv(train = df, cl = group, k = k)
  errorsqCV <- (df.knn != group)
  Aerr   <- c(Aerr,(sum(errorsqCV))/n_obs) #controllare che stiamo parlando di n_obs
}
min(Aerr)
which(Aerr==min(Aerr)) 


#best k
best.index=which.min(Aerr)
kbest<-k_range[best.index]
kbest # 18

AERkCV<-min(Aerr)
AERkCV # 0.195

best <- knn.cv(train = df, cl = group, k = kbest)
x  <- seq(min(df[,1]), max(df[,1]), length=200)
y  <- seq(min(df[,2]), max(df[,2]), length=200)
xy <- expand.grid(COLUMN_1_NAME=x, COLUMN_2_NAME=y) #change accordingly
names(df)

# Remember to use the best k if you have it
df.knn <- knn(train = df, test = xy, cl = group, k = kbest)
z  <- as.numeric(df.knn)


plot(df, main='', pch=20)
points(df[iA,], col='red', pch=19)
points(df[iB,], col='blue', pch=19)
legend("topright", legend=c(levels(group)), fill=c('red','blue'))
contour(x, y, matrix(z, 200), levels=c(1.5, 2.5), drawlabels=F, add=T) #change if more than 2 levels

##########################################
# Point D
new <- data.frame(x = 10.8, y= 39.4)    #change accordingly
knn(train = df, test = new, cl = group, k=kbest)   
# L
