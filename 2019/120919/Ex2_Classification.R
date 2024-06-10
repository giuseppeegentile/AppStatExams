# Marco Scarpelli

# Exam 12/09/2019

# Exercise 3

library(MASS)
library(car)
library(rgl)   #3D plots
library(leaps) #best subset selection
library(tree)  #decision trees
library(corrplot) #correlation
library(glmnet)
library(mvnormtest)
library(MVN)
library(heplots)
library(ggplot2)
library(mvtnorm)
library(class)     #for KNN

rm(list=ls())
graphics.off()

df <- read.table('debris.txt', header=T)

head(df)

group<-factor(df$risk) #df$nameofgroupcolumn

class.A <- levels(group)[1] # name of class A
class.B <- levels(group)[2] # name of class B

iA <- which(df$risk==levels(group)[1])  
iB <- which(df$risk==levels(group)[2]) 

df<-df[,1:2] # keep the quantitative columns

head(df)

n<-dim(df)[1]
p<-dim(df)[2]

col.lab<-rep(0,n)
col.lab[iA]<-'red'
col.lab[iB]<-'blue'

plot(df, col=col.lab,main='', pch=19) 
# plot if p=1 or 2, pairs otherwise
legend("topright", legend=levels(group), fill=c('red','blue'), lty=c(1,1)) #only in plot

##########################################
# Point A

# Check difference between groups is significant
fit<-manova(as.matrix(df)~group) #if univariate anov<-aov((df~group)) summary(anov)
summary(fit, test='Wilks')

# The p-value is very low (2.2e-16), so it is not. The clustering will be difficult.
      
# Assumptions
# Multivariate normality within the groups
p1<-mvn(df[iA,])$multivariateNormality 
p2<-mvn(df[iB,])$multivariateNormality
rbind(p1,p2)

# One of the two clusters fails the multivariate normality test.
# To use LDA, we should observe that no value on the diagonal of each matrix 
#   is greater than 4 times larger than the corresponding diagonal values
#   on the other matrices (i.e. S1_ii and S2_ii differ from less than 4 times)
SA<-cov(df[iA,])
SB<-cov(df[iB,])
SA
SB

nA <- length(iA)
nB <- length(iB)
n  <- nA + nB
pA <- nA / n
pB <- nB / n
priors<-c(pA,pB)

# REPORTING PARAMETERS

# We must use QDA since we observe a diagonal value that is
#   5 times the other. Furthermore, we observe that
#   the clusters have a non-linear boundary.
df.da <- qda(df, group, prior=priors)
df.da

# Means:
#           x         y
# H 2.7830000 -1.233777
# L 0.5692588 -2.873935

SA<-cov(df[iA,])
SB<-cov(df[iB,])
SA
SB
# > SA
# x        y
# x 1.058284 1.067964
# y 1.067964 3.017442
# > SB
# x          y
# x 2.2028985  0.2054168
# y 0.2054168 15.6631038

da.on.df  <- predict(df.da, df)

x  <- seq(min(df[,1]), max(df[,1]), length=200)
y  <- seq(min(df[,2]), max(df[,2]), length=200)
xy <- expand.grid(x=x, y=y) #change accordingly
z  <- predict(df.da, xy)$post  # P_i*f_i(x,y)  
z1 <- z[,1] - pmax(z[,2]) #, z[,3])  # P_1*f_1(x,y)-max{P_j*f_j(x,y)} #pmax Returns the (regular or parallel) maxima and minima of the input values.
z2 <- z[,2] - pmax(z[,1]) #, z[,3])  # P_2*f_2(x,y)-max{P_j*f_j(x,y)}  
plot(df, col=col.lab,main='', pch=19) # plot if p=1 or 2, pairs otherwise
contour(x, y, matrix(z1, 200), levels=0, drawlabels=F, add=T)  
contour(x, y, matrix(z2, 200), levels=0, drawlabels=F, add=T)
legend("bottom", legend=levels(group), fill=c('red','blue'), lty=c(1,1)) #only in plot

##########################################
# Point B

# AER CV
daCV<- qda(df, group, CV=TRUE) #priors estimated from dataset 
errorsqCV <- (daCV$class != group)
AER_CV   <- sum(errorsqCV)/length(group)

round(AER_CV, 3) # 0.1366667 = 0.137

# Sanity check
miscCV <- table(class.true=group, class.assigned=daCV$class)
miscCV

#           class.assigned
# class.true   H   L
#         H 111  19
#         L  22 148

# The model suffers a bit from the fact that the data is
#   not Gaussian, but performs reasonably well otherwise.
# Its biggest weakness is that it cannot create disjoint
#   regions, hence there is a small tail for group "H" that
#   falls wrongly into group "L"

##########################################
# Point C
n_obs<- dim(df)[1] #n
k_range<- 10:30 #range of k to test, estremi inclusi
Aerr<-NULL
set.seed(321)  #run seed every time!
for (k in k_range) {
  df.knn <- knn.cv(train = df, cl = group, k = k)
  errorsqCV <- (df.knn != group)
  Aerr   <- c(Aerr,(sum(errorsqCV))/n_obs) #controllare che stiamo parlando di n_obs
}
# Minimum error:
round(min(Aerr), 3) # 0.097
# k with minimum error:
which(Aerr==min(Aerr)) # 11

# Plot
#KNN on all grid for decision regions
x  <- seq(min(df[,1]), max(df[,1]), length=200)
y  <- seq(min(df[,2]), max(df[,2]), length=200)
xy <- expand.grid(x=x, y=y) #change accordingly
names(df)

# Remember to use the best k if you have it
df.knn <- knn(train = df, test = xy, cl = group, k = 11)
z  <- as.numeric(df.knn)

plot(df, main='', pch=20)
points(df[iA,], col='red', pch=19)
points(df[iB,], col='blue', pch=19)
legend("topright", legend=c(levels(group)), fill=c('red','blue'))
contour(x, y, matrix(z, 200), levels=c(1.5, 2.5), drawlabels=F, add=T) #change if more than 2 levels

##########################################
# Point D

# From what we computed, the KNN classifier is the better one.
new <- data.frame(x = 1, y= -4)    #change accordingly
knn(train = df, test = new, cl = group, k=1)   

# KNN predicts "H"