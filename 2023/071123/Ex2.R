# NOME COGNOME

# Exam 07/11/2023

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
library(nlmeU)
library(lattice)
library(class)
library(dbscan)
library(cluster)

# ATTENZIONE: questa libreria interferisce con biplot
# library(plot.matrix) 


rm(list=ls())
graphics.off()

df.full <- read.table('gemstones.txt', header=T)

group <- factor(df.full$Type)

df <- df.full[,1:8]

head(df)
n<-dim(df)[1]
p<-dim(df)[2]

mean<-colMeans(df)
#Covariance matrix
cov<- cov(df)
#SD
SD<-diag(cov) #if necessary for standardization
#Correlation matrix
R <- cor(df)
#Generalized variance
var.gen <- det(cov)
#Total variance
var.tot <- sum(diag(cov))

# Again, this problem is strikingly similar to the
#   bean exam. Or was it rice?

##########################################
# Point A

# Area and ConvexArea's values are huge w.r.t. the others, and
#   Eccentricity and Roundess go from 0 to 1. We must scale
#   the data frame.

boxplot(scale(df, center=TRUE, scale=TRUE), las=2, col='gold',pch=20
        , main='Data Boxplot')  

mydf<-scale(df)
pca.df <- princomp(mydf, scores=T)  #or prcomp
summary(pca.df)

##########################################
# Point B
load.df <- pca.df$loadings #leggo le principal components per colonne #Full list: load.df[,]
n.PCs<-  2
par(mfrow=c(1,n.PCs))
for(i in 1:n.PCs) 
  barplot(pca.df$loadings[,i], ylim = c(-1, 1), main=paste("PC",i), 
          cex.axis = 0.4, las=2) #to change dimension of labels
par(mfrow=c(1,1))

# ... interpretation of the loadings is the same as the bean exam.

scores.df <- pca.df$scores
col.lab<-factor(as.numeric(group))

plot(scores.df[,1:2], col=col.lab, main="scores along the first two PCs", pch=19)
abline(h=0, v=0, lty=2, col='grey')
legend("topright", legend=c(levels(group)), col=levels(col.lab), lty=1) #only in plot

# Again same as the bean/rice/whatever exam.

##########################################
# Point C
df.ruby <- scores.df[df.full$Type=='ruby',1:2]

mvn(df.ruby)$multivariateNormality # p = 0.01198651
# The test fails by a tiny bit. We still go on.

n <- dim(df.ruby)[1]
p <- dim(df.ruby)[2]

x.mean    <- colMeans(df.ruby)
x.cov    <- cov(df.ruby)
x.invcov <- solve(x.cov)

alpha<-.05
cfr.fisher <- ((n-1)*p/(n-p))*qf(1-alpha,p,n-p)

#Formulas 
#-Center: mean
#-direction of the axes: eigenvectors of the variance-covariance matrix
#-radius:sqrt(cfr.fisher)
#lenght of the semi-axes: radius*sqrt(eigenvalues of (x.cov/n))

# Mean, radius, axes length, etc.
direction.axes<-eigen(x.cov)$vector
radius<-sqrt(cfr.fisher)
length.semi.axes<-radius*sqrt(eigen(x.cov/n)$values)
# Print all together
c(mean=x.mean, semi.axes.length=length.semi.axes,radius=radius) 
# mean.Comp.1       mean.Comp.2 semi.axes.length1 
#   0.5239404        -0.7231025         0.4465617 
# semi.axes.length2            radius 
#         0.2555111         2.5523326 


plot(df.ruby, asp = 1, main='Confidence region for the mean',xlab=colnames(df.ruby)[1],ylab=colnames(df.ruby)[2]) # Plots the data points; cex is the size of the points
#cov is divided by n in the case of confidence region of the mean!
ellipse(center=x.mean, shape= x.cov/n, radius=sqrt(cfr.fisher), col = 'red', lty = 1, lwd=2, center.cex=1)

points(x.mean[1],x.mean[2], col='red') #sample mean
