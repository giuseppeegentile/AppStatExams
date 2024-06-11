# Marco Scarpelli

# Exam 07/07/2023

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
#library(plot.matrix)
library(nlmeU)
library(lattice)

rm(list=ls())
graphics.off()

df <- read.table('products.txt', header=T)

head(df)
n<-dim(df)[1]
p<-dim(df)[2]


##########################################
# Point A
boxplot(scale(df, center=TRUE, scale=FALSE), las=2, col='gold',pch=20
        , main='Data Boxplot')  

# It seems that everything has the same order of
#   magnitude -> we can use the original data.

mean<-colMeans(df)
cov<- cov(df)
SD<-diag(cov) #if necessary for standardization
var.gen <- det(cov)
var.tot <- sum(diag(cov))

mydf<-df #df or scaled df
pca.df <- princomp(mydf, scores=T)  #or prcomp
summary(pca.df)

load.df <- pca.df$loadings #leggo le principal components per colonne #Full list: load.df[,]
plot(cumsum(pca.df$sd^2)/sum(pca.df$sd^2), type='b', axes=F, xlab='number of components', 
     ylab='contribution to the total variance', ylim=c(0,1), main="Screeplot")  
abline(h=0.8, lty=2, col='blue')
box()
axis(2,at=0:10/10,labels=0:10/10)
axis(1,at=1:ncol(mydf),labels=1:ncol(mydf),las=2)

# We need up to the fifth component.

##########################################
# Point B
n.PCs<- 3
par(mfrow=c(1,n.PCs))
for(i in 1:n.PCs) 
  barplot(pca.df$loadings[,i], ylim = c(-1, 1), main=paste("PC",i), 
          cex.axis = 0.4, las=2) #to change dimension of labels
par(mfrow=c(1,1))

# From the plot we can see that the first PC is about
#   the contrast between electronics and home appliances.

##########################################
# Point C
graphics.off()
biplot(pca.df)
# It seems that store 134 sells a lot of appliances compared
#   to electronics, and is also low on component 2 which seems
#   to be the general mean of the data.

##########################################
# Point D
newdatapoint <- c(1.41, 1.14, 1.02, 1.21, 1.11, 1.14, 0.99, 0.73, 0.94, 1.04, 1.02, 1.11)
mydf <- df
newdatapoint.proj <- t(pca.df$loadings[,1:5])%*%(newdatapoint) #mydf è quello su cui ho fatto la pca
newdatapoint.proj
#         [,1]
# Comp.1 -0.2118011
# Comp.2  3.4581859
# Comp.3 -0.1983980
# Comp.4 -0.1401019
# Comp.5  0.7682464