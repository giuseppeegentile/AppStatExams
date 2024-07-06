# Marco Scarpelli

# Exam 2022/09/09

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

df <- read.table('fossil.txt', header=T)

head(df)
group<-factor(df$type)

class.A<- levels(group)[1] # name of class A
class.B<- levels(group)[2] # name of class B

iA <- which(df$type==levels(group)[1])  
iB <- which(df$type==levels(group)[2])  

df<-df[,1:2] # keep the quantitative columns
head(df) 

n<-dim(df)[1]
n
p<-dim(df)[2]
p

#set.seed(9)
# jittering
#for(i in 1:p){
#  df[,i] <- df[,i] + cbind(rnorm(n, sd=abs(sapply(df, mean)[i])*0.01)) 
#}

col.lab<-rep(0,n)
col.lab[iA]<-'red'
col.lab[iB]<-'blue'

plot(df, col=col.lab, main='', pch=19) 
# plot if p=1 or 2, pairs otherwise
legend("topright", legend=levels(group), fill=c('red','blue'), lty=c(1,1)) #only in plot

##########################################
# Point A
p1<-mvn(df[iA,])$multivariateNormality 
p2<-mvn(df[iB,])$multivariateNormality
rbind(p1,p2)
# 1 Henze-Zirkler 0.2750574 0.9186182 YES
# 2 Henze-Zirkler 0.7261392 0.2440743 YES

SA<-cov(df[iA,])
SB<-cov(df[iB,])
round(diag(SA/SB), 3)
#   lon   lat 
# 0.813 1.034 
# We can use LDA

nA <- length(iA)
nB <- length(iB)
n  <- nA + nB
pA <- nA / n
pB <- nB / n
priors<-c(pA,pB)

df.da <- lda(df, group, prior=priors)
df.da
# Group means:
#                     lon      lat
# nerinea       -159.5653 21.89723
# perisphinctes -159.4089 22.10339

SA<-cov(df[iA,])
SB<-cov(df[iB,])

SA
#             lon         lat
# lon 0.017434392 0.002295883
# lat 0.002295883 0.009846973
SB
#              lon          lat
# lon  0.021444520 -0.002625401
# lat -0.002625401  0.009527641

x  <- seq(min(df[,1]), max(df[,1]), length=200)
y  <- seq(min(df[,2]), max(df[,2]), length=200)
xy <- expand.grid(lon=x, lat=y) #change accordingly
z  <- predict(df.da, xy)$post  # P_i*f_i(x,y)  
# Uno z per ogni gruppo
z1 <- z[,1] - pmax(z[,2]) #, z[,3])  # P_1*f_1(x,y)-max{P_j*f_j(x,y)} #pmax Returns the (regular or parallel) maxima and minima of the input values.
z2 <- z[,2] - pmax(z[,1]) #, z[,3])  # P_2*f_2(x,y)-max{P_j*f_j(x,y)}  
plot(df, col=col.lab,main='', pch=19) # plot if p=1 or 2, pairs otherwise
# Un contour per ogni gruppo
# Plot the contour line of level (levels=0) of z1, z2, z3: 
#   P_i*f_i(x,y)-max{P_j*f_j(x,y)}=0 i.e., boundary between R.i and R.j 
#   where j realizes the max.
contour(x, y, matrix(z1, 200), levels=0, drawlabels=F, add=T)  
contour(x, y, matrix(z2, 200), levels=0, drawlabels=F, add=T)
# Group means
points(df.da$means, pch=4,col=c('red','blue') , lwd=2, cex=1.5)

legend("topright", legend=levels(group), fill=c('red','blue'), lty=c(1,1)) #only in plot


##########################################
# Point B
da.on.df <- predict(df.da, df) #I use my classifier to predict the training set
da.on.df
names(da.on.df)

# Nell'APER bisogna TASSATIVAMENTE usare i priors originali, non quelli corretti.
priors<-c(pA, pB)
G <- 2 #Number of groups
misc <- table(class.true=group, class.assigned=da.on.df$class)
APER <- 0
for(g in 1:G)
  APER <- APER + sum(misc[g,-g])/sum(misc[g,]) * priors[g]  #Metto il prior della riga al denominatore!
APER # [1] 0.08241758

daCV<- lda(df, group, CV=TRUE) 
errorsqCV <- (daCV$class != group)
AER_CV   <- sum(errorsqCV)/length(group)
AER_CV # [1] 0.08791209
# Very slightly higher, but very similar. Probably means
#   that we are not overfitting.


##########################################
# Point C
set.seed(9)

n_obs<- dim(df)[1] #n
k_range<- 5:20 #range of k to test, estremi inclusi

Aerr<-NULL
set.seed(19)  #run seed every time!
for (k in k_range) {
  df.knn <- knn.cv(train = df, cl = group, k = k)
  errorsqCV <- (df.knn != group)
  Aerr   <- c(Aerr,(sum(errorsqCV))/n_obs) #controllare che stiamo parlando di n_obs
}
min(Aerr) # [1] 0.08791209
best.index=which.min(Aerr)
kbest<-k_range[best.index]
kbest # [1] 20
AERkCV<-min(Aerr)
AERkCV # 0.08791209

x  <- seq(min(df[,1]), max(df[,1]), length=200)
y  <- seq(min(df[,2]), max(df[,2]), length=200)
xy <- expand.grid(lon=x, lat=y) #change accordingly
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

# Errors are equal, I will use KNN.
selected_k <- 20
new <- data.frame(lon = -159.5, lat= 21.9)    #change accordingly
knn(train = df, test = new, cl = group, k=selected_k)   
# nerinea