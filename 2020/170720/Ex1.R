# Marco Scarpelli

# Exam 17/07/2020

# Exercise 1

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

rm(list=ls())
graphics.off()

df <- read.table('occupancy.txt', header=T)

head(df)

group<-factor(df$X)

# Get class names
class.A<- levels(group)[1] # name of class A
class.B<- levels(group)[2] # name of class B
# Get indices of the classes
iA <- which(df$X==levels(group)[1])  
iB <- which(df$X==levels(group)[2])  

df<-df[,1:2] # keep the quantitative columns
head(df) # 'nameofgroupcolumn'

n<-dim(df)[1]
p<-dim(df)[2]

head(df)

col.lab<-rep(0,n)
col.lab[iA]<-'red'
col.lab[iB]<-'blue'

plot(df, col=col.lab, main='', pch=19) 
# plot if p=1 or 2, pairs otherwise
legend("topright", legend=levels(group), fill=c('red','blue'), lty=c(1,1)) #only in plot

##########################################
# Point A

# Multivariate normality:
p1<-mvn(df[iA,])$multivariateNormality 
p2<-mvn(df[iB,])$multivariateNormality
rbind(p1,p2)

# Both tests succeed with p-values around 0.5

# Covariance structure
SA<-cov(df[iA,])
SB<-cov(df[iB,])
SA
SB

# The highest ratio between diagonal elements is 3.36
#   which is below 4, so we can use LDA.


# We know that a room is occupied (i.e. in class B)
#   9 hours over the day.
pA<-1-9/24
pB<-9/24
priors<-c(pA,pB)
priors

df.da <- lda(df, group, prior=priors)
# Report parameters
df.da

# Group means:
#   Humidity      CO2
# 0 24.53469 5.128682
# 1 26.36389 9.578757
# 
# Coefficients of linear discriminants:
#   LD1
# Humidity 0.004837008
# CO2      0.610382181

#da.on.df  <- predict(df.da, df)

names(df) # Prendere da qui i nomi delle colonne

x  <- seq(min(df[,1]), max(df[,1]), length=200)
y  <- seq(min(df[,2]), max(df[,2]), length=200)
xy <- expand.grid(Humidity=x, CO2=y) #change accordingly

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
# Medie
points(df.da$means, pch=4,col=c('red','blue') , lwd=2, cex=1.5)

legend("topright", legend=levels(group), fill=c('red','blue'), lty=c(1,1)) #only in plot

# Possible weaknesses: the model, being linear, fails to
#   account for the roundness of the cluster below.
# QDA could have avoided some misclassified points 
#   near the boundary. We used LDA because we found that
#   the covariance structures were similar enough, but
#   they are probably an edge case; I think in this case
#   switching to QDA should be OK.


##########################################
# Point B
da.on.df <- predict(df.da, df) # I use my classifier to predict the training set

#Per APER vanno considerati i costi o no? direi di NO, quindi usare priors normali, NON aggiustati per i costi!
errors<-sum(da.on.df$class!=group)
# per esempio sum(Lda.iris$class != species.name)
APER<-errors/length(group)
APER # 0.1

##########################################
# Point C
s0.new<- data.frame(Humidity = 26.0, CO2= 9) #change if necessary
predict(df.da, s0.new) # Predizione: 1 (blu), dal grafico sembra OK
points(s0.new[1],s0.new[2], col='green', pch=4,lwd=2, cex=1.5)

##########################################
# Point D
k <- 5
df.knn <- knn(train = df, test = df, cl = group, k = k)
misc<-table(df.knn, group)
misc
#         group
# df.knn  0  1
#       0 38  4
#       1  2 56


errors<-df.knn!=group 


#AERknn: no priors
APERknn<-sum(errors)/n
APERknn # 0.06

x  <- seq(min(df[,1]), max(df[,1]), length=200)
y  <- seq(min(df[,2]), max(df[,2]), length=200)
xy <- expand.grid(Humidity=x, CO2=y) #change accordingly
names(df)

# Remember to use the best k if you have it
df.knn <- knn(train = df, test = xy, cl = group, k = k)
z  <- as.numeric(df.knn)


plot(df, main='', pch=20)
points(df[iA,], col='red', pch=19)
points(df[iB,], col='blue', pch=19)
legend("topright", legend=c(levels(group)), fill=c('red','blue'))
contour(x, y, matrix(z, 200), levels=c(1.5, 2.5), drawlabels=F, add=T) #change if more than 2 levels

# The APER of this classifier is marginally better
#   than the LDA's (0.06 vs. 0.1) and the classification
#   regions are similar.