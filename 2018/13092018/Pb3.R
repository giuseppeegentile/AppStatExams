setwd("C:/Users/Giuseppe/Documents/.magistrale/secondo anno/AppStat/Exams/2018/20180913/20180913")
library(MASS)
library(class)
library(rgl)
library(mvtnorm)
library(MVN)
library(e1071)
library(heplots)

options(rgl.printRglwidget = TRUE)
data <- read.table("Sailing.txt",header=T)
head(data)
factor(data$type)
prior <- c(0.2, 0.8)

p <- dim(data)[2] - 1 # no categorical variable
par(mfrow=c(1,1))
plot(data[,1:p],pch=19)
head(data)
colnames(data) <- c("feature1","feature2","group")


groups.name <- factor(data$group)
g = length(levels(groups.name)) 
g
# Do accordingly to how many g
i1 <- which(groups.name == levels(groups.name)[1])
i2 <- which(groups.name == levels(groups.name)[2])
i3 <- which(groups.name == levels(groups.name)[3])

# Do accordingly to how many g
n1 <- length(i1)
n2 <- length(i2)
n3 <- length(i3)
n <- n1 + n2 + n3

data.feats <- data[,1:p] # in this case features are before




lda.data <- lda(data.feats, groups.name, prior=prior
) 
lda.data

# Plot the 2d partition induced by LDA
{
  x  <- seq(min(data.feats[,1]), max(data.feats[,1]), length=200)
  y  <- seq(min(data.feats[,2]), max(data.feats[,2]), length=200)
  
  # rename this, don't use feature1 and feature2, but the true name
  xy <- expand.grid(feature1=x, feature2=y)
  
  plot(data.feats, main='Dataset', xlab='X1', ylab='X2', pch=20)
  points(data.feats[i1,], col='red', pch=20)
  points(data.feats[i2,], col='green', pch=20)
  legend("topright", legend=levels(groups.name), fill=c('red','green','blue'), cex=.7)
  
  points(lda.data$means, pch=4,col=c('red','green','blue') , lwd=2, cex=1.5)
  
  
  # if g =2
  {
    
    z  <- predict(lda.data, xy)$post  # these are P_i*f_i(x,y)  
    z1 <- z[,1] - pmax(z[,2])  # P_1*f_1(x,y)-max{P_j*f_j(x,y)}  
    z2 <- z[,2] - pmax(z[,1])  # P_2*f_2(x,y)-max{P_j*f_j(x,y)}   
  }
  
  
  # Plot the contour line of level (levels=0) of z1, z2, z3: 
  # P_i*f_i(x,y)-max{P_j*f_j(x,y)}=0 i.e., boundary between R.i and R.j 
  # where j realizes the max.
  contour(x, y, matrix(z1, 200), levels=0, drawlabels=F, add=T)  
  contour(x, y, matrix(z2, 200), levels=0, drawlabels=F, add=T)
  contour(x, y, matrix(z3, 200), levels=0, drawlabels=F, add=T)
  
}


predict(lda.data, data[1,1:2])
#posterior 0.95565   0.044347

qda.data <- qda(data.feats, groups.name, prior=prior
) 
qda.data
predict(qda.data, data[1,1:2]) #0.96475   0.035253





# Plot the 2d partition induced by LDA
{
  x  <- seq(min(data.feats[,1]), max(data.feats[,1]), length=200)
  y  <- seq(min(data.feats[,2]), max(data.feats[,2]), length=200)
  
  # rename this, don't use feature1 and feature2, but the true name
  xy <- expand.grid(feature1=x, feature2=y)
  
  plot(data.feats, main='Dataset', xlab='X1', ylab='X2', pch=20)
  points(data.feats[i1,], col='red', pch=20)
  points(data.feats[i2,], col='green', pch=20)
  legend("topright", legend=levels(groups.name), fill=c('red','green','blue'), cex=.7)
  
  points(qda.data$means, pch=4,col=c('red','green','blue') , lwd=2, cex=1.5)
  
  
  # if g =2
  {
    
    z  <- predict(qda.data, xy)$post  # these are P_i*f_i(x,y)  
    z1 <- z[,1] - pmax(z[,2])  # P_1*f_1(x,y)-max{P_j*f_j(x,y)}  
    z2 <- z[,2] - pmax(z[,1])  # P_2*f_2(x,y)-max{P_j*f_j(x,y)}   
  }
  
  
  # Plot the contour line of level (levels=0) of z1, z2, z3: 
  # P_i*f_i(x,y)-max{P_j*f_j(x,y)}=0 i.e., boundary between R.i and R.j 
  # where j realizes the max.
  contour(x, y, matrix(z1, 200), levels=0, drawlabels=F, add=T)  
  contour(x, y, matrix(z2, 200), levels=0, drawlabels=F, add=T)
  contour(x, y, matrix(z3, 200), levels=0, drawlabels=F, add=T)
  
}



# AERCV of A
# With custom priors
{
  LdaCV <- lda(data.feats, groups.name, CV=TRUE, prior = prior)  # specify the argument CV
  misc <- table(class.true=groups.name, class.assignedCV=LdaCV$class)
  AERCV  <- misc[1,2]*prior[1]/sum(misc[1,]) + misc[2,1]*prior[2]/sum(misc[2,])
  AERCV #0.063333
}

# With custom priors
{
  QdaCV <- qda(data.feats, groups.name, CV=TRUE, prior = prior)  # specify the argument CV
  misc <- table(class.true=groups.name, class.assignedCV=QdaCV$class)
  AERCV  <- misc[1,2]*prior[1]/sum(misc[1,]) + misc[2,1]*prior[2]/sum(misc[2,])
  AERCV # 0.063333
}

# they are equal, because the two classes are separable
# i choose LDA because has fewer parameters and better generalization


z0 <- data.frame(feature1 = 32.08, feature2 = 82.69)
predict(lda.data, z0)











