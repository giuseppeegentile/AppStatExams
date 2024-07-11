library(MASS)
library(MVN)
library(class)
library(e1071)
library(heplots)

data <- read.table("fish.txt", header=TRUE)
head(data)

features <- data[,1:2]
groups <- data[,3]
groups <- factor(groups)
groups
g <- length(levels(groups))
group1 <- features[which(groups==levels(groups)[1]),]
group2 <- features[which(groups==levels(groups)[2]),]
n1 <-length(group1)
n2 <-length(group2)
n <- n1+n2
m <- colMeans(features)
m1 <- colMeans(group1)
m2 <- colMeans(group2)
S1 <- cov(group1)
S2 <- cov(group2)
Sp <- ((n1-1)*S1 + (n2-1)*S2)/(n-g)


plot(features, col=ifelse(groups==levels(groups)[1],"red","blue"), pch=19)

mvn(group1)$multivariateNormality
mvn(group2)$multivariateNormality

# Verify covariance structure
cov1 <- cov(group1)
cov2 <- cov(group2)
cov2/cov1
# they are similar enough
# also visually

# we don't have gaussianity for one group and have a different covariance structure (from visual assess)
# i would use QDA
fit <- qda(features,groups)
fit

# plot of the classification region
# creates some fake point in the interval defined by the training data
x  <- seq(min(features[,1]), max(features[,1]), length=200)
y  <- seq(min(features[,2]), max(features[,2]), length=200)
# expand them to create a dataset  joining every possible combination of x and y
xy <- expand.grid(x=x, y=y)

# Plot the partition induced by qda
plot.partition.qda <- function() {
  # plots the different point with different colours
  plot(features, main='FIshing areas', xlab='lon', ylab='lat', pch=20)
  points(group1,col='red', pch=20)
  points(group2, col='green', pch=20)
  legend("topright", legend=levels(groups), fill=c('red','green'), cex=.7)
  # add the means
  points(fit$means, pch=4,col=c('red','green') , lwd=2, cex=1.5)
  
  # predicts all the fictious point and calculates the posterior
  # prob of being in class i given that you are x
  z  <- predict(fit, xy)$post  # these are P_i*f_i(x,y)  
  # finds the 3 boundaries
  z1 <- z[,1] - z[,2] # P_1*f_1(x,y)-max{P_j*f_j(x,y)}  
  # Plot the contour line of level (levels=0) of z1, z2, z3: 
  # P_i*f_i(x,y)-max{P_j*f_j(x,y)}=0 i.e., boundary between R.i and R.j 
  # where j realizes the max.
  contour(x, y, matrix(z1, 200), levels=0, drawlabels=F, add=T)  
}
plot.partition.qda()


fit$prior
#estimates of parameters
cov(group1)
cov(group2)
fit$means

# the gaussianity assumption does not really hold and for QDA we do not have fisher argument.

# LOO CV AER estimation
predict.cv <- qda(features, groups, CV=TRUE)
table(class.true=groups, class.assigned=predict.cv$class) # apply the given prior connection

errors <- (predict.cv$class != groups)

AERCV   <- sum(errors)/length(groups)
AERCV

# knn
set.seed(19)
knnCVs <- c()
fits <- c()
# find best k in a range
for(k in 10:30){
  fit.knn <- knn.cv(train=features, cl=groups, k=k)
  table(class.true=groups, class.assigned=fit.knn)
  errors <- (fit.knn != groups)
  aercv <- sum(errors)/length(groups)
  knnCVs <- rbind(knnCVs,c(k,aercv))
  fits <- cbind(fits, fit.knn)
}
knnCVs
mink <- knnCVs[which(knnCVs[,2]==min(knnCVs[,2])),1]
bestError <- knnCVs[which(knnCVs[,2]==min(knnCVs[,2])),2]
bestmodelindex <- which(knnCVs[,2]==min(knnCVs[,2]))
bestError

{
  plot(features, main='Dataset', xlab='X1', ylab='X2', pch=20)
  points(group1, col="red", pch=20)
  points(group2, col=4, pch=20)
  
  x  <- seq(min(data[,1]), max(data[,1]), length=200)
  y  <- seq(min(data[,2]), max(data[,2]), length=200)
  xy <- expand.grid(feature1=x, feature2=y)
  # a knn just to plot the contour
  dataknn <- knn(train=features,test=xy, cl=groups, k=mink[1])
  z  <- as.numeric(dataknn)
  
  contour(x, y, matrix(z, 200), levels=c(1.5, 2.5), drawlabels=F, add=T)
}

# newobs
newobs <- data.frame(x=10.8,y=39.4)
points(newobs,col="black")
#knn classify it as HIGH 

prediction <- predict(fit, newobs)
prediction
# qda as low









































































































