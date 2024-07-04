library(MASS)
library(MVN)

data <- read.table("fossil.txt", header=TRUE)
head(data)

features <- data[,1:2]
groups <- data[,3]
groups <- factor(groups)
groups
group1 <- features[which(groups==levels(groups)[1]),]
group2 <- features[which(groups==levels(groups)[2]),]

plot(features, col=ifelse(groups==levels(groups)[1],"red","blue"), pch=19)

# Verify gaussianity
mvn(group1)$multivariateNormality
mvn(group2)$multivariateNormality

# Verify covariance structure
cov1 <- cov(group1)
cov2 <- cov(group2)
cov1/cov2
# they are similar enough
# also visually

bartlett.test(features, groups)

# we can assume gaussianity and same covariance -> LDA
fit <- lda(features,groups)
fit

# plot of the classification region
# creates some fake point in the interval defined by the training data
x  <- seq(min(features[,1]), max(features[,1]), length=200)
y  <- seq(min(features[,2]), max(features[,2]), length=200)
# expand them to create a dataset  joining every possible combination of x and y
xy <- expand.grid(lon=x, lat=y)

# Plot the partition induced by LDA
plot.partition.lda <- function() {
  # plots the different point with different colours
  plot(features, main='Fossils', xlab='lon', ylab='lat', pch=20)
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
plot.partition.lda()

fit.predict <- predict(fit, features)
fit.predict$class   # assigned classes

table(class.true=groups, class.assigned=fit.predict$class)

errors <- (fit.predict$class != groups)
errors
sum(errors)
length(groups)

APER   <- sum(errors)/length(groups)
APER


predict.cv <- lda(features, groups, CV=TRUE)
table(class.true=groups, class.assigned=predict.cv$class)

errors <- (predict.cv$class != groups)
errors
sum(errors)
length(groups)

AERCV   <- sum(errors)/length(groups)
AERCV
# little higher

# k-nearest neighbour
library(class)
set.seed(9)
knnCV <- c()
for(k in 5:20){
  fit.knn <- knn.cv(train=features, cl=groups, k=k)
  table(class.true=groups, class.assigned=fit.knn)
  errors <- (fit.knn != groups)
  aercv <- sum(errors)/length(groups)
  knnCV <- rbind(knnCV,c(k,aercv))
}
knnCV
mink <- knnCV[which(knnCV[,2]==min(knnCV[,2])),1]
mink

bestknn <- knn.cv(train=features, cl=groups, k=mink[1])

{
  plot(features, main='Dataset', xlab='X1', ylab='X2', pch=20)
  points(group1, col="red", pch=20)
  points(group2, col=4, pch=20)
  
  x  <- seq(min(data[,1]), max(data[,1]), length=200)
  y  <- seq(min(data[,2]), max(data[,2]), length=200)
  xy <- expand.grid(feature1=x, feature2=y)
  
  dataknn <- knn(train=features,test=xy, cl=groups, k=mink[1])
  z  <- as.numeric(dataknn)
  
  contour(x, y, matrix(z, 200), levels=c(1.5, 2.5), drawlabels=F, add=T)
}

points(newobs,col="black")
# i would classify it as group1 -> nerinea
