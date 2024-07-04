library(MASS)
library(MVN)
library(class)
library(e1071)

# Univariate BINARY ---------------------------------------------------------------

data <- read.table("musicCountry.txt", header=TRUE)
head(data)

features <- data[,1:2]
groups <- data[,3]
groups <- factor(groups)
groups
group1 <- features[which(groups==levels(groups)[1]),]
group2 <- features[which(groups==levels(groups)[2]),]

priors = c(,)

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
library(heplots)
summary(boxM(features,groups))
# reject

# we can assume gaussianity but not same covariance -> QDA
fit <- qda(features,groups,prior=c(0.1,0.9))
fit

# plot of the classification region
# creates some fake point in the interval defined by the training data
x  <- seq(min(features[,1]), max(features[,1]), length=200)
y  <- seq(min(features[,2]), max(features[,2]), length=200)
# expand them to create a dataset  joining every possible combination of x and y
xy <- expand.grid(price=x, average.length=y)

# Plot the partition induced by LDA
plot.partition.lda <- function() {
  # plots the different point with different colours
  plot(features, main='Induced partition', xlab='X', ylab='Y', pch=20)
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


# LDA CV with GIVEN PRIORS
errors_CV <- 0
for(i in 1:150){
  QdaCV.i <- qda(features[-i,], groups[-i], prior=c(0.1,0.9))
  errors_CV <- errors_CV + as.numeric(predict(QdaCV.i,features[i,])$class != groups[i])
}
errors_CV

AERCV   <- sum(errors_CV)/length(groups)
AERCV

# QDA CV with R
predict.cv <- qda(features, groups, CV=TRUE,prior=c(0.1,0.9))
misc <- table(class.true=groups, class.assigned=predict.cv$class)

errors <- (predict.cv$class != groups)

AERCV   <- sum(errors)/length(groups)
AERCV

misc

TN <- misc[1, 1]
FN <- misc[2, 1]
FP <- misc[1, 2]
TP <- misc[2, 2]

# number of assigned positive over all negative times the prob to have a negative
# number of assigned positive over all positive times the prob to have a positive
# probability of estimating a US
(FP / (TN + FP)) * p.neg + (TP / (TP + FN)) * p.pos

newobs <- data.frame(price=50, average.length=3.5)

prediction <- predict(fit, newobs)
prediction


## SVM (Two classes)------------------------------------------------------
x <- features
y <- groups

dat <- data.frame(x=x, y=y)

tune.out <- tune(svm, y~., data=dat, kernel = 'linear',
                 ranges = list(cost=c(0.001 , 0.01, 0.1, 1,10,100) ))
# Extract the best model from the result of tune
bestmod <- tune.out$best.model
summary(bestmod)

par(mfrow=c(1,2))
plot(bestmod, dat, col =c('salmon', 'light blue'), pch=19,asp=0.1)

newobs <-cbind(50,3.5)
newobs <- data.frame(x=newobs)
colnames(newobs)[1:2] <- colnames(dat)[1:2] #colnames must be the same

prediction <- predict(bestmod, newobs)
prediction


