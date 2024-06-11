library(MASS)
library(MVN)

data <- read.table("pressure.txt", header=TRUE)
features <- data[,1:2]
result <- factor(data[,3])
plot(features, col=ifelse(result=="L","red","blue"), pch=19)

low <- features[which(result=="L"),]
high <- features[which(result=="H"),]

# LDA vs QDA
# normality assumption
mvn(low)$multivariateNormality
mvn(high)$multivariateNormality

# gaussianity not assumable for low population

S1 <- cov(low)
S2 <- cov(high)
S1/S2
# by the rule of thumb i would confirm the same covariance hypothesis
# but not from the plot

# i will proceed with QDA 
fit <- qda(features, result)
fit
# priors 0.5 and 0.5
# Group means:
#     x        y
# H 19.66933 55.62965
# L  9.68124 55.49661
# covariances for both grups are above

# Plot the partition induced by QDA
plot.partition.qda <- function() {
  plot(features, pch=20)
  points(low, col='red', pch=20)
  points(high, col='green', pch=20)
  legend("topright", legend=levels(result), fill=c('green','red'))
  
  points(fit$means, col=c("green","red"), pch=4, lwd=2, cex=1.5)
  
  x  <- seq(min(features[,1]), max(features[,1]), length=200)
  y  <- seq(min(features[,2]), max(features[,2]), length=200)
  xy <- expand.grid(x=x, y=y)
  
  z  <- predict(fit, xy)$post  
  z1 <- z[,1] - z[,2]   
  z2 <- z[,2] - z[,1]    

  contour(x, y, matrix(z1, 200), levels=0, drawlabels=F, add=T)
  contour(x, y, matrix(z2, 200), levels=0, drawlabels=F, add=T)
}
plot.partition.qda()

# the possible weakness is the not respected gaussianity assumptions.

qda.cv <- qda(features, result, CV=T)

table(class.true=result, class.assignedCV=qda.cv$class)

errorsqCV <- (qda.cv$class != result)
errorsqCV

AERqCV   <- sum(errorsqCV)/length(result)
AERqCV




library(class)
set.seed(19)
help(knn)
AERqCV <- NULL
for (k in 10:30){
  fit2 <- knn.cv(train=features, cl=result,k=k)
  errorsqCV <- (fit2 != result)
  
  AERqCV[k-9] <- sum(errorsqCV)/length(result)

}
AERqCV
min(AERqCV)
# for k=18
fit18 <- knn.cv(train=features, cl=result, k=18)

{
  k <- 18
  plot(features, main='Dataset', xlab='X1', ylab='X2', pch=20)
  points(low, col=2, pch=20)
  points(high, col=4, pch=20)

  x  <- seq(min(data[,1]), max(data[,1]), length=200)
  y  <- seq(min(data[,2]), max(data[,2]), length=200)
  xy <- expand.grid(feature1=x, feature2=y)
  
  data.knn <- knn(train = features,test=xy, cl = result, k = k)
  
  z  <- as.numeric(data.knn)
  
  contour(x, y, matrix(z, 200), levels=c(1.5, 2.5), drawlabels=F, add=T)
} # i have no idea dio cane

# according to APER the best is knn -> 0.195
newdata <- cbind(10.8,39.4)

predictions <- knn(train=features, test= newdata, cl=result, k=18)
predictions
# predicted as LOW.