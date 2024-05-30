setwd("C:/Users/Giuseppe/Documents/.magistrale/secondo anno/AppStat/Exams/2022/20220616/20220616")
data <- read.table("musicCountry.txt",header=T)
dim(data)
library(MASS)
library(class)
library(rgl)
library(mvtnorm)
library(MVN)


colnames(data) <- c("feature1","feature2","group")


groups.name <- factor(data$group)
g = length(levels(groups.name)) 
g
p <- dim(data)[2] - 1 # no categorical variable

i1 <- which(groups.name == levels(groups.name)[1])
i2 <- which(groups.name == levels(groups.name)[2])


n1 <- length(i1)
n2 <- length(i2)
n <- n1 + n2

prior= c(0.1,0.9)

data.feats <- data[,1:p] # in this case features are before
# Estimates and MANOVA to see if there is some difference in the groups
{
  m <-  colMeans(data[,1:p])
  m1 <- colMeans(data[i1, 1:p])
  m2 <- colMeans(data[i2, 1:p])
  
  S1 <- cov(data[i1, 1:p])
  S2 <- cov(data[i2, 1:p])
  Sp  <- ((n1 - 1) * S1 + (n2 - 1) * S2 ) / (n - g)
  
  # One-way MANOVA 
  fit <- manova(as.matrix(data.feats) ~ groups.name)
  summary.manova(fit, test="Wilks")
  
} #low pval -> there is difference in groups

# Assumptions
{
  # multivariate normality in each group
  mvn(data[which(data$group == levels(groups.name)[1]), 1:dim(data.feats)[2]])$multivariateNormality
  mvn(data[which(data$group == levels(groups.name)[2]), 1:dim(data.feats)[2]])$multivariateNormality

  
  # var.test is used instead of bartlett.test when there are only 2 groups
  # var.test(data.reduced[A,1], data.reduced[B,1])
  bartlett.test(data.feats[i1,], data.feats[i2,])
  
  # Quantitatively, boxplot
  {
    par(mfrow=c(1,length(levels(groups.name))))
    boxplot(scale(data[which(data$group == levels(groups.name)[1]),1:p], center = T, scale = F))
    boxplot(scale(data[which(data$group == levels(groups.name)[2]),1:p], center = T, scale = F))
  }
  
  # Quantitatively, images of S
  {
    S1 <- cov(data.feats[i1,])
    S2 <- cov(data.feats[i2,])
    
    par(mfrow=c(1,2))
    image(S1)
    image(S2)
  }
}
# QDA, isnce variances are very different

qda.data <- qda(data.feats, groups.name,
                prior=prior
)
qda.data
# Estimate of parameters
{
  # Means of parameters
  qda.data$means
  
  # Covariance matrix 
  cov(data[which(data$group == 'US'), 1:2])
  cov(data[which(data$group == 'Germany'), 1:2])
}


# Plot the 2D partition induced by QDA
{
  plot(data.feats, main='dataset', xlab='X1', ylab='X2', pch=20)
  points(data.feats[i1,], col='red', pch=20)
  points(data.feats[i2,], col='green', pch=20)
  legend("topright", legend=levels(groups.name), fill=c('red','green','blue'))
  
  points(qda.data$means, col=c('red','green','blue'), pch=4, lwd=2, cex=1.5)
  
  x  <- seq(min(data.feats[,1]), max(data.feats[,1]), length=200)
  y  <- seq(min(data.feats[,2]), max(data.feats[,2]), length=200)
  
  # rename this, don't use feature1 feature2, but the true name for features
  xy <- expand.grid(feature1=x, feature2=y)
  

  # if g = 2
  {
    z  <- predict(qda.data, xy)$post  
    z1 <- z[,1] - pmax(z[,2])    
    z2 <- z[,2] - pmax(z[,1])    
  }
  
  contour(x, y, matrix(z1, 200), levels=0, drawlabels=F, add=T)
  contour(x, y, matrix(z2, 200), levels=0, drawlabels=F, add=T)
}

qda.data

# b
# with priors
# always use the priors without the cost information!!!
{
  # Leave One Out CV: specify priors accordingly in the for loop!
  errors_CV <- 0
  for(i in 1:dim(data)[1]){
    QdaCV.i <- qda(data.feats[-i,], groups.name[-i], prior=prior)
    if
    penalize = 
    as.numeric(predict(QdaCV.i,data.feats[i,])$class != groups.name[i])
    errors_CV <- errors_CV + as.numeric(predict(QdaCV.i,data.feats[i,])$class != groups.name[i])
  }
  AERCV   <- sum(errors_CV)/length(groups.name)
  AERCV # typically higher than APER, more accurate
}



ldaCV <- qda(data.feats[,1:2], data$group, CV=TRUE, prior = prior)  # specify the argument CV
misc <- table(class.true=data$group, class.assignedCV=ldaCV$class)
AERCV  <- misc[1,2]*prior[1]/sum(misc[1,]) + misc[2,1]*prior[2]/sum(misc[2,])
AERCV

# c)
# Posterior: estimated probability for a new entry
x <- data.frame(seq(min(data.feats[,1]), max(data.feats[,1]),100),
                seq(min(data.feats[,2]), max(data.feats[,2]),100))
arr <- predict(qda.data, xy, prior=prior)$posterior[,2]
int = arr
for (i in 1:length(arr)){
  if (arr[i] > 0.5)
    int[i] = 1
  else
    int[i] = 0
}
mean(int)


# d)
# Predict new data
{
  Z0 <- data.frame(feature1 =50,feature2=3.5)
  predict(qda.data, Z0)
  # classify US with 92.31 %
}


# SVM 
set.seed(1)
library(e1071)
par(mfrow=c(1,2))
tune.out <- tune(svm, as.factor(group)~.,data=data,kernel = 'linear',
                 ranges =list(cost=c(0.001 , 0.01, 0.1, 1, 10,100) ))
summary(tune.out)

# Extract the best model from the result of tune
bestmod <- tune.out$best.model
summary(bestmod)

# SVM classification plot
par(mfrow=c(1,2))
plot(bestmod , data, col =c('salmon', 'light blue'), pch=19)
