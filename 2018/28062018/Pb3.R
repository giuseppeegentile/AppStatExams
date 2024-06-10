setwd("C:/Users/Giuseppe/Documents/.magistrale/secondo anno/AppStat/Exams/2018/20180628/20180628")


library(MASS)
library(class)
library(rgl)
library(mvtnorm)
library(MVN)
library(e1071)
library(heplots)

options(rgl.printRglwidget = TRUE)
data <- read.table("Precolombian.txt",header=T)
head(data)

levels(factor(data$Civilization))


{
  #prior probabilities
  p1 = 20/100                                                    
  p2 = 10/100
  p3 = 70/100
  
  prior = c(p1, p2, p3)
  prior
}
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
par(mfrow=c(1,1))
plot(data[,1:2], pch=19, col=factor(data$group))
# variances seems very different, also, non linear boundaries between classes

# Assumptions
{
  # multivariate normality in each group
  mvn(data[which(data$group == levels(groups.name)[1]), 1:dim(data.feats)[2]])$multivariateNormality
  mvn(data[which(data$group == levels(groups.name)[2]), 1:dim(data.feats)[2]])$multivariateNormality
  mvn(data[which(data$group == levels(groups.name)[3]), 1:dim(data.feats)[2]])$multivariateNormality
  
  # if there is normality
  boxM(data.feats, groups.name)
  
  # we reject with boXM (we have normality too) -> variances are different
  # Qualitatively, boxplot
  {
    par(mfrow=c(1,length(levels(groups.name))))
    boxplot(scale(data[which(data$group == levels(groups.name)[1]),1:p], center = T, scale = F))
    boxplot(scale(data[which(data$group == levels(groups.name)[2]),1:p], center = T, scale = F))
    boxplot(scale(data[which(data$group == levels(groups.name)[3]),1:p], center = T, scale = F))
    par(mfrow=c(1,1))
  }
  
  # Qualitatively,S
  {
    S1 <- cov(data.feats[i1,])
    S2 <- cov(data.feats[i2,])
    S3 <- cov(data.feats[i3,])
    S1
    S2
    S3
    
    96049.38038/791 > 10
    
    # heat map of covariances
    par(mfrow=c(1,g))
    image(S1, col=heat.colors(100),main='Cov. S1', asp=1, axes = FALSE, breaks = quantile(rbind(S1,S2,S3), (0:100)/100, na.rm=TRUE))
    image(S2, col=heat.colors(100),main='Cov. S2', asp=1, axes = FALSE, breaks = quantile(rbind(S1,S2,S3), (0:100)/100, na.rm=TRUE))
    image(S3, col=heat.colors(100),main='Cov. S2', asp=1, axes = FALSE, breaks = quantile(rbind(S1,S2,S3), (0:100)/100, na.rm=TRUE))
    par(mfrow=c(1,1))
  }
}


qda.data <- qda(data.feats, groups.name
                , prior=prior
)
qda.data


# Group means:
#   feature1 feature2
# Aztec  1431.6053 1.947632
# Maya    765.9425 1.201034
# Toltec 1002.9762 2.008333


# Plot the 2D partition induced by QDA
{
  plot(data.feats, main='dataset', xlab='X1', ylab='X2', pch=20)
  points(data.feats[i1,], col='red', pch=20)
  points(data.feats[i2,], col='green', pch=20)
  points(data.feats[i3,], col='blue', pch=20)
  legend("topright", legend=levels(groups.name), fill=c('red','green','blue'))
  
  points(qda.data$means, col=c('red','green','blue'), pch=4, lwd=2, cex=1.5)
  
  x  <- seq(min(data.feats[,1]), max(data.feats[,1]), length=200)
  y  <- seq(min(data.feats[,2]), max(data.feats[,2]), length=200)
  
  # rename this, don't use feature1 feature2, but the true name for features
  xy <- expand.grid(feature1=x, feature2=y)
  
  # if g = 3
  {
    z  <- predict(qda.data, xy)$post  
    z1 <- z[,1] - pmax(z[,2], z[,3])    
    z2 <- z[,2] - pmax(z[,1], z[,3])    
    z3 <- z[,3] - pmax(z[,1], z[,2])
  }
  # Plot the contour line of level (levels=0) of z1, z2, z3: 
  # P_i*f_i(x,y)-max{P_j*f_j(x,y)}=0 i.e., boundary between R.i and R.j 
  contour(x, y, matrix(z1, 200), levels=0, drawlabels=F, add=T)
  contour(x, y, matrix(z2, 200), levels=0, drawlabels=F, add=T)
  contour(x, y, matrix(z3, 200), levels=0, drawlabels=F, add=T)
}

{
  inference_train <- predict(qda.data)
  G <- g
  misc <- table(class.true=groups.name, class.assigned=inference_train$class)
  APER <- 0
  for(g in 1:G)
    APER <- APER + sum(misc[g,-g])/sum(misc[g,]) * prior[g]  
  APER
  #0.0137931
}


# Predict on new entry
{
  Z0 <- data.frame(feature1=986, feature2=1.4)
  predict(qda.data, Z0) # -> toltec with prob 75.597%
}







  
