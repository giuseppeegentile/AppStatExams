setwd("C:/Users/Giuseppe/Documents/.magistrale/secondo anno/AppStat/Exams/2019/20190628/20190628")
library(car)
library(MVN)


data <- read.table("profiling.txt",header=T)
plot(data[,1:2],pch=19,col=as.factor(data[,3]))
p <- dim(data)[2] - 1 # no categorical variable
par(mfrow=c(1,1))
plot(data[,1:p],pch=19, col=factor(data[,3]))
head(data)
colnames(data) <- c("feature1","feature2","group")


groups.name <- factor(data$group)
g = length(levels(groups.name)) 
g
# Do accordingly to how many g
i1 <- which(groups.name == levels(groups.name)[1])
i2 <- which(groups.name == levels(groups.name)[2])

# Do accordingly to how many g
n1 <- length(i1)
n2 <- length(i2)

n <- n1 + n2 

data.feats <- data[,1:p]


# Assumptions
{
  # multivariate normality in each group
  mvn(data[which(data$group == levels(groups.name)[1]), 1:dim(data.feats)[2]])$multivariateNormality
  mvn(data[which(data$group == levels(groups.name)[2]), 1:dim(data.feats)[2]])$multivariateNormality
  
  
  # if there is normality
  boxM(data.feats, groups.name)
  #we reject with boxM
  # if no normality: the ratio of the S1's element wise on diagonal must be < 4
  #                  out of diagonal < 10
  
  # Qualitatively, boxplot
  {
    par(mfrow=c(1,length(levels(groups.name))))
    boxplot(scale(data[which(data$group == levels(groups.name)[1]),1:p], center = T, scale = F))
    boxplot(scale(data[which(data$group == levels(groups.name)[2]),1:p], center = T, scale = F))
    par(mfrow=c(1,1))
  }
  
  # Qualitatively,S
  {
    S1 <- cov(data.feats[i1,])
    S2 <- cov(data.feats[i2,])
    S1
    S2
    
    # heat map of covariances
    par(mfrow=c(1,g))
    image(S1, col=heat.colors(100),main='Cov. S1', asp=1, axes = FALSE, breaks = quantile(rbind(S1,S2), (0:100)/100, na.rm=TRUE))
    image(S2, col=heat.colors(100),main='Cov. S2', asp=1, axes = FALSE, breaks = quantile(rbind(S1,S2), (0:100)/100, na.rm=TRUE))
    par(mfrow=c(1,1))
  }
}

# I'd use QDA in this case: 
#Variances are different (not so much byt we reject with boxM), we have lots of data n=1000 >> p = 2
# there are points of the other group on the left, so we need Quadratic shapes for them






qda.data <- qda(data.feats, groups.name,
                # add priors if given
) 
qda.data

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



# usual stuff



