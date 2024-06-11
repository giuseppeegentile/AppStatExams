setwd("C:/Users/Giuseppe/Documents/.magistrale/secondo anno/AppStat/Exams/2017/20170718/20170718")

library(MASS)
library(class)
library(rgl)
library(mvtnorm)
library(MVN)
library(e1071)
library(heplots)

options(rgl.printRglwidget = TRUE)
data <- read.table("horsecolic.txt",header=T)
head(data)


data1 <- data[which(data$Pain == "Yes"), 1:4]
data2 <- data[which(data$Pain == "No"), 1:4]
n1 <- dim(data1)[1]
n2 <- dim(data2)[1]
n  = n1+n2
p <- dim(data2)[2]


data1.mean <- sapply(data1, mean)
data2.mean <- sapply(data2, mean)

data1.cov <- cov(data1)
data2.cov <- cov(data2)
Sp        <- ((n1 - 1) * data1.cov + (n2 - 1) * data2.cov) / (n1 + n2 - 2)

# We compare the matrices when n1 and n2 are small -> 
# rule of thumb:
#    don't reject equality of covariance if s1_ii and s2_ii differ less than 4
# Do this only if n1 and n2 are small:
list(S1 = data1.cov, S2 = data2.cov, Spooled = Sp)


abs(data1.cov)/abs(data2.cov)
plot(data[,1:4],col=factor(data$Pain),pch=19) # covariance seems somehow comparable

# We have normality -> 
mvn(data=data1)$multivariateNormality
mvn(data=data2)$multivariateNormality


# Bonferroni for the difference of the means
{
  k <- 4
  alpha= 0.01
  cfr.t <- qt(1-alpha/(2*k),n1+n2-2)
  Bf1 <- cbind(inf    = data1.mean-data2.mean - cfr.t*sqrt(diag(Sp)*(1/n1+1/n2)),
               center = data1.mean-data2.mean,
               sup    = data1.mean-data2.mean + cfr.t*sqrt(diag(Sp)*(1/n1+1/n2)))
  Bf1
} 
# only pulse and respiratory rate are significatly different between orses with pain and no pain

data <- data[,c(2,3,5)]
head(data)

p <- dim(data)[2] - 1 # no categorical variable
par(mfrow=c(1,1))
plot(data[,1:p],pch=19,col=factor(data$Pain))
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

data.feats <- data[,1:p] # in this case features are before





# Assumptions
{
  # multivariate normality in each group
  mvn(data[which(data$group == levels(groups.name)[1]), 1:dim(data.feats)[2]])$multivariateNormality
  mvn(data[which(data$group == levels(groups.name)[2]), 1:dim(data.feats)[2]])$multivariateNormality
  
  # if there is normality
  boxM(data.feats, groups.name)
  
  # reject -> use QDA
  

  
  # Qualitatively,S
  {
    S1 <- cov(data.feats[i1,])
    S2 <- cov(data.feats[i2,])
    
    S1
    S2
    
    S1/S2
    
    # heat map of covariances
    par(mfrow=c(1,g))
    image(S1, col=heat.colors(100),main='Cov. S1', asp=1, axes = FALSE, breaks = quantile(rbind(S1,S2), (0:100)/100, na.rm=TRUE))
    image(S2, col=heat.colors(100),main='Cov. S2', asp=1, axes = FALSE, breaks = quantile(rbind(S1,S2), (0:100)/100, na.rm=TRUE))
    image(S3, col=heat.colors(100),main='Cov. S2', asp=1, axes = FALSE, breaks = quantile(rbind(S1,S2), (0:100)/100, na.rm=TRUE))
    par(mfrow=c(1,1))
  }
}
par(mfrow=c(1,1))



lda.data <- lda(data.feats, groups.name,
                # add priors if given
) 
lda.data

{
  LdaCV.R <- lda(data.feats, groups.name, CV=TRUE)
  misc <- table(class.true=groups.name, class.assignedCV=LdaCV.R$class)
  errorsCV <- (LdaCV.R$class != groups.name)
  AERCV   <- sum(errorsCV)/length(groups.name)
  AERCV
}


qda.data <- qda(data.feats, groups.name,
                # add priors if given
) 
qda.data


#       No       Yes 
# 0.7366667 0.2633333 

# Group means:
#     feature1 feature2
# No  61.23738 41.69032
# Yes 97.86899 61.50570

{
  LdaCV.R <- qda(data.feats, groups.name, CV=TRUE)
  misc <- table(class.true=groups.name, class.assignedCV=LdaCV.R$class)
  errorsCV <- (LdaCV.R$class != groups.name)
  AERCV   <- sum(errorsCV)/length(groups.name)
  AERCV
}

# -> use QDA


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
    z  <- predict(qda.data, xy)$post  # these are P_i*f_i(x,y)  
    z1 <- z[,1] - pmax(z[,2])     # P_1*f_1(x,y)-max{P_j*f_j(x,y)}  
    z2 <- z[,2] - pmax(z[,1])    # P_2*f_2(x,y)-max{P_j*f_j(x,y)}    
  }
  # Plot the contour line of level (levels=0) of z1, z2, z3: 
  # P_i*f_i(x,y)-max{P_j*f_j(x,y)}=0 i.e., boundary between R.i and R.j 
  contour(x, y, matrix(z1, 200), levels=0, drawlabels=F, add=T)
  contour(x, y, matrix(z2, 200), levels=0, drawlabels=F, add=T)
  contour(x, y, matrix(z3, 200), levels=0, drawlabels=F, add=T)
}



# APER
{
  # with default priors 
  {
    inference_train <- predict(qda.data)
    misc <- table(class.true=groups.name, class.assigned=inference_train$class)
    
    errorsq <- (inference_train$class != groups.name)
    APERq   <- sum(errorsq)/length(groups.name)
    APERq
  }
}







