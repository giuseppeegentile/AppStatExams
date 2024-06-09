setwd("C:/Users/Giuseppe/Documents/.magistrale/secondo anno/AppStat/Exams/2019/20191121/20191121")

library(MASS)
library(class)
library(rgl)
library(mvtnorm)
library(MVN)
library(e1071)
library(heplots)



data <- read.table("sansiro.txt",header=T)
head(data)



p_suspended =  0.08
p_cancelled =  0.03
p_regularly =  1 -(0.03+0.08)


prior = c(p_cancelled, p_regularly, p_suspended)
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

data.feats <- data[,1:p] 

plot(data[,1:2], pch=19, col=factor(data$group))

# Assumptions
{
  # multivariate normality in each group
  mvn(data[which(data$group == levels(groups.name)[1]), 1:dim(data.feats)[2]])$multivariateNormality
  mvn(data[which(data$group == levels(groups.name)[2]), 1:dim(data.feats)[2]])$multivariateNormality
  mvn(data[which(data$group == levels(groups.name)[3]), 1:dim(data.feats)[2]])$multivariateNormality
  # third group not gaussian at 1%, but LDA/QDa are robust to gass assumption
  
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
    
    # heat map of covariances
    par(mfrow=c(1,g))
    image(S1, col=heat.colors(100),main='Cov. S1', asp=1, axes = FALSE, breaks = quantile(rbind(S1,S2,S3), (0:100)/100, na.rm=TRUE))
    image(S2, col=heat.colors(100),main='Cov. S2', asp=1, axes = FALSE, breaks = quantile(rbind(S1,S2,S3), (0:100)/100, na.rm=TRUE))
    image(S3, col=heat.colors(100),main='Cov. S2', asp=1, axes = FALSE, breaks = quantile(rbind(S1,S2,S3), (0:100)/100, na.rm=TRUE))
    par(mfrow=c(1,1))
  }
  # they're not so different, we also have few data (15 per class) and boundaries seems linear 
  # -> we use LDA
}




lda.data <- lda(data.feats, groups.name,
                prior=prior
) 
lda.data





# Estimate of parameters
{
  # Means of parameters
  lda.data$means
  
  # Covariance matrix estimated with pooled
  S1 <- cov(data[which(data$group == groups.name[1]), 1:p])
  S2 <- cov(data[which(data$group == groups.name[2]), 1:p])
  S3 <- cov(data[which(data$group == groups.name[3]), 1:p])
  
  Sp <- ((n1-1)*S1 + (n2-1)*S2  +(n3-1)*S3 )/(n-g)
  Sp
}



# Plot the 2d partition induced by LDA
{
  x  <- seq(min(data.feats[,1]), max(data.feats[,1]), length=200)
  y  <- seq(min(data.feats[,2]), max(data.feats[,2]), length=200)
  
  # rename this, don't use feature1 and feature2, but the true name
  xy <- expand.grid(feature1=x, feature2=y)
  
  plot(data.feats, main='Dataset', xlab='X1', ylab='X2', pch=20)
  points(data.feats[i1,], col='red', pch=20)
  points(data.feats[i2,], col='green', pch=20)
  points(data.feats[i3,], col='blue', pch=20)
  legend("topright", legend=levels(groups.name), fill=c('red','green','blue'), cex=.7)
  
  points(lda.data$means, pch=4,col=c('red','green','blue') , lwd=2, cex=1.5)
  
  # if g = 3
  {
    z  <- predict(lda.data, xy)$post  # these are P_i*f_i(x,y)  
    z1 <- z[,1] - pmax(z[,2], z[,3])  # P_1*f_1(x,y)-max{P_j*f_j(x,y)}  
    z2 <- z[,2] - pmax(z[,1], z[,3])  # P_2*f_2(x,y)-max{P_j*f_j(x,y)}    
    z3 <- z[,3] - pmax(z[,1], z[,2])  # P_3*f_3(x,y)-max{P_j*f_j(x,y)}
  }
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

# IMPORTANT: always use the priors without the cost information!!!
{
  inference_train <- predict(lda.data)
  G <- g
  misc <- table(class.true=groups.name, class.assigned=inference_train$class)
  APER <- 0
  for(g in 1:G)
    APER <- APER + sum(misc[g,-g])/sum(misc[g,]) * prior[g]  
  APER
  # 0.029
}

# Predict on new entry
{
  Z0 <- data.frame(feature1=28.0, feature2=12.0)
  predict(lda.data, Z0)
  # -> suspeded at 99%
}
# Estimates and MANOVA: is there difference in groups?
{
  data.feats <- data[,2]
  m <-  colMeans(data[,1:p])
  m1 <- colMeans(data[i1, 1:p])
  m2 <- colMeans(data[i2, 1:p])
  m3 <- colMeans(data[i3, 1:p])
  
  S1 <- cov(data[i1, 1:p])
  S2 <- cov(data[i2, 1:p])
  S3 <- cov(data[i3, 1:p])
  Sp  <- ((n1 - 1) * S1 + (n2 - 1) * S2 + (n3 - 1) * S3) / (n - g)
  
  # One-way MANOVA 
  fit <- aov(data.feats ~ groups.name)
  summary(fit)
  # low pval<2e-16 -> it is significant the temperature
}



