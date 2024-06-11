library(mvtnorm) # to deal with multivariate normal distributions
library(car)


data <- read.table("decathlon.txt", header=TRUE)
features <- data[,3:12]

par(mfrow=c(1,1))
boxplot(features)
# from the boxplot it's clear it's better to scale the data

scaled <- scale(features)
boxplot(scaled)

# proposed transformation to have it that if you have a high value in that variable means good performance
# now with seconds it's not like that
data$X100m <- 1/data$X100m
data$X400m <- 1/data$X400m
data$X110m_hurdles <- 1/data$X110m_hurdles
data$X1500m <- 1/data$X1500m


pca <- princomp(scaled, scores=TRUE)
loadings<-pca$loadings
# loadings of first principal component
loadings[,1]

par(mar = c(2,2,2,1), mfrow=c(3,1))
for(i in 1:3)barplot(loadings[,i], ylim = c(-1, 1), main=paste('Loadings PC ',i,sep=''))

# high score on PC1, general high performance, with exception in shot_put, and discus_throw, may indicate
# better running capabilities
# high score on PC2, means very bad in throws while good performance on 1500. Long running athletes come cazz
# si scrive in inglese
# high score on PC3, indicates athletes with bad jumping ability

# biplot
# it's bugged wtf
par(mfrow=c(1,1))
biplot(pca)

# positive score on PC1 and PC2 means athletes with good running capabilities in general (both long
# and short distances), and bad performance on throws.
par(mfrow=c(1,1))
plot(cumsum(pca$sde^2) / sum(pca$sde^2), type = 'b', axes = FALSE, 
     xlab = 'Number of components', ylab = 'Contribution to the total variance', ylim = c(0, 1))
abline(h = 1, col = 'blue')
abline(h = 0.8, lty = 2, col = 'blue')
box()
axis(2, at = 0:10/10, labels = 0:10/10)
axis(1, at = 1:ncol(scaled), labels = 1:ncol(scaled), las = 2)

# only considering the proportion of variance i would use 4 PCs, since after the increase of variability
# explained it's not that substantial.
# however if we want to plot the projected data it may be better to cut at 3 PCs, since the proportion of
# variance is only 9% lower. Cutting at two we would miss too much variance.

# with k=4 i have 
0.7945781 

loading <- pca$loadings[,1:4]
loading

newdata <- rbind(1/10.81, 7.46, 14.56, 2.02, 1/47.90, 1/14.44,43.04, 4.90, 57.24, 1/(4*60 +41.63))
newdata <- scale(newdata)

projection <- t(loading) %*% newdata
projection



