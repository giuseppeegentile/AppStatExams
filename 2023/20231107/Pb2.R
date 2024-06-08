library(mvtnorm) # to deal with multivariate normal distributions
library(car) # "Companion to Applied Regression" for regression analysis

data <- read.table("gemstones.txt",header=TRUE)
head(data)

features <- data[,1:8]
head(features)

boxplot(features)
# form the boxplot is clear that the scale of variances are very different, it's better
# to scale the data

scaled <- scale(features)
boxplot(scaled)

pca <- princomp(scaled, scores=TRUE)
summary(pca)

loadings <- pca$loadings
par(mfrow = c(2,1))
for(i in 1:2) barplot(loadings[,i], ylim = c(-1, 1))
# we see that gem with high scores on PC1 will be big in general but with low eccentricity
# gem with high scores on PC2 will present special eccentricoty and lower values of the other characteristics
# in particular roundness.


types <- factor(data$Type)
levels(types)
n <- dim(data)[1]
scores <- pca$scores
# Define a cyclic palette
colors <- c("red", "blue", "green")

col.lab1 <- rep(NA, n)
for(i in 1:n)
  col.lab1[i] <- colors[which(types[i] == levels(types))]

par(mfrow=c(1,1))
plot(scores[, 1:2], col = col.lab1, pch = 19, xlim = c(-8, 25), ylim = c(-3, 3.2))

legend('topright', levels(types), fill = colors, bty = 'n')


# confidence region 95%
ruby <- scores[which(types == "ruby"),1:2]
n <- dim(ruby)[1]
p <- dim(ruby)[2]

alpha <-0.05
mean <- colMeans(ruby)
S <- cov(ruby)

cfr.fisher <- (((n-1)*p)/(n-p))*qf(1-alpha,p,n-p )

dev.off()
plot(ruby)
ellipse(center=mean, shape=S/n, radius=sqrt(cfr.fisher), lwd=2, col='grey',
        center.cex=1.25)
center <- mean
directions <- eigen(S)$vectors
length_semiaxis <- c(sqrt(cfr.fisher)/sqrt(eigen(S)$values[1]),
          sqrt(cfr.fisher)/sqrt(eigen(S)$values[2]))
length_semiaxis
# test for normality
p <- mvn(ruby)$multivariateNormality
p
# at 1% we can assess normality.