library(mvtnorm)
library(MVN)
library(car)
# 15.11
data <- read.table("asteroids.txt", header=TRUE)

quantitative <- data[,1:8]

boxplot(quantitative)

# it's clear that this data have different scales
# it's better to proceed with standardized variable in order to avoid variabilty masking

scaled <- scale(quantitative)
scaled <- data.frame(scaled)

pca <- princomp(scaled, scores = TRUE)
pca
summary(pca)
plot(pca)
plot(cumsum(pca$sde^2) / sum(pca$sde^2), type = 'b', axes = FALSE, 
     xlab = 'Number of components', ylab = 'Contribution to the total variance', ylim = c(0, 1))

load <- pca$loadings
par(mar = c(2,2,2,1), mfrow=c(2,1))
for(i in 1:2)barplot(load[,i], ylim = c(-1, 1), main=paste('Loadings PC ',i,sep=''))

scores <- pca$scores

types <- factor(data$Type)

n <- dim(data)[1]

colors <- c("red", "green", "blue")
all_possible_labels <- c(unique(types))
data.label <- factor(types, levels = all_possible_labels)

col.lab1 <- rep(NA, n)
for(i in 1:n)
  col.lab1[i] <- colors[which(types[i] == levels(as.factor(types)))]

plot(scores[, 1:2], col = col.lab1, pch = 19)
legend('topright', levels(data.label), fill = colors, bty = 'n')


metallic <- scores[which(data$Type == levels(types)[2]),1:2]
head(metallic)

alpha <-0.05
p<-dim(metallic)[2]
n<-dim(metallic)[1]

mean <- colMeans(metallic, mean)
mean
S <- cov(metallic)
S.inv <- solve(S)
quantile <- ((n-1)*p /(n-p))*qf(1-alpha,p,n-p)

plot(metallic, main='Stocazzo',xlim=c(-2,2))
ellipse(center=mean, shape=S/n, radius=sqrt(quantile), lwd=2)


mvn(metallic)$multivariateNormality
