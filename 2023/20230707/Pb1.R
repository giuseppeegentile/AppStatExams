library(mvtnorm) # to deal with multivariate normal distributions
library(car) # "Companion to Applied Regression" for regression analysis

data <- read.table("products.txt", header=TRUE)

head(data)

boxplot(data)
# i would argue it's reasonable to use the original variables

pca <- princomp(data, scores=TRUE)

summary(pca)
plot(pca)

plot(cumsum(pca$sde^2) / sum(pca$sde^2), type = 'b', axes = FALSE, 
     xlab = 'Number of components', ylab = 'Contribution to the total variance', ylim = c(0, 1))
abline(h = 1, col = 'blue')
abline(h = 0.8, lty = 2, col = 'blue')
box()
axis(2, at = 0:10/10, labels = 0:10/10)
axis(1, at = 1:ncol(tourists.sd), labels = 1:ncol(tourists.sd), las = 2)
# it's not really clear how many principal component must be considered.
# I would argue at least 3, but the increase is steady after the third and the fourth.

# to explain at least 80% you need 5 components


load <- pca$loadings
load

# Graphical representation
par(mar = c(2,2,2,1), mfrow=c(3,1))
for(i in 1:3)barplot(load[,i], ylim = c(-1, 1), main=paste('Loadings PC ',i,sep=''))

scores <- pca$scores

par(mfrow=c(1,1))
biplot(pca)

# 134 is more important for what concerns electrodomestics (come cazz se scrive)
# less important for what concerns electonic personal devices

# point a suggests a dimensionality reduction of 5. i guess.
fivepc <-load[,1:5]
newobs <- c(1.41,1.14,1.02,1.21,1.11,1.14,0.99,0.73,0.94,1.04,1.02,1.11)
newobs

scores.newobs <- newobs %*% fivepc 
scores.newobs
