library(mvtnorm) # to deal with multivariate normal distributions
library(car) # "Companion to Applied Regression" for regression analysis

data <- read.table("data.txt" ,header=TRUE)

boxplot(scale(x = data, center = T, scale = F), las = 2, col = 'gold')

## Scaling -----
data <- scale(data)
data <- data.frame(data)

# always boxplot when doing PCA to asess variances magnitude
# scaled in the mean
boxplot(scale(x = data, center = T, scale = F), las = 2, col = 'gold')

pca <- princomp(data, scores = T)
pca
summary(pca)

# To obtain the rows of the summary:
# Standard deviation of the components
pca$sd # square root of the eigenvalues
# Proportion of variance explained by each PC
pca$sd^2 / sum(pca$sd^2) # already present in summary
# Cumulative proportion of explained variance
cumsum(pca$sd^2) / sum(pca$sd^2)

# loadings
loads <- pca$loadings
loads # hides very small loadings, this is the matrix P.
loads[, 1:dim(data)[2]] # show all loadings

#show barplot of first n principal components
n <- 2
par(mfrow = c(n,1))
for(i in 1:n) barplot(loads[,i], ylim = c(-1, 1), main=paste('Loadings PC ',i,sep=''))

# Plot contribution to the total variance by number of components
plot(cumsum(pca$sd^2) / sum(pca$sd^2), type = 'b', axes = FALSE,
     xlab = 'Number of components', ylab = 'Contribution to the total variance', ylim = c(0, 1))
abline(h = 1, col = 'blue')
abline(h = 0.8, lty = 2, col = 'blue')
box()
axis(2, at = 0:10/10, labels = 0:10/10)
axis(1, at = 1:ncol(data), labels = 1:ncol(data), las = 2)


## Scores -------------------------------------------------------------------------------------
scores <- pca$scores
scores

# scatter plot of the scores
par(mfrow = c(1,1))
plot(scores.tourists[, 1:2])
abline(h=0, v=0, lty=2, col='grey')

# plot according to categorical variable
# Define a cyclic palette
colors <- c("black","red","yellow","green","blue","cyan")

n <- dim(features)[1]
col.lab1 <- rep(NA, n)

for(i in 1:n)
  col.lab1[i] <- colors[which(groups[i] == levels(groups))]

plot(scores[, 1:2], col = col.lab1, pch = 19, xlim = c(-8, 25), ylim = c(-3, 3.2))
legend('topright', levels(groups), fill = colors, bty = 'n')

# Biplot
par(mfrow = c(1, 1))
biplot(pca) # index of the observations.
# the arrows are projection of the original axes (features).
# Moving along first principal component i see i move along specific direction from my
# original dataset.

# scores of the new obs
newobs <- c()
newobs <- scale(newobs) #if you scaled at the start

newscores <- t(loads) %*% newobs


