library(mvtnorm) # to deal with multivariate normal distributions
library(car) # "Companion to Applied Regression" for regression analysis

data <- read.table("dnd_monsters.txt" ,header=TRUE)
head(data)
features <- data[,1:8]
labels <- data[,9]
groups <- factor(labels)

head(features)

boxplot(scale(x = features, center = T, scale = F), las = 2, col = 'gold')
# the hit points variable has a wider variability

## Scaling -----
features <- scale(features)
features <- data.frame(features)

# always boxplot when doing PCA to asess variances magnitude
# scaled in the mean
boxplot(scale(x = features, center = T, scale = F), las = 2, col = 'gold')

pca <- princomp(features, scores = T)
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
loads[, 1:dim(features)[2]] # show all loadings

#show barplot of first n principal components
n <- 2
par(mfrow = c(1,n))
for(i in 1:n) barplot(loads[,i],ylim=c(-1,1), main=paste('Loadings PC ',i,sep=''))
# from the first loadings we see average contribution of all the variables
# from the second we see that character with high physical stats tend to have lower
# mental and agility ones and viceversa. This indicates a distincition

# Plot contribution to the total variance by number of components
plot(cumsum(pca$sd^2) / sum(pca$sd^2), type = 'b', axes = FALSE,
     xlab = 'Number of components', ylab = 'Contribution to the total variance', ylim = c(0, 1))
abline(h = 1, col = 'blue')
abline(h = 0.8, lty = 2, col = 'blue')
box()
axis(2, at = 0:10/10, labels = 0:10/10)
axis(1, at = 1:ncol(features), labels = 1:ncol(features), las = 2)


## Scores -------------------------------------------------------------------------------------
scores <- pca$scores
scores

# scatter plot of the scores
# Define a cyclic palette
colors <- c("black","red","yellow","green","blue","cyan")

n <- dim(features)[1]
col.lab1 <- rep(NA, n)

for(i in 1:n)
  col.lab1[i] <- colors[which(groups[i] == levels(groups))]

plot(scores[, 1:2], col = col.lab1, pch = 19, xlim = c(-8, 25), ylim = c(-3, 3.2))
legend('topright', levels(groups), fill = colors, bty = 'n')

#SVM------------------------------------------
newdat <- scores[which(groups=="Tiny" | groups=="Huge"),1:2]
newlabels <- labels[which(groups=="Tiny" | groups=="Huge")]

library(e1071)
newdat <- data.frame(x=newdat, y=as.factor(newlabels))

svmfit <- svm(y~., data= newdat, kernel="linear", cost=1,scale=FALSE)
summary(svmfit)
# 5 support vectors


par(mfrow=c(1,1))
plot(svmfit, newdat, col =c('salmon', 'light blue'), pch=19, asp=1)


newobs <- c(14,50,19,10,16,8,12,13)
newobs <- scale(newobs)

newscore <- t(loads) %*% newobs
newscore <- data.frame(x = cbind(newscore[1],newscore[2]))
colnames(newscore)[1:2] <- colnames(newdat)[1:2]

prediction <- predict(svmfit, newscore)
prediction
# predicted as huge
