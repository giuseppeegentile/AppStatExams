library(mvtnorm)
library(MVN)
library(rgl)
library(car)
library(dbscan)
library(cluster)
library(fields)

## HIERARCHICAL CLUSTERING--------------------------------
data <- read.table("pinnanobilis.txt", header=TRUE)
head(data)

plot(data)

distance <- dist(data, method='euclidean')

hclust.c <- hclust(distance, method='complete')

# plot of the dendrograms
par(mfrow=c(1,1))

plot(hclust.c)

clusters <- cutree(hclust.c, k=2) # euclidean-complete:

# plot if 2 clusters
plot(data, col=ifelse(clusters==1,"red","blue"),pch=19)

groups <- factor(clusters)
groups

fit <- manova(as.matrix(data) ~ groups)
summary.manova(fit)
# yes the grouping structure is significant

# Bonferroni
p <-2
g <-2
n <- dim(data)[1]
alpha <- 0.1
k <- p * g * (g - 1) / 2 # confidence intervals for every pair of groups and every variable
qT <- qt(1 - alpha / (2 * k), n - g)

i1<-which(groups==levels(groups)[1])
i2<-which(groups==levels(groups)[2])
n1 <- length(i1)
n2 <- length(i2)

mvn(data[i1,])$multivariateNormality
mvn(data[i2,])$multivariateNormality


values <- data
W <- summary.manova(fit)$SS$Residuals
m  <- sapply(values, mean)         # estimates mu
m1 <- sapply(values[i1,], mean)    # estimates mu.1 = mu + tau.1
m2 <- sapply(values[i2,], mean)    # estimates mu.2 = mu + tau.2


inf12 <- m1 - m2 - qT * sqrt(diag(W)/(n-g) * (1/n1+1/n2))
sup12 <- m1 - m2 + qT * sqrt(diag(W)/(n-g) * (1/n1+1/n2))

CI <- cbind(inf12,sup12)
CI
# there is difference in both the measures
