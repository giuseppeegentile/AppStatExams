###----------------------###
### Problem 2 (20220712) ###
###----------------------###

library(MVN)
library(heplots)
library(car)

rm(list = ls())
graphics.off()

data <- read.table('demogorgons.txt', header = T)
head(data)

n <- dim(data)[1]

plot(data, pch = 19)


# a) ----------------------------------------------------------------------

dist <- dist(data, method = 'euclidean')

HC <- hclust(dist, method = 'average')

plot(HC, main = paste(HC$dist.method, "-" , HC$method, sep = ''), hang = -0.1, sub = '', labels = F, xlab = '')

k <- 2 # number of clusters
rect.hclust(HC, k = k)

cluster <- cutree(HC, k = k)
table(cluster)


# b) ----------------------------------------------------------------------

treatment <- factor(cluster)
target <- data[, 1:2] # variables

n <- length(treatment)
ng <- table(treatment)
treat <- levels(treatment)
g <- length(levels(treatment))
p <- dim(target)[2]

idx <- list()
for(i in 1:g)
{
  idx[[i]] <- which(treatment == treat[i])
}
idx

# 1) normality (multivariate) in each group
Ps <- NULL
for(i in 1:g) {
  mvn.test <- mvn(data = target[idx[[i]], ])
  Ps <- rbind(Ps, mvn.test$multivariateNormality$`p value`)
}
dimnames(Ps)[[1]] <- treat
dimnames(Ps)[[2]] <- c("p-value")
Ps
# OK

# 2) same covariance structure (= same covariance matrix Sigma)

covs <- list()
covs[["S"]] <- cov(target)
for(i in 1:g)
{
  cov <- cov(target[idx[[i]],])
  covs[[paste("S", i, sep = '')]] <- cov
}
covs

summary(boxM(target, treatment))
boxM(target, treatment)$p.value
# 0.8481158 -> OK

fit <- manova(as.matrix(target) ~ treatment)
summary.manova(fit)
#  Pr(>F) 
# 2.2e-16 ***
# -> There is statistical evidence to state that the membership to a cluster has an effect on the mean positions

means <- NULL
for(i in 1:length(unique(cluster)))
{
  means = rbind(means, sapply(data[cluster == i,], mean))
}
means

colMeans(data) # mu
#      lon      lat 
# 86.29718 39.78549 

means[1, ] - colMeans(data) # tau_1
#        lon        lat 
# -0.3229842 -0.3248838 

means[2, ] - colMeans(data) # tau_2
#       lon       lat 
# 0.1906136 0.1917347

sqrt(sum((fit$residuals)^2)/fit$df) # sigma
# 0.1734843


# c) ----------------------------------------------------------------------

plot(data, col = cluster + 1, pch = 16, lwd = 2)
points(means, col = sort(unique(cluster)) + 1, pch = '*', cex = 4)

alpha <- 0.05

n1 <- length(idx[[1]])
S <- covs$S1
cfr.fisher <- ((n1 - 1) * p / (n1 - p)) * qf(1 - alpha, p, n1 - p)

points(means[1, 1], means[1, 2], pch = 19)
ellipse(center = means[1, ], shape = S/n1, radius = sqrt(cfr.fisher), lwd = 2, col = 'blue', center.cex = 1.25)

n2 <- length(idx[[2]])
S <- covs$S2
cfr.fisher <- ((n2 - 1) * p / (n2 - p)) * qf(1 - alpha, p, n2 - p)

points(means[2, 1], means[2, 2], pch = 19)
ellipse(center = means[2, ], shape = S/n2, radius = sqrt(cfr.fisher), lwd = 2, col = 'blue', center.cex = 1.25)
