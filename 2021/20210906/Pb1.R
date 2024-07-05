###----------------------###
### Problem 1 (20210906) ###
###----------------------###

library(MVN)
library(heplots)

rm(list = ls())
graphics.off()

data <- read.table('pinnanobilis.txt', header = T)
head(data)

n <- dim(data)[1]

plot(data, pch = 19)


# a) ----------------------------------------------------------------------

dist <- dist(data, method = 'euclidean')

HC <- hclust(dist, method = 'complete')

plot(HC, main = paste(HC$dist.method, "-" , HC$method, sep = ''), hang = -0.1, sub = '', labels = F, xlab = '')

k <- 2 # number of clusters
rect.hclust(HC, k = k)

cluster <- cutree(HC, k = k)
table(cluster)

plot(data, col = cluster + 1, pch = 16, lwd = 2)


# b) ----------------------------------------------------------------------

treatment <- factor(cluster)
target <- data[, 1:2] # variables

n <- length(treatment)
ng <- table(treatment)
treat <- levels(treatment)
g <- length(levels(treatment))
p <- dim(target)[2]

Ps <- NULL
for(i in 1:g) {
  mvn.test <- mvn(data = target[treatment == treat[i], ])
  Ps <- rbind(Ps, mvn.test$multivariateNormality$`p value`)
}
dimnames(Ps)[[1]] <- treat
dimnames(Ps)[[2]] <- c("p-value")
Ps
# p-values: 0.6852188 and 0.8864103 -> OK

covs <- list()
covs[["S"]] <- cov(target)
for(i in 1:g)
{
  cov <- cov(target[treatment == treat[i], ])
  covs[[paste("S", i, sep = '')]] <- cov
}
covs

# Box's M test for homogeneity of covariance matrices
# Should be done only if n_i > ~20 for all i, p < 5 and g < 5
# WARNING: Very sensitive to departure from normality and too restrictive for MANOVA
# especially if we have a high number of samples

summary(boxM(target, treatment))
boxM(target, treatment)$p.value
# 0.2235859 -> OK

fit <- manova(as.matrix(target) ~ treatment)
summary.manova(fit)
# 2.2e-16 *** -> there is statistical evidence to state that the membership to a cluster has an effect on the mean features

means <- NULL
for(i in 1:g) {
  means <- rbind(means, sapply(target[treatment == treat[i], ], mean)) 
}
dimnames(means)[[1]] <- treat
means
#     height     width
# 1 34.56746 12.660919
# 2 16.56287  7.179675

fit$coefficients
#                height     width
# (Intercept)  34.56746 12.660919
# treatment2  -18.00458 -5.481244

Spooled <- 1/(n-2) * ((cov(target[treatment == treat[1], ])) * (table(treatment)[1] - 1) + (cov(target[treatment == treat[2], ])) * (table(treatment)[2] - 1))
Spooled
#           height    width
# height 11.474476 3.591766
# width   3.591766 3.670716


# c) ----------------------------------------------------------------------

alpha <- 0.10
k <- p * g * (g - 1) / 2
qT <- qt(1 - alpha/(2 * k), n - g)

W <- summary.manova(fit)$SS$Residuals
m <- sapply(target, mean)

means <- NULL
for(i in 1:g) {
  means <- rbind(means, sapply(target[treatment == treat[i], ], mean)) 
}
dimnames(means)[[1]] <- treat
means

inf <- list()
sup <- list()
CI <- list()
pairs <- combn(g, 2)

for (i in 1:ncol(pairs)) {
  idx1 <- pairs[1, i]
  idx2 <- pairs[2, i]
  
  m1 <- means[idx1, ]
  m2 <- means[idx2, ]
  n1 <- ng[idx1]
  n2 <- ng[idx2]
  
  inf_val <- m1 - m2 - qT * sqrt(diag(W) / (n - g) * (1 / n1 + 1 / n2))
  sup_val <- m1 - m2 + qT * sqrt(diag(W) / (n - g) * (1 / n1 + 1 / n2))
  
  inf[[paste("inf", idx1, idx2, sep = '')]] <- inf_val
  sup[[paste("sup", idx1, idx2, sep = '')]] <- sup_val
  
  CI[[paste(treat[idx1], "_", treat[idx2], sep = '')]] <- cbind(inf_val, sup_val)
}

CI
# $`1_2`
#          inf_val   sup_val
# height 16.368443 19.640724
# width   4.555844  6.406644

# Both the features in the two clusters are different, being height the most discriminatory one


# Extra -------------------------------------------------------------------

w <- NULL
w_i <- NULL
mean <- colMeans(data)

for(k in 1:10)
{
  cluster <- cutree(HC, k = k)
  w_i <- NULL
  means <- NULL
  for(i in 1:length(unique(cluster)))
  {
    means <-  rbind(means, sapply(data[cluster == i, ], mean))
  }
  mean_bar <- colMeans(means)
  for(i in 1:length(unique(cluster)))
  {
    cluster_i_centered_rel <- data[cluster == i, ]
    cluster_i_centered_rel[, 1] <- data[cluster == i, 1] - means[i, 1]
    cluster_i_centered_rel[, 2] <- data[cluster == i, 2] - means[i, 2]
    w_i <- c(w_i, sum(cluster_i_centered_rel^2))
  }
  w <- c(w, sum(w_i))
}

data_centered <- data
data_centered[, 1] <- data[, 1] - mean[1]
data_centered[, 2] <- data[, 2] - mean[2]

t <- sum(data_centered^2)

matplot(1:10, w/t, pch = '', xlab = 'clusters', ylab = 'within/tot', main = 'Choice of k', ylim = c(0, 1))
lines(1:10, w/t, type = 'b', lwd = 2)
# -> elbow at k = 2