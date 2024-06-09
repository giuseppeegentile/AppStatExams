###-----------------------------------###
### Problem 1: Kapok trees (20231107) ###
###-----------------------------------###

rm(list = ls())
graphics.off()

library(MVN)

data <- read.table('kapok.txt', header = T)
head(data)

plot(data)


# a) ----------------------------------------------------------------------

HC <- hclust(dist(data, method='euclidean'), method = 'ward.D2')

plot(HC, hang=-0.1, sub='', labels=F, xlab='')

rect.hclust(HC, k=4)

cluster <- cutree(HC, k=4)
table(cluster)

plot(data, col=cluster+1, pch=16, lwd=2)

means <- NULL
for(k in 1:length(unique(cluster)))
{
  means = rbind(means, sapply(data[cluster==k,],mean))
}

means
points(means, pch=17, cex=1.5)


# b) ----------------------------------------------------------------------

vars <- NULL
for(k in 1:length(unique(cluster)))
{
  vars <- rbind(vars, sapply(data[cluster == k, ], var))
}

p <- dim(data)[2] 
k <- 2 * p * length(unique(cluster))
alpha <- 0.1

BF <- NULL
for (i in 1:length(unique(cluster)))
{
  n <- table(cluster)[i]
  cfr.t <- qt(1-alpha/(2*k), n-1) 
  BF_i <- rbind(c(means[i, ] - cfr.t*sqrt(vars[i, ]/n),
                  means[i, ],
                  means[i, ] + cfr.t*sqrt(vars[i, ]/n)),
                c(vars[i, ]*(n-1) / qchisq(1 - alpha/(2*k), n-1),
                  vars[i, ],
                  vars[i, ]*(n-1) / qchisq(alpha/(2*k), n-1)))
  
  dimnames(BF_i)[[1]] <- c(paste("m", i, sep = ''), paste("v", i, sep = ''))
  BF <- rbind(BF, BF_i)
}

dimnames(BF)[[2]] <- rep(c('inf', 'center', 'sup'), each = p)

for(j in 1:p)
{
  print(paste("Confidence intervals for variable:", dimnames(data)[[2]][j]))
  print(BF[, seq(j, 3*p, by = p)])
  cat("\n")
}

for(i in 1:length(unique(cluster)))
  print(paste("Normality p-value for cluster #", i, ": ", mvn(data[cluster == i, ])$multivariateNormality$'p value', sep = ''))

# Only cluster #4 doesn't satisfy normality assumption


# Extra -------------------------------------------------------------------


## Cophenetic Coefficient --------------------------------------------------

data.e <- dist(data, method='euclidean')
coph.ew <- cophenetic(HC)

cor(data.e, coph.ew)


## Choice of k (W(k)/SS_tot Plot) ------------------------------------------

w <- NULL
w_i <- NULL
b <- NULL
b_i <- NULL
mean <- colMeans(data)

for(k in 1:10){
  cluster <- cutree(HC, k = k)
  w_i <- NULL
  b_i <- NULL
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
    mean_i_centered <- means[i, ]
    mean_i_centered[1] <- means[i, 1] - mean_bar[1]
    mean_i_centered[2] <- means[i, 2] - mean_bar[2]
    w_i <- c(w_i, sum(cluster_i_centered_rel^2))
    b_i <- c(b_i, table(cluster)[i] * sum(mean_i_centered^2)) 
    # there's something wrong here with b_i (don't use it)
  }
  w <- c(w, sum(w_i))
  b <- c(b, sum(b_i))
}

data_centered <- data
data_centered[, 1] <- data[, 1] - mean[1]
data_centered[, 2] <- data[, 2] - mean[2]

t <- sum(data_centered^2)

matplot(1:10, w/t, pch='', xlab='clusters', ylab='within/tot', main='Choice of k', ylim=c(0,1))
lines(1:10, w/t, type='b', lwd=2)