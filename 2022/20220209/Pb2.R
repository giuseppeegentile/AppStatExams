###----------------------###
### Problem 2 (20220209) ###
###----------------------###

library(MVN)

rm(list = ls())
graphics.off()

data <- read.table('streaming.txt', header = T)
head(data)

n <- dim(data)[1]

plot(data, pch = 19)


# a) ----------------------------------------------------------------------

dist <- dist(data, method = 'euclidean')

HC <- hclust(dist, method = 'single')

plot(HC, main = paste(HC$dist.method, "-" , HC$method, sep = ''), hang = -0.1, sub = '', labels = F, xlab = '')

k <- 3 # number of clusters
rect.hclust(HC, k = k)

# Clustering

cluster <- cutree(HC, k = k)

plot(data, col = cluster + 1, pch = 16, lwd = 2)


# b) ----------------------------------------------------------------------

means <- NULL
for(i in 1:length(unique(cluster)))
{
  means = rbind(means, sapply(data[cluster == i,], mean))
}
means
#       minutes artists
# [1,] 199.9639 29.5506
# [2,]  59.9540 44.8832
# [3,]  48.0832  3.7916

points(means, pch = '*', cex = 4)

table(cluster)
#   1   2   3 
# 150  50  25

coph <- cophenetic(HC)
cor(dist, coph)
# 0.8953819


# c) ----------------------------------------------------------------------

p <- dim(data)[2]
k <- p * length(unique(cluster))
alpha <- 0.05

n1 <- table(cluster)[1]

M1 <- sapply(data[which(cluster == 1), ], mean)
S1 <- cov(data[which(cluster == 1), ])
S1.inv <- solve(S1)

mvn(data[which(cluster == 1), ])$multivariateNormality
# p-value = 0.9678169 -> OK

cfr.t1 <- qt(1 - alpha/(2*k), n1-1)

BF1.I <- cbind(M1 - cfr.t1 * sqrt(diag(S1)/n1),
               M1, 
               M1 + cfr.t1 * sqrt(diag(S1)/n1))
dimnames(BF1.I)[[2]] <- c('inf', 'center', 'sup')
BF1.I
#               inf   center       sup
# minutes 193.35203 199.9639 206.57571
# artists  27.23848  29.5506  31.86272

# -

n2 <- table(cluster)[2]

M2 <- sapply(data[which(cluster == 2), ], mean)
S2 <- cov(data[which(cluster == 2), ])
S2.inv <- solve(S2)

mvn(data[which(cluster == 2), ])$multivariateNormality
# p-value = 0.1042467 -> OK

cfr.t2 <- qt(1 - alpha/(2*k), n2-1)

BF2.I <- cbind(M2 - cfr.t2 * sqrt(diag(S2)/n2),
               M2, 
               M2 + cfr.t2 * sqrt(diag(S2)/n2))
dimnames(BF2.I)[[2]] <- c('inf', 'center', 'sup')
BF2.I
#              inf  center      sup
# minutes 55.95035 59.9540 63.95765
# artists 41.93662 44.8832 47.82978

# -

n3 <- table(cluster)[3]

M3 <- sapply(data[which(cluster == 3), ], mean)
S3 <- cov(data[which(cluster == 3), ])
S3.inv <- solve(S3)

mvn(data[which(cluster == 3), ])$multivariateNormality
# p-value = 0.7279738 -> OK

cfr.t3 <- qt(1 - alpha/(2*k), n3-1)

BF3.I <- cbind(M3 - cfr.t3 * sqrt(diag(S3)/n3),
               M3, 
               M3 + cfr.t3 * sqrt(diag(S3)/n3))
dimnames(BF3.I)[[2]] <- c('inf', 'center', 'sup')
BF3.I
#              inf  center      sup
# minutes 42.86400 48.0832 53.30240
# artists  3.07285  3.7916  4.51035
