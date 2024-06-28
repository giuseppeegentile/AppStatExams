###----------------------###
### Problem 2 (20230118) ###
###----------------------###

rm(list = ls())
graphics.off()

data <- read.table('beers.txt', header = T)
head(data)

n <- dim(data)[1]

plot(data, pch = 19)


# a) ----------------------------------------------------------------------

boxplot(data, las = 2, col = 'gold')
boxplot(scale(x = data, center = T, scale = F), las = 2, col = 'gold')
boxplot(scale(x = data, center = T, scale = T), las = 2, col = 'gold')

data.sd <- scale(data)
data <- data.frame(data.sd)

dist <- dist(data, method = 'euclidean')
image(1:n, 1:n, as.matrix(dist), asp = 1, xlab = 'i', ylab = 'j')

HC.s <- hclust(dist, method = 'single')
HC.c <- hclust(dist, method = 'complete')

plot(HC.s, main = paste(HC.s$dist.method, "-" , HC.s$method, sep = ''), hang = -0.1, sub = '', labels = F, xlab = '')
plot(HC.c, main = paste(HC.c$dist.method, "-" , HC.c$method, sep = ''), hang = -0.1, sub = '', labels = F, xlab = '')


# b) ----------------------------------------------------------------------

# Single linkage is strongly victim of the chaining effect
# -> go with the complete one

HC <- hclust(dist, method = 'complete')

k <- 2 # number of clusters
rect.hclust(HC, k = k)


# c) ----------------------------------------------------------------------

cluster <- cutree(HC, k = k)
table(cluster)
#  1  2 
# 70 55 

plot(data, col = cluster + 1, pch = 16, lwd = 2)

means <- NULL
for(i in 1:length(unique(cluster)))
{
  means = rbind(means, sapply(data[cluster == i,], mean))
}

means
#         alcohol        ibu
# [1,] -0.6909623 -0.8190009
# [2,]  0.8794066  1.0423647

points(means, col = sort(unique(cluster)) + 1, pch = '*', cex = 4)

coph <- cophenetic(HC)
cor(dist, coph)
# 0.7919285

par(mfrow=c(1,2))
image(as.matrix(dist), main = HC$dist.method)
image(as.matrix(coph), main = HC$method)
par(mfrow=c(1,1))

