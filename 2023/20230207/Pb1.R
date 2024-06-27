###----------------------###
### Problem 1 (20230207) ###
###----------------------###

rm(list = ls())
graphics.off()

data <- read.table('decathlon.txt', header = T)
head(data)


# a) ----------------------------------------------------------------------

data.label <- data[, 1:2]
data <- data[, 3:12]

boxplot(data, las = 2, col = 'gold')
boxplot(scale(x = data, center = T, scale = F), las = 2, col = 'gold')
boxplot(scale(x = data, center = T, scale = T), las = 2, col = 'gold')
# We definitely need to scale data because we have significant changes in variability between all the variables

data[, c(1, 5, 6, 10)] <- -data[, c(1, 5, 6, 10)]
# Now we have all variables denoting better performance for higher values

data.sd <- scale(data)
data.sd <- data.frame(data.sd)

boxplot(data.sd, las = 2, col = 'gold')

pc.data <- princomp(data.sd, scores = T)
pc.data
summary(pc.data)

layout(matrix(c(2, 3, 1, 3), 2, byrow = TRUE))

plot(pc.data, las = 2, main = 'Principal Components', ylim = c(0, 7))
abline(h = 1, col = 'blue')

barplot(sapply(data.sd, sd)^2, las = 2, main = 'Original Variables', ylim = c(0, 7),
        ylab = 'Variances')

plot(cumsum(pc.data$sde^2) / sum(pc.data$sde^2), type = 'b', axes = FALSE, 
     xlab = 'Number of components', ylab = 'Contribution to the total variance', ylim = c(0, 1))
abline(h = 1, col = 'blue')
abline(h = 0.8, lty = 2, col = 'blue')
box()
axis(2, at = 0:10/10, labels = 0:10/10)
axis(1, at = 1:ncol(data.sd), labels = 1:ncol(data.sd), las = 2)

load.data <- pc.data$loadings
load.data

load.data[, 1]


# b) ----------------------------------------------------------------------

nPCs <- 3
par(mar = c(2,2,2,1), mfrow = c(nPCs, 1))
for(i in 1:nPCs) barplot(load.data[, i], ylim = c(-1, 1), main = paste('Loadings PC ', i, sep = ''))

# The first PC represents.an overall performance
# The second PC mainly contains the information about "throwing" disciplines
# The third PC contrasts running fast capabilities with jumping performances

# High PC1: general high performance, going bad in discus throw
# Low PC1: general low performance, going good in discus throw
# High PC2: good timing in 1500m, really bad performances in "throwing" disciplines
# Low PC2: bad timing in 1500m, really good performances in "throwing" disciplines
# High PC3: really good timing in 100m, really bad performances in "jumping" disciplines
# Low PC3: really bad timing in 100m, really good performances in "jumping" disciplines


# c) ----------------------------------------------------------------------

par(mfrow = c(1, 1))
biplot(pc.data)
# Athletes with positive scores for both the first and second components are
# good in running and jumping disciplines, bad in throwing ones

biplot(pc.data, choices = 2:3)
# Athletes with positive scores for both the second and third components are
# good in running but perform badly in the other disciplines


# d) ----------------------------------------------------------------------

plot(cumsum(pc.data$sde^2) / sum(pc.data$sde^2), type = 'b', axes = FALSE, 
     xlab = 'Number of components', ylab = 'Contribution to the total variance', ylim = c(0, 1))
abline(h = 1, col = 'blue')
abline(h = 0.8, lty = 2, col = 'blue')
box()
axis(2, at = 0:10/10, labels = 0:10/10)
axis(1, at = 1:ncol(data.sd), labels = 1:ncol(data.sd), las = 2)
# To explain at least 80% of the variaility, we would need to take the first 5 components

(cumsum(pc.data$sde^2) / sum(pc.data$sde^2))[5]
(cumsum(pc.data$sde^2))[5]


# e) ----------------------------------------------------------------------

new.datum <- c(X100m = -10.81,
               long_jump = 7.46,
               shot_put = 14.56,
               high_jump = 2.02,
               X400m = -47.90,
               X110m_hurdles = -14.44,
               discus_throw = 43.04,
               pole_vault = 4.90,
               javelin_throw = 57.24,
               X1500m = -281.63)

new.datum.std <- (new.datum - colMeans(data)) / sapply(data, sd)

new.datum.std.score <- t(load.data) %*% (new.datum.std - colMeans(data.sd))
new.datum.std.score
new.datum.std.score[1:5]
# He's good at running, but has difficulties in throwing stuff

plot(pc.data$scores[, 1:2], pch = 19, las = 2)
points(new.datum.std.score[1], new.datum.std.score[2], col = 'red', pch = 19)


# Extra -------------------------------------------------------------------

# Reconstruct the signal of the new datum

scores.data <- pc.data$scores
k <- 5 # number of PCs

par(mfrow=c(2,1+floor(k/2)+k%%2))
matplot(t(data.sd), type = 'l', main = 'Data', ylim = range(data.sd), xaxt = 'n')
axis(1, at = 1:dim(data.sd)[[2]], labels = colnames(data), las = 2)
matplot(cbind(new.datum.std), type = 'l', lwd = 3, col = 'red', add = T)
meanF <- colMeans(data.sd)
matplot(meanF, type = 'l', main = 'First 0 PCs', lwd = 2, ylim = range(data.sd), xaxt = 'n')
axis(1, at = 1:dim(data.sd)[[2]], labels = colnames(data), las = 2)
projection <- matrix(meanF, dim(data.sd)[[1]], dim(data.sd)[[2]], byrow = T)
projection.new.datum.std <- matrix(meanF, 1, dim(data.sd)[[2]], byrow = T)
for (i in 1:k) {
  projection <- projection + scores.data[, i] %*% t(load.data[, i])
  projection.new.datum.std <- projection.new.datum.std + new.datum.std.score[i] %*% t(load.data[, i])
  matplot(t(projection), type = 'l', main = paste('First', i, 'PCs'), ylim = range(data.sd), xaxt = 'n')
  axis(1, at = 1:dim(data.sd)[[2]], labels = colnames(data), las = 2)
  matplot(t(projection.new.datum.std), type = 'l', lwd = 3, col = 'red', add = T)
  matplot(meanF, type = 'l', lwd = 2, add = T)
}
