###-----------------------------------------------###
### Problem 2: Asteroid shape analysis (20240117) ###
###-----------------------------------------------###

rm(list = ls())
graphics.off()

options(rgl.printRglwidget = TRUE)

library(MVN)
library(mvtnorm)
library(car)

data <- read.table('asteroids.txt', header=T)
head(data)
dim(data)

n <- dim(data)[1]
p <- dim(data)[2]


# a) ----------------------------------------------------------------------

data.label <- data[, 9]
data <- data[, -9]

boxplot(scale(x = data, center = T, scale = F), las = 2, col = 'gold')

# It makes sense to use standardized variables otherwise the PCs would basically be SurfArea and ConVol

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

# If we wanted to perform dimensionality reduction, we could keep 3 PCs


# b) ----------------------------------------------------------------------

load.data <- pc.data$loadings
load.data

par(mar = c(2,2,2,1), mfrow=c(2,1))
for(i in 1:2) barplot(load.data[,i], ylim = c(-1, 1), main=paste('Loadings PC ', i, sep=''))

# The first PC represents an average of the measures regarding how big the asteroid is.
# The second PC contains the information about eccentricity and roundness.

# High PC1: big asteroid overall
# Low PC1: small asteroid overall
# High PC2: high eccentricity, small roundness
# Low PC2: small eccentricity, high roundness

scores.data <- pc.data$scores

col.lab <- ifelse(data.label[] %in% c("silicate", "metallic"), 
                  ifelse(data.label[] %in% c("silicate"), 
                         'blue', 'green'), 
                  'red')

par(mfrow = c(1, 1))
plot(scores.data[, 1:2], col = col.lab, pch = 19, xlim = c(-5, 5), ylim = c(-5, 5))
abline(h = -5, v = -5, col = 1)
points(scores.data[, 1], rep(-5, n), col = col.lab, pch = 19)
points(rep(-5, n), scores.data[, 2], col = col.lab, pch = 19)
abline(h = 0, v = 0, lty = 2, col = 'grey')
legend('topright', levels(factor(data.label[])), fill = c('red', 'green', 'blue'), bty = 'n')

# The asteroids are very well clustered along the first PC's direction:
# carbonaceous ones are relatively small, silicate ones are really big, metallic ones are halfway round


# c) ----------------------------------------------------------------------

scores.new <- scores.data[which(data.label == "metallic"), 1:2]

n.new <- dim(scores.new)[1]
p.new <- dim(scores.new)[2]

M <- colMeans(scores.new)
S <- cov(scores.new)

plot(scores.new, asp = 1, xlab = 'PC 1', ylab = 'PC 2', pch = 19)

alpha <- 0.05
cfr.fisher <- ((n.new - 1) * p.new / (n.new - p.new)) * qf(1 - alpha, p.new, n.new - p.new)

ellipse(M, shape = S/n.new, sqrt(cfr.fisher), col = 'red', lty = 2, center.pch = 16)

abline(a = M[2] - eigen(S/n.new)$vectors[2, 1] / eigen(S/n.new)$vectors[1, 1] * M[1], 
       b = eigen(S/n.new)$vectors[2, 1] / eigen(S/n.new)$vectors[1, 1], 
       lty = 2, col = 'dark red', lwd = 2)
abline(a = M[2] - eigen(S/n.new)$vectors[2, 2] / eigen(S/n.new)$vectors[1, 2] * M[1], 
       b = eigen(S/n.new)$vectors[2, 2] / eigen(S/n.new)$vectors[1, 2], 
       lty = 2, col = 'red', lwd = 2)

print("Center coordinates"); M
print("Direction of the major semi-axis"); eigen(S/n.new)$vectors[, 1]
print("Direction of the minor semi-axis"); eigen(S/n.new)$vectors[, 2]
print("Length of the major semi-axis"); sqrt(cfr.fisher)*sqrt(eigen(S/n.new)$values[1])
print("Length of the minor semi-axis"); sqrt(cfr.fisher)*sqrt(eigen(S/n.new)$values[2])

mvn(scores.new)$multivariateNormality

# The data don't seem Gaussian