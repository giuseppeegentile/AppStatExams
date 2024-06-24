###------------------------------------------------------------------------------###
### Problem 1: Products assortment of electronics and appliance shops (20230707) ###
###------------------------------------------------------------------------------###

rm(list = ls())
graphics.off()

data <- read.table('products.txt', header = T)
head(data)


# a) ----------------------------------------------------------------------

boxplot(data, las = 2, col = 'gold')
boxplot(scale(x = data, center = T, scale = F), las = 2, col = 'gold')

# No need to scale data (no significant change in variability between all the variables)

pc.data <- princomp(data, scores = T)
pc.data
summary(pc.data)

layout(matrix(c(2, 3, 1, 3), 2, byrow = TRUE))

plot(pc.data, las = 2, main = 'Principal Components', ylim = c(0, 7))
abline(h = 1, col = 'blue')

barplot(sapply(data, sd)^2, las = 2, main = 'Original Variables', ylim = c(0, 7),
        ylab = 'Variances')

plot(cumsum(pc.data$sde^2) / sum(pc.data$sde^2), type = 'b', axes = FALSE, 
     xlab = 'Number of components', ylab = 'Contribution to the total variance', ylim = c(0, 1))
abline(h = 1, col = 'blue')
abline(h = 0.8, lty = 2, col = 'blue')
box()
axis(2, at = 0:10/10, labels = 0:10/10)
axis(1, at = 1:ncol(data), labels = 1:ncol(data), las = 2)

# There's not a clear number of components to pick;
# in order to explain at least 80% of variability, we need to pick the first 5 PCs


# b) ----------------------------------------------------------------------

load.data <- pc.data$loadings
load.data

nPCs <- 3
par(mar = c(2,2,2,1), mfrow = c(nPCs, 1))
for(i in 1:nPCs) barplot(load.data[, i], ylim = c(-1, 1), main = paste('Loadings PC ', i, sep = ''))

# The first PC represents the contrasts between shops focused more on electronics and the ones on appliances

# High PC1: high supply of electronic devices, poor supply of appliances
# Low PC1: poor supply of electronic devices, high supply of appliances


# c) ----------------------------------------------------------------------

par(mfrow = c(1, 1))
biplot(pc.data)

pc.data$scores[134, 1:2]

# Store 134 is specialized on appliances and it's in general badly supplied


# d) ----------------------------------------------------------------------

new.datum <- c(cellphone = 1.41,
               tablet = 1.14,
               smartwatch = 1.02,
               computer = 1.21,
               drone = 1.11,
               dockstation = 1.14,
               fridge = 0.99,
               oven = 0.73,
               dishwasher = 0.94,
               television = 1.04,
               blender = 1.02,
               hairdryer = 1.11)

new.datum.score <- t(load.data) %*% (new.datum - colMeans(data))
new.datum.score
new.datum.score[1:5]

plot(pc.data$scores[, 1:2], pch = 19, las = 2)
points(new.datum.score[1], new.datum.score[2], col = 'red', pch = 19)
