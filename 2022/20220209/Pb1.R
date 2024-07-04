###----------------------###
### Problem 1 (20220209) ###
###----------------------###

rm(list = ls())
graphics.off()

data <- read.table('nutrients.txt', header = T)
head(data)


# a) ----------------------------------------------------------------------

boxplot(data, las = 2, col = 'gold')
boxplot(scale(x = data, center = T, scale = F), las = 2, col = 'gold')
boxplot(scale(x = data, center = T, scale = T), las = 2, col = 'gold')
# It's better top scale data

data.sd <- scale(data)
data.sd <- data.frame(data.sd)

boxplot(data.sd, las = 2, col = 'gold')

pc.data <- princomp(data.sd, scores = T)
pc.data
summary(pc.data)

load.data <- pc.data$loadings
load.data[, 1]
# Energy_kcal   Protein_g       Fat_g      Carb_g     Sugar_g     Fiber_g 
#  0.36836703 -0.53846028 -0.09029219  0.46641374  0.43424217 -0.40013191 


# b) ----------------------------------------------------------------------

nPCs <- 3
par(mar = c(2,2,2,1), mfrow = c(nPCs, 1))
for(i in 1:nPCs) barplot(load.data[, i], ylim = c(-1, 1), main = paste('Loadings PC ', i, sep = ''))

# The first PC contrasts the "energy" aspects with the ones more related to "regulatory" aspects 
# The second PC contains the information about the fatness level, that wasn't present in PC1
# The third PC is a weighted average of the nutrients

# High PC1: high "energy" levels, scarce presence of protein and fiber
# Low PC1: high presence of protein and fiber, scarce "energy" levels
# High PC2: high.presence of fat; really energetic cereals
# Low PC2: absence of fat; not so energetic cereals


# c) ----------------------------------------------------------------------

par(mfrow = c(1, 1))
biplot(pc.data)
# Cereals with positive scores for both the first and second components have high values for energy and sugar
# and are generally poor of proteins and fibers (+ they are typically fat)


# d) ----------------------------------------------------------------------

plot(cumsum(pc.data$sde^2) / sum(pc.data$sde^2), type = 'b', axes = FALSE, 
     xlab = 'Number of components', ylab = 'Contribution to the total variance', ylim = c(0, 1))
abline(h = 1, col = 'blue')
abline(h = 0.8, lty = 2, col = 'blue')
box()
axis(2, at = 0:10/10, labels = 0:10/10)
axis(1, at = 1:ncol(data.sd), labels = 1:ncol(data.sd), las = 2)
# In order to explain at least the 80% of the total variability, we need to take 3 PCs

(cumsum(pc.data$sde^2) / sum(pc.data$sde^2))[3]
# 0.9070371


# e) ----------------------------------------------------------------------

new.datum <- c(400, 9, 5, 100, 30, 4)

new.datum.std <- (new.datum - colMeans(data)) / sapply(data, sd)

new.datum.std.score <- t(load.data) %*% (new.datum.std - colMeans(data.sd))
new.datum.std.score[1:3]
# 2.5059632 -0.3110952 -0.1301437

