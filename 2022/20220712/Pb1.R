###----------------------###
### Problem 1 (20220712) ###
###----------------------###

library(e1071)

rm(list = ls())
graphics.off()

data <- read.table('dnd_monsters.txt', header = T)
head(data)

n <- dim(data)[1]

data.label <- data[, 9]
data <- data[, -9]


# a) ----------------------------------------------------------------------

boxplot(data, las = 2, col = 'gold')
boxplot(scale(x = data, center = T, scale = F), las = 2, col = 'gold')
boxplot(scale(x = data, center = T, scale = T), las = 2, col = 'gold')
# We have to use standardized variables since the feature "hit.points" 
# has a very different variability wrt the others

data.sd <- scale(data)
data.sd <- data.frame(data.sd)

boxplot(data.sd, las = 2, col = 'gold')

pc.data <- princomp(data.sd, scores = T)
pc.data
summary(pc.data)


# b) ----------------------------------------------------------------------

load.data <- pc.data$loadings
load.data

nPCs <- 2
par(mar = c(2,2,2,1), mfrow = c(nPCs, 1))
for(i in 1:nPCs) barplot(load.data[, i], ylim = c(-1, 1), main = paste('Loadings PC ', i, sep = ''))

# The first PC represents an average of the characteristics, not taking into consideration dexterity
# The second PC contrasts the aspects more related to intelligence and ability to the "physical" ones

# High PC1: generally powerful monster
# Low PC1: generally weak monster
# High PC2: generally intelligent monster, but not so physically strong
# Low PC2: generally physically strong monster, but not so intelligent


# c) ----------------------------------------------------------------------

scores.data <- pc.data$scores

col.lab <- rainbow(length(levels(factor(data.label[]))))

par(mfrow = c(1, 1))
plot(scores.data[, 1:2], col = col.lab, pch = 19, 
     xlim = c(min(scores.data[, 1]) - 1, max(scores.data[, 1]) + 1), 
     ylim = c(min(scores.data[, 2]) - 1, max(scores.data[, 2]) + 1))
abline(h = min(scores.data[, 2]) - 1, v = min(scores.data[, 1]) - 1, col = 1)
points(scores.data[, 1], rep(min(scores.data[, 2]) - 1, n), col = col.lab, pch = 19)
points(rep(min(scores.data[, 1]) - 1, n), scores.data[, 2], col = col.lab, pch = 19)
abline(h = 0, v = 0, lty = 2, col = 'grey')
legend('topright', levels(factor(data.label[])), fill = col.lab, bty = 'n')
# We can't say anything much, apart the fact that some large monsters have the highest PC1
# (therefore they are amongst the most powerful monsters) and that some tiny ones have
# the highest PC2 (therefore they are amongst the most intelligent and physically weak)

# Better

col.lab <- ifelse(data.label[] %in% c("Medium", "Small", "Tiny"), 'blue', 'red')
col.lab <- ifelse(data.label[] %in% c("Large", "Medium", "Small", "Tiny"), 
                  ifelse(data.label[] %in% c("Small", "Tiny"), 
                         'blue', 'green'), 
                  'red')

par(mfrow = c(1, 1))
plot(scores.data[, 1:2], col = col.lab, pch = 19, 
     xlim = c(min(scores.data[, 1]) - 1, max(scores.data[, 1]) + 1), 
     ylim = c(min(scores.data[, 2]) - 1, max(scores.data[, 2]) + 1))
abline(h = min(scores.data[, 2]) - 1, v = min(scores.data[, 1]) - 1, col = 1)
points(scores.data[, 1], rep(min(scores.data[, 2]) - 1, n), col = col.lab, pch = 19)
points(rep(min(scores.data[, 1]) - 1, n), scores.data[, 2], col = col.lab, pch = 19)
abline(h = 0, v = 0, lty = 2, col = 'grey')
legend('topright', levels(factor(data.label[])), fill = c('red', 'red', 'red', 'blue', 'blue', 'blue'), bty = 'n')
legend('topright', levels(factor(data.label[])), fill = c('red', 'red', 'green', 'green', 'blue', 'blue'), bty = 'n')
# We can see that bigger monsters tend to have an overall (PC1) higher than the smaller ones;
# the majority of the bigger ones have a negative PC2 too, leading to the conclusion that they have
# higher physical attibutes and lower ability/intelligence.
# On the contrary, small monsters tend not to have a high overall, but they are more specialized in 
# ability tasks, losing points over physical characteristics


# d) ----------------------------------------------------------------------

target <- scores.data[which(data.label == "Tiny" | data.label == "Huge"), c(1, 2)]
data.label <- data.label[which(data.label == "Tiny" | data.label == "Huge")]

groups <- factor(data.label, labels = c('H','T'))
col.lab <- ifelse(groups %in% c("H"), 'red', 'blue')

plot(target, col = col.lab, pch = 19, asp = 1)

dat <- data.frame(x = target, y = groups)
svmfit <- svm(y~., data = dat, kernel = 'linear', cost = 1, scale = FALSE)
summary(svmfit)

length(svmfit$index)
# 5

n.g <- 100

xgrid <- expand.grid(x.1 = seq(from = range(dat[, 1])[1], to = range(dat[, 1])[2],length = n.g),
                     x.2 = seq(from = range(dat[, 2])[1], to = range(dat[, 2])[2],length = n.g))
colnames(xgrid) <- colnames(dat)[1:2]
ygrid <- predict(svmfit, xgrid)
ygrid <- factor(ygrid, labels = c(0, 1))

plot(target, col = col.lab ,pch = 19)
contour(seq(from = range(dat[, 1])[1], to = range(dat[, 1])[2], length = n.g),
        seq(from = range(dat[, 2])[1], to = range(dat[, 2])[2], length = n.g),
        matrix(as.numeric(ygrid), n.g, n.g), level = 1.5, add = TRUE, drawlabels = F)

new.datum <- c(14, 
               50, 
               19, 
               10, 
               16, 
               8, 
               12, 
               13)

new.datum.std <- (new.datum - colMeans(data)) / sapply(data, sd)

new.datum.std.score <- t(load.data) %*% (new.datum.std - colMeans(data.sd))
new.datum.std.score

points(new.datum.std.score[1], new.datum.std.score[2], col = 'black', pch = 19)

testdat <- data.frame(x = cbind(new.datum.std.score[1], new.datum.std.score[2]))
colnames(testdat)[1:2] <- colnames(dat)[1:2]

ypred <- predict(svmfit, testdat)
ypred
# Huge
