###----------------------###
### Problem 2 (20220119) ###
###----------------------###

library(MVN)
library(MASS)
library(class)

rm(list = ls())
graphics.off()

data <- read.table('fish.txt', header = T)
head(data)

groups <- factor(data$abundance, labels = c('P','N')) # P: H / N: L
col.lab <- ifelse(groups %in% c("P"), 'red', 'blue')

neg <- which(groups == "N")
pos <- which(groups == "P")

n <- dim(data)[1]


# a) ----------------------------------------------------------------------

target <- data[, 1:2]
p <- dim(target)[2]

plot(target, col = col.lab, pch = 20)

mvn(target[neg, ])$multivariateNormality
mvn(target[pos, ])$multivariateNormality
# p-values = 1.02481e-05 and 0.1855965 -> NOT OK (due to neg group)

list(SN = cov(target[neg, ]), SP = cov(target[pos, ]))
abs(cov(target[neg, ])/cov(target[pos, ]))
#          x        y
# x 1.125772 3.471280
# y 3.471280 6.824064

# Even the covariance matrices doesn't seem much the same...

# ... since Gaussianity assumption is not respected, let's try with LDA

lda <- lda(target, groups)
lda

lda$means
#          x        y
# P 10.61727 39.92299
# N 12.14345 39.56668

Spooled <- 1/(n-2) * ((cov(target[neg, ])) * (table(groups)["N"] - 1) + (cov(target[pos, ])) * (table(groups)["P"] - 1))
Spooled
#             x           y
# x  0.48318432 -0.09005061
# y -0.09005061  0.06950479

lda$prior
#    P    N 
# 0.48 0.52

plot(target[, 1:2], xlab = colnames(target)[1], ylab = colnames(target)[2], pch = '')
points(target[neg, 1:2], col = 'blue', pch = 20)
points(target[pos, 1:2], col = 'red', pch = 20)
legend('topright', legend = levels(groups), fill = c('red', 'blue'), cex = .7)

points(lda$means[, 1:2], pch = 4, col = c('red', 'blue') , lwd = 2, cex = 1.5)

x <- seq(min(target[, 1]), max(target[, 1]), length = 200)
y <- seq(min(target[, 2]), max(target[, 2]), length = 200)
xy <- expand.grid(V1 = x, V2 = y)

colnames(xy) <- colnames(target)

post <- predict(lda, xy)$post  
post1 <- post[, 1] - post[, 2] 
post2 <- post[, 2] - post[, 1]  

contour(x, y, matrix(post1, 200), levels = 0, drawlabels = F, add = T)  
contour(x, y, matrix(post2, 200), levels = 0, drawlabels = F, add = T)


# b) ----------------------------------------------------------------------

# Data overlaps and with a linear classifier we can't be much effective
# -> we won't obtain a sufficiently satisfying separation between the classes
# -> we should try with something non-linear (QDA)

G <- 2

ldaCV <- lda(target, groups, CV = TRUE)

miscCV <- table(class.true = groups, class.assigned = ldaCV$class)
AERCV <- 0
for(g in 1:G)
  AERCV <- AERCV + sum(miscCV[g,-g])/sum(miscCV[g,]) * lda$prior[g]

print(paste("AERCV:", AERCV))
# 0.148


# c) ----------------------------------------------------------------------

set.seed(19)
AERkCV <- NULL
for(k in 10:30)
{
  knnCV <- knn.cv(train = target, cl = groups, k = k)
  
  errorskCV <- (knnCV != groups)
  AERkCV[k-(10-1)] <- sum(errorskCV) / length(groups)
}
AERkCV

min(AERkCV)
# 0.128

which(AERkCV == min(AERkCV))
# 13

k <- 13
knn <- knn(train = target, test = xy, cl = groups, k = k)

z <- as.numeric(knn)

plot(target[, 1:2], xlab = colnames(target)[1], ylab = colnames(target)[2], pch = '')
points(target[neg, 1:2], col = 'blue', pch = 20)
points(target[pos, 1:2], col = 'red', pch = 20)
legend('topright', legend = levels(groups), fill = c('red', 'blue'), cex = .7)

contour(x, y, matrix(z, 200), levels = c(1.5), drawlabels = F, add = T)


# d) ----------------------------------------------------------------------

new.datum <- c(10.8, 39.4)

points(new.datum[1], new.datum[2], pch = 19)

knn.pred <- knn(train = target, test = new.datum, cl = groups, k = k)
knn.pred
# H


# Extra -------------------------------------------------------------------

# QDA

qda <- qda(target, groups)
qda

qda$means
#          x        y
# P 10.61727 39.92299
# N 12.14345 39.56668

list(SN = cov(target[neg, ]), SP = cov(target[pos, ]))
# $SN
#            x          y
# x  0.5105541 -0.1367735
# y -0.1367735  0.1177096
#
# $SP
#             x           y
# x  0.45351455 -0.03940145
# y -0.03940145  0.01724919

qda$prior
#    P    N 
# 0.48 0.52

plot(target[, 1:2], xlab = colnames(target)[1], ylab = colnames(target)[2], pch = '')
points(target[neg, 1:2], col = 'blue', pch = 20)
points(target[pos, 1:2], col = 'red', pch = 20)
legend('topright', legend = levels(groups), fill = c('red', 'blue'), cex = .7)

points(qda$means[, 1:2], pch = 4, col = c('red', 'blue') , lwd = 2, cex = 1.5)

post <- predict(qda, xy)$post  
post1 <- post[, 1] - post[, 2] 
post2 <- post[, 2] - post[, 1]  

contour(x, y, matrix(post1, 200), levels = 0, drawlabels = F, add = T)  
contour(x, y, matrix(post2, 200), levels = 0, drawlabels = F, add = T)

G <- 2

qdaCV <- qda(target, groups, CV = TRUE)

miscCV <- table(class.true = groups, class.assigned = qdaCV$class)
AERCV <- 0
for(g in 1:G)
  AERCV <- AERCV + sum(miscCV[g,-g])/sum(miscCV[g,]) * qda$prior[g]

print(paste("AERCV:", AERCV))
# 0.1

new.datum <- c(10.8, 39.4)

points(new.datum[1], new.datum[2], pch = 19)

qda.pred <- predict(qda, new.datum)
qda.pred$class
# L