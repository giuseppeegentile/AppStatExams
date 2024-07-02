###----------------------###
### Problem 2 (20220909) ###
###----------------------###

library(MVN)
library(heplots)
library(MASS)
library(class)

rm(list = ls())
graphics.off()

data <- read.table('fossil.txt', header = T)
head(data)

groups <- factor(data$type, labels = c('N','P'))
col.lab <- ifelse(groups %in% c("P"), 'red', 'blue')

neg <- which(groups == "N")
pos <- which(groups == "P")

n <- dim(data)[1]

target <- data[, 1:2]
p <- dim(target)[2]

plot(target, col = col.lab, pch = 20)


# a) ----------------------------------------------------------------------

mvn(target[neg, ])$multivariateNormality
mvn(target[pos, ])$multivariateNormality
# OK

Pvt <- NULL
Pbart <- NULL
rownames <- NULL
for(i in 1:p)
{
  Pvt <- rbind(Pvt, var.test(target[neg, i], target[pos, i])$p.value)
  Pbart <- rbind(Pbart, bartlett.test(target[, i], groups)$p.value)
  rownames <- cbind(rownames, paste(colnames(target)[i], "_N vs ", colnames(target)[i], "_P", sep = ''))
}
dimnames(Pvt)[[1]] <- rownames
dimnames(Pbart)[[1]] <- rownames
dimnames(Pvt)[[2]] <- c("p-value")
dimnames(Pbart)[[2]] <- c("p-value")
Pvt
Pbart
# Note: when groups are 2, var.test is basically the same as bartlett.test!

summary(boxM(target, groups))
boxM(target, groups)$p.value

list(SF = cov(target[neg, ]), ST = cov(target[pos, ]))
abs(cov(target[neg, ])/cov(target[pos, ]))
#OK -> LDA

lda <- lda(target, groups)
lda

lda$means
colMeans(target[pos, ])
colMeans(target[neg, ])
#         lon      lat
# N -159.5653 21.89723
# P -159.4089 22.10339

Spooled <- 1/(n-2) * ((cov(target[neg, ])) * (table(groups)["N"] - 1) + (cov(target[pos, ])) * (table(groups)["P"] - 1))
Spooled
#             lon         lat
# lon  0.02046427 -0.00142242
# lat -0.00142242  0.00960570

plot(target[, 1:2], xlab = colnames(target)[1], ylab = colnames(target)[2], pch = '')
points(target[neg, 1:2], col = 'blue', pch = 20)
points(target[pos, 1:2], col = 'red', pch = 20)
legend('topright', legend = levels(groups), fill = c('blue', 'red'), cex = .7)

points(lda$means[, 1:2], pch = 4, col = c('blue', 'red') , lwd = 2, cex = 1.5)

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

lda.pred <- predict(lda, target)

errors <- (lda.pred$class != groups)

APER <- sum(errors) / length(groups)
APER
# 0.08241758

ldaCV <- lda(target, groups, CV = TRUE)

errorsCV <- (ldaCV$class != groups)

AERCV <- sum(errorsCV) / length(groups)
AERCV
# 0.08791209


# c) ----------------------------------------------------------------------

set.seed(9)
AERkCV <- NULL
for(k in 5:20)
{
  knnCV <- knn.cv(train = target, cl = groups, k = k)
  
  errorskCV <- (knnCV != groups)
  AERkCV[k-4] <- sum(errorskCV) / length(groups)
}
AERkCV

min(AERkCV)
# 0.08791209
which(AERkCV == min(AERkCV))
# k = 12/18

plot(target, col = col.lab, pch = 20)

k <- 12
knn <- knn(train = target, test = xy, cl = groups, k = k)

z <- as.numeric(knn)

contour(x, y, matrix(z, 200), levels = c(1.5), drawlabels = F, add = T)

plot(target, col = col.lab, pch = 20)

k <- 18
knn <- knn(train = target, test = xy, cl = groups, k = k)

z <- as.numeric(knn)

contour(x, y, matrix(z, 200), levels = c(1.5), drawlabels = F, add = T)


# d) ----------------------------------------------------------------------

new.datum <- c(-159.5, 21.9)

knn.pred <- knn(train = target, test = new.datum, cl = groups, k = 12)
knn.pred
# nerinea

knn.pred <- knn(train = target, test = new.datum, cl = groups, k = 18)
knn.pred
# nerinea

lda.pred <- predict(lda, new.datum)
lda.pred$class
# nerinea

