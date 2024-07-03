###----------------------###
### Problem 2 (20220616) ###
###----------------------###

library(MVN)
library(MASS)
library(e1071)

rm(list = ls())
graphics.off()

data <- read.table('musicCountry.txt', header = T)
head(data)

groups <- factor(data$release.country, labels = c('N','P')) # N: Germany, P: USA
col.lab <- ifelse(groups %in% c("P"), 'red', 'blue') # blue: Germany, red: USA

neg <- which(groups == "N")
pos <- which(groups == "P")

n <- dim(data)[1]


# a) ----------------------------------------------------------------------

target <- data[, 1:2]
p <- dim(target)[2]

plot(target, col = col.lab, pch = 20)

mvn(target[neg, ])$multivariateNormality
mvn(target[pos, ])$multivariateNormality
# p-values: 0.4363344, 0.869017

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

summary(boxM(target, groups)) #!library(heplots)
boxM(target, groups)$p.value
# 2.837295e-12 -> different covariance structure -> QDA

list(SF = cov(target[neg, ]), ST = cov(target[pos, ]))
abs(cov(target[neg, ])/cov(target[pos, ]))

p.pos <- 0.9
p.neg <- 1-0.9
prior = c(p.neg, p.pos)
prior

qda <- lda(target, groups, prior = prior)
qda

qda$means
#      price average.length
# N 59.82982       5.891951
# P 29.74888       4.377556

list(SF = cov(target[neg, ]), ST = cov(target[pos, ]))
# $SF
#                     price average.length
# price          108.936681      -7.621866
# average.length  -7.621866       8.505943

# $ST
#                    price average.length
# price          173.53448      -1.524040
# average.length  -1.52404       1.543192

plot(target[, 1:2], xlab = colnames(target)[1], ylab = colnames(target)[2], pch = '')
points(target[neg, 1:2], col = 'blue', pch = 20)
points(target[pos, 1:2], col = 'red', pch = 20)
legend('topright', legend = levels(groups), fill = c('blue', 'red'), cex = .7)

points(qda$means[, 1:2], pch = 4, col = c('blue', 'red') , lwd = 2, cex = 1.5)

x <- seq(min(target[, 1]), max(target[, 1]), length = 200)
y <- seq(min(target[, 2]), max(target[, 2]), length = 200)
xy <- expand.grid(V1 = x, V2 = y)

colnames(xy) <- colnames(target)

post <- predict(qda, xy)$post  
post1 <- post[, 1] - post[, 2] 
post2 <- post[, 2] - post[, 1]  

contour(x, y, matrix(post1, 200), levels = 0, drawlabels = F, add = T)  
contour(x, y, matrix(post2, 200), levels = 0, drawlabels = F, add = T)


# b) ----------------------------------------------------------------------

G <- 2

qdaCV <- qda(target, groups, CV = TRUE, prior = prior)

miscCV <- table(class.true = groups, class.assigned = qdaCV$class)
AERCV <- 0
for(g in 1:G)
  AERCV <- AERCV + sum(miscCV[g,-g])/sum(miscCV[g,]) * prior[g] # priors NON adjusted! p(pos)p(miscl.|pos) + p(neg)p(miscl.|neg)

print(paste("AERCV:", AERCV))
# 0.0486842105263158


# c) ----------------------------------------------------------------------

miscCV

TN <- miscCV[1, 1]
FN <- miscCV[2, 1]
FP <- miscCV[1, 2]
TP <- miscCV[2, 2]

(FP / (TN + FP)) * p.neg + (TP / (TP + FN)) * p.pos
# 0.9013158


# d) ----------------------------------------------------------------------

new.datum <- c(50, 3.5)

qda.pred <- predict(qda, new.datum)
qda.pred$class
# USA


# e) ----------------------------------------------------------------------

dat <- data.frame(x = target, y = groups)

set.seed(20220616)
tune.out <- tune(svm, y~., data = dat, kernel = 'linear',
                 ranges = list(cost = c(0.001 , 0.01, 0.1, 1, 10, 100)))
summary(tune.out)$best.model$cost
# - best parameters:
#  cost
#    10

summary(tune.out)$best.model$cost
tune.out$best.model$cost
# 10

bestmod <- tune.out$best.model
summary(bestmod)

bestmod$cost
# 10

n.g <- 100

xgrid <- expand.grid(x.1 = seq(from = range(dat[, 1])[1], to = range(dat[, 1])[2],length = n.g),
                     x.2 = seq(from = range(dat[, 2])[1], to = range(dat[, 2])[2],length = n.g))
colnames(xgrid) <- colnames(dat)[1:2]
ygrid <- predict(bestmod, xgrid)
ygrid <- factor(ygrid, labels = c(0, 1)) # not necessary afterwards

plot(target, col = col.lab ,pch = 19)
contour(seq(from = range(dat[, 1])[1], to = range(dat[, 1])[2], length = n.g),
        seq(from = range(dat[, 2])[1], to = range(dat[, 2])[2], length = n.g),
        matrix(as.numeric(ygrid), n.g, n.g), level = 1.5, add = TRUE, drawlabels = F)

testdat <- data.frame(x = cbind(50, 3.5))
colnames(testdat)[1:2] <- colnames(dat)[1:2]

ypred <- predict(bestmod, testdat)
ypred
# USA

