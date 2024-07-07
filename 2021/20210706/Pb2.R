###----------------------###
### Problem 2 (20210706) ###
###----------------------###

library(MVN)
library(heplots)
library(MASS)
library(e1071)

rm(list = ls())
graphics.off()

data <- read.table('orthopaedics.txt', header = T)
head(data)

groups <- factor(data$norm_abnorm, labels = c('P','N')) # P: AB / N: NO
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
# p-values = 0.9774976 and 0.7251014 -> OK

Pvt <- NULL
rownames <- NULL
for(i in 1:p)
{
  Pvt <- rbind(Pvt, var.test(target[neg, i], target[pos, i])$p.value)
  rownames <- cbind(rownames, paste(colnames(target)[i], "_N vs ", colnames(target)[i], "_P", sep = ''))
}
dimnames(Pvt)[[1]] <- rownames
dimnames(Pvt)[[2]] <- c("p-value")
Pvt
#                                 p-value
# incidence_N vs incidence_P 8.000733e-06
# tilt_N vs tilt_P           9.920769e-05
# -> the variances are not equal in the two groups

summary(boxM(target, groups))
boxM(target, groups)$p.value
# 2.135337e-08 -> NOT OK

# Gaussianity and different covariance matrices -> QDA

p.pos <- 0.35
p.neg <- 1-0.35
prior = c(p.pos, p.neg)
prior

qda <- qda(target, groups, prior = prior)
qda

qda$means
#   incidence     tilt
# P  63.56038 19.58847
# N  51.26018 12.89092

list(SN = cov(target[neg, ]), SP = cov(target[pos, ]))
# $SN
#           incidence     tilt
# incidence 109.63118 39.74931
# tilt       39.74931 41.38297
#
# $SP
#           incidence      tilt
# incidence 314.53723  95.15106
# tilt       95.15106 103.33664

qda$prior
#    P    N 
# 0.35 0.65

plot(target[, 1:2], xlab = colnames(target)[1], ylab = colnames(target)[2], pch = '')
points(target[neg, 1:2], col = 'blue', pch = 20)
points(target[pos, 1:2], col = 'red', pch = 20)
legend('topright', legend = levels(groups), fill = c('red', 'blue'), cex = .7)

points(qda$means[, 1:2], pch = 4, col = c('red', 'blue') , lwd = 2, cex = 1.5)

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
# 0.225


# c) ----------------------------------------------------------------------

new.datum <- c(60, 0)

points(new.datum[1], new.datum[2], pch = 19)

qda.pred <- predict(qda, new.datum)
qda.pred$class
# AB


# d) ----------------------------------------------------------------------

dat <- data.frame(x = target, y = groups)
svmfit <- svm(y~., data = dat, kernel = 'linear', cost = 0.1, scale = FALSE)
summary(svmfit)

n.g <- 100

xgrid <- expand.grid(x.1 = seq(from = range(dat[, 1])[1], to = range(dat[, 1])[2], length = n.g),
                     x.2 = seq(from = range(dat[, 2])[1], to = range(dat[, 2])[2], length = n.g))
colnames(xgrid) <- colnames(dat)[1:2]
ygrid <- predict(svmfit, xgrid)
ygrid <- factor(ygrid, labels = c(1, 0))

plot(target, col = col.lab , pch = 19)
contour(seq(from = range(dat[, 1])[1], to = range(dat[, 1])[2], length = n.g),
        seq(from = range(dat[, 2])[1], to = range(dat[, 2])[2], length = n.g),
        matrix(as.numeric(ygrid), n.g, n.g), level = 1.5, add = TRUE, drawlabels = F)

length(svmfit$index)
# 105

# There is no separating hyperplane between the two classes because data overlap;
# using a linear kernel therefore doesn't seem a good choice (a radial one would probably be better), 
# indeed the number of support vectors is really high, meaning bad bias

testdat <- data.frame(x = cbind(60, 0))
colnames(testdat)[1:2] <- colnames(dat)[1:2]

points(new.datum[1], new.datum[2], pch = 5, cex = 1.5)

ypred <- predict(svmfit, testdat)
ypred
# NO


# Extra -------------------------------------------------------------------

svm.pred <- predict(svmfit, dat)
svmfit$fitted # equivalent

G <- 2

misc <- table(true.label = groups, assigned.label = svm.pred)
APER <- 0
for(g in 1:G)
  APER <- APER + sum(misc[g,-g])/sum(misc[g,]) * prior[g]

print(paste("APER:", APER))
# 0.273125

set.seed(20210706) # no need if cross = n (i.e. LOOCV)

svmfitCV <- svm(y~., data = dat, kernel = 'linear', cost = 0.1, cross = 10, scale = FALSE)
summary(svmfitCV)

miscCV <- table(true.label = groups, assigned.label = svmfitCV$fitted)
AERCV <- 0
for(g in 1:G)
  AERCV <- AERCV + sum(miscCV[g,-g])/sum(miscCV[g,]) * prior[g]

print(paste("AERCV:", AERCV))
# 0.273125

# -

svmfit.rad <- svm(y~., data = dat, kernel = 'radial', gamma = 1, cost = 10)
summary(svmfit.rad)

par(mfrow=c(1,2))
plot(svmfit.rad, dat, col = c('salmon', 'light blue'), pch = 19)
par(mfrow=c(1,1))
