###---------------------------###
### SUPERVISED CLASSIFICATION ###----------------------------------------------------------
###---------------------------###

### Try It Out! ###
rm(list = ls())
graphics.off()
par(mfrow=c(1,1))
cat("\014")
data <- read.table('datasets/data_classification.txt', header = T)
data <- data[, c(1, 2, 5)]
### Try It Out! ###

groups <- factor(data$class, labels = c('N','P'))
col.lab <- ifelse(groups %in% c("P"), 'red', 'blue')

neg <- which(groups == "N")
pos <- which(groups == "P")

n <- dim(data)[1]

#### LDA & QDA ----

##### Univariate Case -----

### Try It Out! ###
data <- data[, c(2, 3)]
### Try It Out! ###

target <- data$target

plot(target, rep(0, n), col = col.lab, pch = 20)


###### Verify Assumptions ------

# 1) normality (univariate) within the groups (although LDA is robust to it)
shapiro.test(target[neg])
shapiro.test(target[pos])

# 2) equal variance (univariate)
var.test(target[neg], target[pos])
# If OK -> LDA, else -> QDA


###### Perform Classification ------

c.np <- 100000
c.pn <- 500

p.pos <- 0.001
p.neg <- 1-0.001
prior = c(p.neg, p.pos)
prior

prior.c <- c(p.neg*c.pn/(c.np*p.pos+c.pn*p.neg), p.pos*c.np/(c.np*p.pos+c.pn*p.neg))
prior.c

x <- data.frame(target = seq(range(target)[1] - diff(range(target))/5, range(target)[2] + diff(range(target))/5, diff(range(target))/100))

# Without priors

lda <- lda(data.frame(target), groups) #!library(MASS)
lda
# Note: if we don't specify the prior probabilities, they are estimated from the sample

# Posterior probability for a grid of x's

head(predict(lda, x)$posterior)
LDA.N <- predict(lda, x)$posterior[, 1]
LDA.P <- predict(lda, x)$posterior[, 2]

plot(target[neg], rep(0, length(neg)), pch = 16, col = 'blue', xlim = range(x), ylim = c(0, 1),
     xlab = 'x', ylab = 'estimated posterior', main = "LDA")
points(target[pos], rep(0, length(pos)), pch = 16, col = 'red')
abline(v = mean(range(target)), col = 'grey')
points(c(mean(range(target)), mean(range(target))), c(predict(lda, data.frame(target = mean(range(target))))$posterior),
       col = c('blue', 'red'), pch = '*', cex = 2.5)

predict(lda, x)$class
predict(lda, mean(range(target)))$class

lines(x[, 1], LDA.N, type = 'l', col = 'blue',
      xlab = 'x', ylab = 'estimated posterior', main = "LDA")
lines(x[, 1], LDA.P, type = 'l', col = 'red')
abline(h = 0.5)
legend(range(target)[1], 1, legend = c('P(NEG|X=x)', 'P(POS|X=x)'), fill = c('blue', 'red'), cex = 0.7)

# Set prior probabilities

lda.p <- lda(data.frame(target), groups, prior = prior.c)
lda.p

LDA.N.p <- predict(lda.p, x)$posterior[,1]
LDA.P.p <- predict(lda.p, x)$posterior[,2]

plot(x[, 1], LDA.N.p, type = 'l', col = 'blue', xlim = range(x), ylim = c(0,1),
     xlab = 'x', ylab = 'estimated posterior', main = "LDA")
points(x[ ,1], LDA.P.p, type = 'l', col = 'red')
abline(h = 0.5)
points(target[neg], rep(0, length(neg)), pch = 16, col = 'blue')
points(target[pos], rep(0, length(pos)), pch = 16, col = 'red')
abline(v = mean(range(target)), col = 'grey')
points(c(mean(range(target)), mean(range(target))), c(predict(lda.p, data.frame(target = mean(range(target))))$posterior),
       col = c('blue', 'red'), pch = '*', cex = 2.5)

points(x[, 1], LDA.N, type = 'l', col = 'grey')
points(x[, 1], LDA.P, type = 'l', col = 'grey')
legend(range(target)[1], 1, legend = c('P(NEG|X=x)', 'P(POS|X=x)'), fill = c('blue', 'red'), cex = 0.7)


##### Multivariate Case -----

target <- data[, 1:2]
p <- dim(target)[2]

plot(target, col = col.lab, pch = 20)


###### Verify Assumptions ------

mvn(target[neg, ])$multivariateNormality
mvn(target[pos, ])$multivariateNormality

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

list(SN = cov(target[neg, ]), SP = cov(target[pos, ]))
abs(cov(target[neg, ])/cov(target[pos, ]))


###### Perform Classification ------

c.np <- 100000
c.pn <- 500

p.pos <- 0.001
p.neg <- 1-0.001
prior = c(p.neg, p.pos) # ATTENTION! The order reflects the one specified when we factorized groups at the beginning 
prior

prior.c <- c(p.neg*c.pn/(c.np*p.pos+c.pn*p.neg), p.pos*c.np/(c.np*p.pos+c.pn*p.neg))
prior.c

lda <- lda(target, groups, prior = prior.c)
lda # Always control that the priors are correct

###### Parameters Estimation ------

lda$means
colMeans(target[pos, ])
colMeans(target[neg, ])

Spooled <- 1/(n-2) * ((cov(target[neg, ])) * (table(groups)["N"] - 1) + (cov(target[pos, ])) * (table(groups)["P"] - 1))
Spooled # for LDA

list(SN = cov(target[neg, ]), SP = cov(target[pos, ])) # for QDA

lda$prior


###### Evaluation ------

lda.pred <- predict(lda, target)

G <- 2

misc <- table(class.true = groups, class.assigned = lda.pred$class)
APER <- 0
for(g in 1:G)
  APER <- APER + sum(misc[g,-g])/sum(misc[g,]) * prior[g] # priors NON adjusted! p(pos)p(miscl.|pos) + p(neg)p(miscl.|neg)

print(paste("APER:", APER))

# or (if we estimate the priors through the sample frequencies)

errors <- (lda.pred$class != groups)

APER   <- sum(errors) / length(groups)
APER

# -

ldaCV <- lda(target, groups, CV = TRUE, prior = prior.c)

miscCV <- table(class.true = groups, class.assigned = ldaCV$class)
AERCV <- 0
for(g in 1:G)
  AERCV <- AERCV + sum(miscCV[g,-g])/sum(miscCV[g,]) * prior[g] # priors NON adjusted! p(pos)p(miscl.|pos) + p(neg)p(miscl.|neg)

print(paste("AERCV:", AERCV))

# or (if we estimate the priors through the sample frequencies)

errorsCV <- (ldaCV$class != groups)

AERCV <- sum(errorsCV) / length(groups)
AERCV


###### Plot Classification Regions ------

# (Second variable against the first)

plot(target[, 1:2], xlab = colnames(target)[1], ylab = colnames(target)[2], pch = '')
points(target[neg, 1:2], col = 'blue', pch = 20)
points(target[pos, 1:2], col = 'red', pch = 20)
legend('topright', legend = levels(groups), fill = c('blue', 'red'), cex = .7)

points(lda$means[, 1:2], pch = 4, col = c('blue', 'red') , lwd = 2, cex = 1.5)

x <- seq(min(target[, 1]), max(target[, 1]), length = 200)
y <- seq(min(target[, 2]), max(target[, 2]), length = 200)
xy <- expand.grid(V1 = x, V2 = y)

xy$V3 <- mean(target[, 3]) # !!! Do it only if p > 2 !!! #
xy$V4 <- mean(target[, 4]) # !!! Do it only if p > 2 !!! #
# Fix reasonably the coordinates of variables you don't want to plot 

colnames(xy) <- colnames(target)

post <- predict(lda, xy)$post  
post1 <- post[, 1] - post[, 2] 
post2 <- post[, 2] - post[, 1]  

contour(x, y, matrix(post1, 200), levels = 0, drawlabels = F, add = T)  
contour(x, y, matrix(post2, 200), levels = 0, drawlabels = F, add = T)


##### Estimates -----

miscCV

TN <- miscCV[1, 1]
FN <- miscCV[2, 1]
FP <- miscCV[1, 2]
TP <- miscCV[2, 2]

rbind("Probability to classify as P", ((FP / (TN + FP)) * p.neg + (TP / (TP + FN)) * p.pos))

total <- 200

rbind("Budget", ((FP / (TN + FP)) * total * p.neg + (TP / (TP + FN)) * total * p.pos) * c.pn)

prev_strategy_cost <- total * c.pn
cur_strategy_cost <- (FP / (TN + FP)) * total * p.neg * c.pn + (FN / (TP + FN)) * total * p.pos * c.np

rbind("Savings", prev_strategy_cost - cur_strategy_cost)


##### Prediction -----

new.datum <- c(6.75, 4.75)

points(new.datum[1], new.datum[2], pch = 17, cex = 1.5)

lda.pred <- predict(lda, new.datum)
lda.pred$class


#### k-Nearest Neighbors (kNN) ----

##### Univariate Case -----

### Try It Out! ###
data <- data[, c(2, 3)]
### Try It Out! ###

target <- data$target

plot(target, rep(0, n), col = col.lab, pch = 20)

x <- data.frame(target = seq(range(target)[1] - diff(range(target))/5, range(target)[2] + diff(range(target))/5, diff(range(target))/100))

k <- 3
knn <- knn(train = target, test = x, cl = groups, k = k, prob = T) #!library(class)
knn.class <- knn == 'P'
knn.P <- ifelse(knn.class == 1, attributes(knn)$prob, 1 - attributes(knn)$prob)

plot(x[, 1], LDA.P, type = 'l', col = 'red', lty = 2, xlab = 'x', ylab = 'Estimated Posterior')
points(x[, 1], knn.P, type = 'l', col = 'black', lty = 1)
abline(h = 0.5)
legend(range(target)[1] - diff(range(target))/5, 0.25, legend = c('LDA', 'knn'), lty = c(2, 1), col = c('red','black'))

# let's change k
par(mfrow=c(3, 4))
for(k in 1:12)
{
  knn <- knn(train = target, test = x, cl = groups, k = k, prob = T)
  knn.class <- knn == 'P'
  knn.P <- ifelse(knn.class == 1, attributes(knn)$prob, 1 - attributes(knn)$prob)
  
  plot(x[, 1], LDA.P, type = 'l', col = 'red', lty = 2, xlab = 'x', ylab = 'Estimated Posterior', main = k)
  points(x[, 1], knn.P, type = 'l', col = 'black', lty = 1, lwd = 2)
  abline(h = 0.5)
}
par(mfrow=c(1,1))


##### Bivariate Case -----

target <- data[, 1:2]
p <- dim(target)[2]

plot(target, col = col.lab, pch = 20)

x <- seq(min(target[, 1]), max(target[, 1]), length = 200)
y <- seq(min(target[, 2]), max(target[, 2]), length = 200)
xy <- expand.grid(V1 = x, V2 = y)

k <- 3
knn <- knn(train = target, test = xy, cl = groups, k = k)

z <- as.numeric(knn)

contour(x, y, matrix(z, 200), levels = c(1.5), drawlabels = F, add = T)


##### Cross-Validation -----

set.seed(9)
AERkCV <- NULL
for(k in 5:20)
{
  knnCV <- knn.cv(train = target, cl = groups, k = k)
  
  errorskCV <- (knnCV != groups)
  AERkCV[k-(5-1)] <- sum(errorskCV) / length(groups)
}
AERkCV

min(AERkCV)
which(AERkCV == min(AERkCV)) # -> k = result + (5-1)


##### Prediction -----

new.datum <- c(6.75, 4.75)

points(new.datum[1], new.datum[2], pch = 17, cex = 1.5)

knn.pred <- knn(train = target, test = new.datum, cl = groups, k = 3)
knn.pred


#### Support Vector Machines (SVM) ----

##### Linear Case -----

target <- data[, 1:2]

# The classes are not separable
plot(target, col = col.lab, pch = 19, asp = 1)

# Fit the Support Vector Classifier (kernel = "linear")
dat <- data.frame(x = target, y = groups)
svmfit <- svm(y~., data = dat, kernel = 'linear', cost = 10, scale = FALSE) #!library(e1071)
summary(svmfit)

par(mfrow=c(1,2))
plot(svmfit, dat, col = c('salmon', 'light blue'), pch = 19)
par(mfrow=c(1,1))

# Support vectors are indicated with crosses.
# They are:
svmfit$index

n.g <- 100

xgrid <- expand.grid(x.1 = seq(from = range(dat[, 1])[1], to = range(dat[, 1])[2], length = n.g),
                     x.2 = seq(from = range(dat[, 2])[1], to = range(dat[, 2])[2], length = n.g))
colnames(xgrid) <- colnames(dat)[1:2]
ygrid <- predict(svmfit, xgrid)
ygrid <- factor(ygrid, labels = c(0, 1)) # not necessary afterwards

plot(xgrid, col = c("blue", "red")[as.numeric(ygrid)], pch = 20, cex = .2)
points(target, col = col.lab, pch = 19)
points(target[svmfit$index, ], pch = 5, cex = 2)

plot(target, col = col.lab , pch = 19)
contour(seq(from = range(dat[, 1])[1], to = range(dat[, 1])[2], length = n.g),
        seq(from = range(dat[, 2])[1], to = range(dat[, 2])[2], length = n.g),
        matrix(as.numeric(ygrid), n.g, n.g), level = 1.5, add = TRUE, drawlabels = F)

# Prediction for a new observation (command predict())

testdat <- data.frame(x = cbind(6.75, 4.75) , y = as.factor("N")) # y is usually not specified
colnames(testdat)[1:2] <- colnames(dat)[1:2]

ypred <- predict(svmfit, testdat)
ypred

table(true.label = testdat$y, assigned.label = ypred) # if testdat$y is specified

points(testdat[, 1:2], col = c("blue", "red")[as.numeric(testdat$y)], pch = 4, lwd = 2, cex = 1.5)

# To set the parameter C we can use the function tune(),
# which is based on cross-validation (10-fold)
set.seed(1)
tune.out <- tune(svm, y~., data = dat, kernel = 'linear',
                 ranges = list(cost = c(0.001 , 0.01, 0.1, 1, 5, 10, 100)))
summary(tune.out)

summary(tune.out)$best.model$cost
tune.out$best.model$cost

# Extract the best model from the result of tune
bestmod <- tune.out$best.model
summary(bestmod)

bestmod$cost

# If the classes are separable, setting a high value for the cost function
# leads to the maximal margin classifier (i.e., it returns the classification
# provided by the best separating hyperplane)


##### Non-linear Case -----

target <- data[, 1:2]

plot(target, col = col.lab, pch = 19, asp = 1)

# Fit the Support Vector Classifier (kernel = "radial") given a cost C
dat <- data.frame(x = target, y = groups)
svmfit <- svm(y~., data = dat, kernel = 'radial', gamma = 1, cost = 10)
summary(svmfit)

par(mfrow=c(1,2))
plot(svmfit, dat, col = c('salmon', 'light blue'), pch = 19)
par(mfrow=c(1,1))