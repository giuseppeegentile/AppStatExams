###----------------------###
### Problem 2 (20230207) ###
###----------------------###

rm(list = ls())
graphics.off()

library(MVN)
library(MASS)
library(class)

data <- read.table('pressure.txt', header = T)
head(data)

groups <- factor(data$pressure, labels = c('P','N'))
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
# The group of low pressure stations is not gaussian -> LDA

Pvt <- NULL
Pbart <- NULL
rownames <- NULL
for(i in 1:p)
{
  Pvt <- rbind(Pvt, var.test(target[neg, i], target[pos, i])$p.value)
  Pbart <- rbind(Pbart, bartlett.test(target[, i], groups)$p.value)
  rownames <- cbind(rownames, paste(colnames(target)[i], "_F vs ", colnames(target)[i], "_T", sep = ''))
}
dimnames(Pvt)[[1]] <- rownames
dimnames(Pbart)[[1]] <- rownames
dimnames(Pvt)[[2]] <- c("p-value")
dimnames(Pbart)[[2]] <- c("p-value")
Pvt
Pbart

list(SF = cov(target[neg, ]), ST = cov(target[pos, ]))
abs(cov(target[neg, ])/cov(target[pos, ]))
# The variances between groups are reasonably the same -> confirm LDA

lda <- lda(target, groups)
lda

lda$means
colMeans(target[pos, ])
colMeans(target[neg, ])

Spooled <- 1/(n-2) * ((cov(target[neg, ])) * (table(groups)["N"] - 1) + (cov(target[pos, ])) * (table(groups)["P"] - 1))
Spooled

lda$prior

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

# There's no clear separation between data;
# probably a non-linear classifier would perform better,
# but QDA is not an option since we don't satisfy gaussianity assumption

p.pos <- 0.5
p.neg <- 1-0.5
prior = c(p.neg, p.pos)
prior

G <- 2

ldaCV <- lda(target, groups, CV = TRUE)

miscCV <- table(class.true = groups, class.assigned = ldaCV$class)
AERCV <- 0
for(g in 1:G)
  AERCV <- AERCV + sum(miscCV[g,-g])/sum(miscCV[g,]) * prior[g] # priors NON adjusted! p(pos)p(miscl.|pos) + p(neg)p(miscl.|neg)

print(paste("AERCV:", AERCV))

# or

errorsCV <- (ldaCV$class != groups)

AERCV <- sum(errorsCV) / length(groups)
AERCV
# 0.21


# c) ----------------------------------------------------------------------

set.seed(19)
AERkCV <- NULL
for(k in 10:30)
{
  knnCV <- knn.cv(train = target, cl = groups, k = k)
  
  errorskCV <- (knnCV != groups)
  AERkCV[k-9] <- sum(errorskCV) / length(groups)
}
AERkCV

min(AERkCV)
# 0.195
which(AERkCV == min(AERkCV))
# k = 18

plot(target, col = col.lab, pch = 20)

x <- seq(min(target[, 1]), max(target[, 1]), length = 200)
y <- seq(min(target[, 2]), max(target[, 2]), length = 200)
xy <- expand.grid(V1 = x, V2 = y)

k <- 18
knn <- knn(train = target, test = xy, cl = groups, k = k)

z <- as.numeric(knn)

contour(x, y, matrix(z, 200), levels = c(1.5), drawlabels = F, add = T)


# d) ----------------------------------------------------------------------

new.datum <- c(10.8, 39.4)

knn.pred <- knn(train = target, test = new.datum, cl = groups, k = k)
knn.pred
# L

lda.pred <- predict(lda, new.datum)
lda.pred$class
# H