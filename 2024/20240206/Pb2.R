library(MASS)
library(class)
library(MVN)

data <- read.table("food.txt",header=TRUE)
features <- data[,1:4]

result <- factor(data$result)

low <- features[which(result=="low"),]
high <- features[which(result=="high"),]


plot(features, pch=19)
plot(low, col='red', pch=19)
plot(high, col='blue', pch=19)

# cost of misclassifications c_ij of attributing to j a unit belonging to i
cost.highlow <- 100000
cost.lowhigh <- 500
# priors
p.low <- 0.001
p.high <-1 - p.low

# not before rescaling the priors in order to account for different misclassification costs
p.low.new <- p.low * cost.highlow / (p.low*cost.highlow + p.high*cost.lowhigh)
p.high.new <- p.high * cost.lowhigh / (p.low*cost.highlow + p.high*cost.lowhigh)

priors.orig <- c(p.high, p.low)

priors <- c( p.high.new, p.low.new)  
priors

# we have two class
# we know LDA,QDA, KNN, SVM
# we need to verify gaussianity and same covariance between groups for LDA
# Assumption of gaussianity

high <- features[result==levels(result)[1],]
low <- features[result==levels(result)[2],]

ps <- c(mvn(high)$multivariateNormality$`p value`,
        mvn(low)$multivariateNormality$`p value`)
ps

# we cannot assume gaussianity at level 5% for the population with low results
# we could however accept it at 1%.

# since we are not so sure about gaussianity i would not use the barlett test.
Shigh <- cov(high)
Slow <- cov(low)
Shigh/Slow
# we can assume same covariance structure since they do not differ by more than a factor
# 10

# we then can use LDA since we have same covariance structure but no gaussianity for one
# group -> we leverage on fisher argument and proceed with LDA.


# just to be sure there is a difference between the two populations
fit <- manova(as.matrix(features) ~ result)
summary(fit)

fit.lda <- lda(features, result, prior=priors)
fit.lda

predictions <-predict(fit.lda, features)
table(class.true=result, class.assigned=predictions$class)

# calculating the APER
G <- 2
misc <- table(class.true=result, class.assigned=predictions$class)
APER <- 0
for(g in 1:G)
 APER <- APER + sum(misc[g,-g])/sum(misc[g,]) * priors.orig[g]
APER

# estimating the AER with Leave one out cross validation
APER.cv <-NULL
fulltable <- cbind(c(0,0),c(0,0))

for(i in 1:dim(features)[1]){
  LdaCV.i <- lda(features[-i,], result[-i], prior=priors)
  prediction <- predict(LdaCV.i, features[i,])
  predicted <- ifelse(prediction$class == "high",1,2)
  g.cv <- ifelse(result[i] == "high",1,2)
  
  if(predicted == g.cv)
    fulltable[g.cv,g.cv]=fulltable[g.cv,g.cv]+1
  if(predicted != g.cv)
    fulltable[g.cv,-g.cv]=fulltable[g.cv,-g.cv]+1
}

AER.cv <- 0
for(g in 1:G)
  AER.cv <- AER.cv + sum(fulltable[g,-g])/sum(fulltable[g,]) * priors.orig[g]
AER.cv

# APER = AER.cv


# to estimate the budget i will use the AERCV that is a better estimate of the actual error
N <- 1000
Nhigh <- N * p.high
Nlow <- N * p.low

budget <- AERCV.lda * Nhigh *cost.lowhigh + AERCV.lda *Nlow*cost.highlow
budget

# with the old strategy
budget.old <- 500*1000
budget.old


# trying QDA due to indecision in covariance structure
fit.qda <- qda(features, result, prior=priors)

predictions.qda <- predict(fit.qda,features)


# APER
G <- 2
misc <- table(class.true=result, class.assigned=predictions.qda$class)
APER.qda <- 0
for(g in 1:G)
  APER.qda <- APER.qda + sum(misc[g,-g])/sum(misc[g,]) * priors.orig[g]
APER.qda

# estimating the AER with Leave one out cross validation
APER.cv.qda <-NULL
fulltable.qda <- cbind(c(0,0),c(0,0))

for(i in 1:dim(features)[1]){
  QdaCV.i <- qda(features[-i,], result[-i], prior=priors)
  prediction.qda <- predict(QdaCV.i, features[i,])
  predicted.qda <- ifelse(prediction.qda$class == "high",1,2)
  g.cv.qda <- ifelse(result[i] == "high",1,2)
  
  if(predicted.qda == g.cv.qda)
    fulltable.qda[g.cv.qda,g.cv.qda]=fulltable.qda[g.cv.qda,g.cv.qda]+1
  if(predicted.qda != g.cv.qda)
    fulltable.qda[g.cv.qda,-g.cv.qda]=fulltable.qda[g.cv.qda,-g.cv.qda]+1
}

AER.cv.qda <- 0
for(g in 1:G)
  AER.cv.qda <- AER.cv.qda + sum(fulltable.qda[g,-g])/sum(fulltable.qda[g,]) * priors.orig[g]
AER.cv.qda
