library(MASS)
library(class)
library(MVN)

data <- read.table("food.txt",header=TRUE)
features <- data[,1:4]

# cost of misclassifications c_ij of attributing to j a unit belonging to i
cost.highlow <- 100000
cost.lowhigh <- 500
# priors
p.low <- 0.001
p.high <-1 - p.low

# not before rescaling the priors in order to account for different misclassification costs
p.low.new <- p.low * cost.highlow / (p.low*cost.highlow + p.high*cost.lowhigh)
p.high.new <- p.high * cost.lowhigh / (p.low*cost.highlow + p.high*cost.lowhigh)

priors <- c( p.high.new, p.low.new)  
priors

# we have two class
# we know LDA,QDA, KNN, SVM
# we need to verify gaussianity and same covariance between groups for LDA
# Assumption of gaussianity

result <- factor(data$result)

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
 APER <- APER + sum(misc[g,-g])/sum(misc[g,]) * priors[g]
APER

# estimating the AER with Leave one out cross validation
errors_CV <- 0
for(i in 1:60){
  LdaCV.i <- lda(features[-i,], result[-i], prior=priors)
  errors_CV <- errors_CV + as.numeric(predict(LdaCV.i,features[i,])$class != result[i])
}
errors_CV

AERCV   <- sum(errors_CV)/length(result)
AERCV
# 30% of the time i will do an error


# to estimate the budget i will use the AERCV that is a better estimate of the actual error
N <- 1000
Nhigh <- N * p.high
Nlow <- N * p.low

budget <- AERCV * Nhigh *cost.lowhigh + AERCV *Nlow*cost.highlow
budget

# with the old strategy
budget.old <- 500*1000
budget.old
