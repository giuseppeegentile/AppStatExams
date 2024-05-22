setwd("C:/Users/Giuseppe/Documents/.magistrale/secondo anno/AppStat/Exams/2023/20230904/20230904")
library(MASS)
library(class)
library(rgl)
library(mvtnorm)
library(MVN)
options(rgl.printRglwidget = TRUE)


data <- read.table('doping.txt', header = T)
head(data)

dim(data)

data.feats <- data[,1:4]
head(data.feats)

# true <- doped
# false <- clean


# misclassification costs
c.tf <- 1000      # predict as doped when they are clean cost 1k (the cost of blood exam, when is not needed)
c.ft <- 50000     # predict as clean when they are doped cost 50k

#prior probabilities
pt <- 0.01    # percentage of doped
pf <- 1-pt    
prior = c(pt, pf)
prior

# Prior modified to account for the misclassification costs
prior.c <- c(pt*c.ft/(c.tf*pf+c.ft*pt), pf*c.tf/(c.tf*pf+c.ft*pt))
prior.c

# a) we have knowledge of priors and cost of misclassification
# we can use either LDA/QDA -> check their assumptions


groups.name <- factor(data$result)
g = length(levels(groups.name)) 
g
i1 <- which(groups.name == levels(groups.name)[1])
i2 <- which(groups.name == levels(groups.name)[2])
n1 <- length(i1)
n2 <- length(i2)
n <- n1 + n2



{
  colors <- rep('blue', nrow(data.feats))
  colors[i1] <- 'red'
  colors[i2] <- 'green'
  pairs(data.feats, pch=19, col=colors)
}


data.reduced <- data.feats
# Assumptions
{
  # multivariate normality in each group
  mvn(data[which(data$result == levels(groups.name)[1]), 1:4])$multivariateNormality
  mvn(data[which(data$result == levels(groups.name)[2]), 1:4])$multivariateNormality
  # Both populations are normal
  
  bartlett.test(data.feats[i1,],data.feats[i2,])
  # for sure different covariance 
}

fit <- manova(as.matrix(data.feats) ~ groups.name)
summary.manova(fit, test="Wilks")
# there isn't a clear separations between groups

qda.data <- qda(data.reduced, groups.name,prior=prior.c)
qda.data


# 2) Leave one out CV
# 2.1) with priors
# IMPORTANT: always use the priors without the cost information!!!
{
  # Leave One Out CV: specify priors accordingly in the for loop!
  errors_CV <- 0
  for(i in 1:dim(data)[1]){
    LdaCV.i <- qda(data.reduced[-i,], groups.name[-i], prior=prior)
    errors_CV <- errors_CV + as.numeric(predict(LdaCV.i,data.reduced[i,])$class != groups.name[i])
  }
  AERCV   <- sum(errors_CV)/length(groups.name)
  AERCV 
  
}


# c)
qda_prediction <- predict(qda.data)
conf <- table(list(predicted=qda_prediction$class, observed=data$result))
conf
# Should budget at least
((8+24)*4)*1000 #euro

# on the training set, classified 32 doped over 50 
# we will have 200 cyclists (*4), 
# But we're optimistic since we predict on training set


# d)
# Expanses with the 2 fold strategy:
1*50000 + 8*1000 - 24*1000
                    # This cost is equivalent to the estimated
                    # economic benefit of correctly identifying a true doping case
# Expanses with every cyclist testes with blood 
50*1000

