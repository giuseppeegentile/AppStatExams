library(MASS)
library(MVN)
library(class)
library(e1071)
library(heplots)

# LDA/QDA MULTIVARIATE ---------------------------------------------------------------

data <- read.table("data.txt", header=TRUE)
head(data)

features <- data[,1:2]
groups <- data[,3]
groups <- factor(groups)
groups
g <- length(levels(groups))
group1 <- features[which(groups==levels(groups)[1]),]
group2 <- features[which(groups==levels(groups)[2]),]
n1 <-length(group1)
n2 <-length(group2)
n <- n1+n2
m <- colMeans(features)
m1 <- colMeans(group1)
m2 <- colMeans(group2)
S1 <- cov(group1)
S2 <- cov(group2)
Sp <- ((n1-1)*S1 + (n2-1)*S2)/(n-g)

# binary groups plot
plot(features, col=ifelse(groups==levels(groups)[1],"red","blue"), pch=19)


### PRIORS MANIPULATION -------------------------------------------------------------------
# REMEMBER: levels is ordered according to alphabetical order
levels(groups)
true <- levels(groups)[1]
false <- levels(groups)[2]
c.tf <-1 # predicting as true a false costs c.tf
c.ft <-1 # predicting as false a true costs c.ft

# priors definition
p.t <-0.5
p.f <-0.5
prior = c(pt, pf)
prior

prior.c <- c(pt*c.ft/(c.tf*pf+c.ft*pt), pf*c.tf/(c.tf*pf+c.ft*pt))
prior.c


### VERIFY ASSUMPTIONS (QDA/LDA) ----------------------------------------------------
mvn(group1)$multivariateNormality
mvn(group2)$multivariateNormality

# Verify covariance structure
cov1 <- cov(group1)
cov2 <- cov(group2)
cov1/cov2
# they are similar enough
# also check it visually!

summary(boxM(features,groups))


# we can assume gaussianity and same covariance -> LDA
# Good practice before performing LDA
# One-way MANOVA
# to see if there is a real separation between groups.
fit.man <- manova(as.matrix(features) ~ groups)
summary.manova(fit.man, test="Wilks")

fit <- lda(features,groups)
fit

### PLOT (binary case)-----------------------------------------------------------

# creates some fake point in the interval defined by the training data
x  <- seq(min(features[,1]), max(features[,1]), length=200)
y  <- seq(min(features[,2]), max(features[,2]), length=200)
# expand them to create a dataset  joining every possible combination of x and y
xy <- expand.grid(dimnames(as.string(features)[[2]][1])=x, dimnames(features)[[2]][2]=y)

# Plot the partition induced by LDA
plot.partition.lda <- function() {
  # plots the different point with different colours
  plot(features, main='Fossils', xlab='lon', ylab='lat', pch=20)
  points(group1,col='red', pch=20)
  points(group2, col='green', pch=20)
  legend("topright", legend=levels(groups), fill=c('red','green'), cex=.7)
  # add the means
  points(fit$means, pch=4,col=c('red','green') , lwd=2, cex=1.5)
  
  # predicts all the fictious point and calculates the posterior
  # prob of being in class i given that you are x
  z  <- predict(fit, xy)$post  # these are P_i*f_i(x,y)  
  # finds the 3 boundaries
  z1 <- z[,1] - z[,2] # P_1*f_1(x,y)-max{P_j*f_j(x,y)}  
  # Plot the contour line of level (levels=0) of z1, z2, z3: 
  # P_i*f_i(x,y)-max{P_j*f_j(x,y)}=0 i.e., boundary between R.i and R.j 
  # where j realizes the max.
  contour(x, y, matrix(z1, 200), levels=0, drawlabels=F, add=T)  
}
plot.partition.lda()
### PLOT (3 classes)  -----------------------------------------------------------
i1<-which(groups==levels(groups)[1])
i2<-which(groups==levels(groups)[2])
i3<-which(groups==levels(groups)[3])

plot.partition.lda <- function() {
  # plots the different point with different colours
  plot(features, pch=20)
  points(features[i1,], col='red', pch=20)
  points(features[i2,], col='green', pch=20)
  points(features[i3,], col='blue', pch=20)
  legend("topright", legend=levels(species.name), fill=c('red','green','blue'), cex=.7)
  # add the means
  points(lda$means, pch=4,col=c('red','green','blue') , lwd=2, cex=1.5)
  
  # predicts all the fictious point and calculates the posterior
  # prob of being in class i given that you are x
  z  <- predict(lda, xy)$post  # these are P_i*f_i(x,y)  
  # finds the 3 boundaries
  z1 <- z[,1] - pmax(z[,2], z[,3])  # P_1*f_1(x,y)-max{P_j*f_j(x,y)}  
  z2 <- z[,2] - pmax(z[,1], z[,3])  # P_2*f_2(x,y)-max{P_j*f_j(x,y)}    
  z3 <- z[,3] - pmax(z[,1], z[,2])  # P_3*f_3(x,y)-max{P_j*f_j(x,y)}
  
  # Plot the contour line of level (levels=0) of z1, z2, z3: 
  # P_i*f_i(x,y)-max{P_j*f_j(x,y)}=0 i.e., boundary between R.i and R.j 
  # where j realizes the max.
  contour(x, y, matrix(z1, 200), levels=0, drawlabels=F, add=T)  
  contour(x, y, matrix(z2, 200), levels=0, drawlabels=F, add=T)
  contour(x, y, matrix(z3, 200), levels=0, drawlabels=F, add=T)
}
plot.partition.lda()

###ERROR ESTIMATION (QDA/LDA) --------------------------------------------------------------------

# APER with SAMPLE PRIORS
fit.predict <- predict(fit, features)

table(class.true=groups, class.assigned=fit.predict$class)
errors <- (fit.predict$class != groups)

APER   <- sum(errors)/length(groups)
APER

# APER with GIVEN PRIORS (NON ADJUSTED ONES)
fit.predict <- predict(fit, features)
prior <- c(1/3, 1/3, 1/3) # given priors
G <- 3 # number of groups
misc <- table(class.true=groups, class.assigned=fit.predict$class)
APER <- 0
for(g in 1:G)
  APER <- APER + sum(misc[g,-g])/sum(misc[g,]) * prior[g]
APER

# LDA CV with GIVEN PRIORS (NON ADJUSTED ONES)
errors_CV <- 0
for(i in 1:150){
  fit.predict <- lda(iris2[-i,], species.name[-i], prior=c(50, 50, 50) / 150)
  errors_CV <- errors_CV + as.numeric(predict(fit.predict,iris2[i,])$class != species.name[i])
}
errors_CV

AERCV   <- sum(errors_CV)/length(species.name)
AERCV

# LDA CV with R with SAMPLE PRIORS
fit.predict <- lda(features, groups, CV=TRUE,prior=prior)
table(class.true=groups, class.assigned=fit.predict$class) # apply the given prior connection

errors <- (fit.predict$class != groups)

AERCV   <- sum(errors)/length(groups)
AERCV

# LDA CV with R with GIVEN PRIORS (NON ADJUSTED ONES)
fit.predict <- lda(features, groups, CV=TRUE,prior=prior)
prior # given priors
G <- 3 # number of groups
misc <- table(class.true=groups, class.assigned=fit.predict$class)
APER <- 0
for(g in 1:G)
  APER <- APER + sum(misc[g,-g])/sum(misc[g,]) * prior[g]
APER

###ECONOMIC LOSS AND BUDGET ----------------------------------------------------------

# Expected economic loss: # |E(EL) = c.vf * p(EL = c.vf) + c.fv * p(EL = c.fv) + 0 * p(EL = 0)
table <- table(class.true=groups, class.assigned=fit.predict$class)
TP <- table[1,1]
FP <- table[2,1]
TN <- table[2,2]
FN <- table[1,2]
allrealfalse <- sum(table[2,])# FN + TP
allrealtrue <- sum(table[1,]) # FP + TN

(FN/FN+TP) * pt * c.ft + (FP/FP+TN)* pf * c.tf

# P[di dire false] = P[di dire false | true]P[true] + P[di dire false | false]P[false]
FN/(FN+TP)*pt + TN/(FP+TN)*pf

# P[di dire true] = P[di dire true | true]P[true] + P[di dire true | false]P[true]
TP/(FN+TP)*pt + FP/(FP+TN)*pf



# K-NEAREST NEIGHBOURS ------------------------------------------------------------
set.seed()
knnCVs <- c()
fits <- c()
# find best k in a range
for(k in 5:20){
  fit.knn <- knn.cv(train=features, cl=groups, k=k)
  table(class.true=groups, class.assigned=fit.knn)
  errors <- (fit.knn != groups)
  aercv <- sum(errors)/length(groups)
  knnCVs <- rbind(knnCVs,c(k,aercv))
  fits <- cbind(fits, fit.knn)
}
knnCVs
mink <- knnCVs[which(knnCVs[,2]==min(knnCVs[,2])),1]
bestError <- knnCVs[which(knnCVs[,2]==min(knnCVs[,2])),2]
bestmodelindex <- which(knnCVs[,2]==min(knnCVs[,2]))

{
  plot(features, main='Dataset', xlab='X1', ylab='X2', pch=20)
  points(group1, col="red", pch=20)
  points(group2, col=4, pch=20)
  
  x  <- seq(min(data[,1]), max(data[,1]), length=200)
  y  <- seq(min(data[,2]), max(data[,2]), length=200)
  xy <- expand.grid(feature1=x, feature2=y)
  # a knn just to plot the contour
  dataknn <- knn(train=features,test=xy, cl=groups, k=mink[1])
  z  <- as.numeric(dataknn)
  
  contour(x, y, matrix(z, 200), levels=c(1.5, 2.5), drawlabels=F, add=T)
}

newobs <- data.frame()
points(newobs,col="black")
# i would classify it as group1 -> nerinea




#FISHER DISCRIMINANT ANALYSIS ---------------------------------------------------
# covariance between groups (estimate)
B <- 1/g*(cbind(m1 - m) %*% rbind(m1 - m) +
            cbind(m2 - m) %*% rbind(m2 - m) +
            cbind(m3 - m) %*% rbind(m3 - m))
B

# covariance within groups (estimate)
Sp

# how many coordinates?
g <- 3 #groups
p <- 2 #features
s <- min(g-1,p)
s

fisher <- lda(features, groups)
fisher
a1 <- #first column of "Coefficients of linear discriminants"
a2 <- #second column of "Coefficients of linear discriminants"
# as many "a" as p
 
# "proportion of trace" in the lda summary
# the amount of variability between groups that i'm able to retrieve taking the 1,2 etc.
# canonical direction

# project on fisher coordinates
cc1 <- as.matrix(features)%*%a1
cc2 <- as.matrix(features)%*%a2

coord.cc <- cbind(cc1.iris,cc2.iris)

#projection of the means
cc.m1 <- c(m1%*%a1, m1%*%a2)
cc.m2 <- c(m2%*%a1, m2%*%a2)
cc.m3 <- c(m3%*%a1, m3%*%a2)

# Fisher classification
f.class=rep(0, n)
for(i in 1:n) # for each datum
{
  # Compute the Euclidean distance of the i-th datum from mean within the groups
  dist.m=c(d1=sqrt(sum((coord.cc[i,]-cc.m1)^2)),
           d2=sqrt(sum((coord.cc[i,]-cc.m2)^2)),
           d3=sqrt(sum((coord.cc[i,]-cc.m3)^2)))
  # Assign the datum to the group whose mean is the nearest
  f.class[i]=which.min(dist.m)
}
f.class
table(class.true=species.name, class.assigned=f.class)

errors <- n - sum(diag(table(class.true=species.name, class.assigned=f.class)))
# if you have same prior this is equal to LDA
APERf   <- errors/length(groups)
APERf

### How do I classify a new observation?
x.new <- c(5.85, 2.90)
# compute the canonical coordinates
cc.new <- c(x.new%*%a1, x.new%*%a2)
# compute the distance from the means
dist.m <- c(d1=sqrt(sum((cc.new-cc.m1)^2)),
            d2=sqrt(sum((cc.new-cc.m2)^2)),
            d3=sqrt(sum((cc.new-cc.m3)^2)))
# assign to the nearest mean
which.min(dist.m)

#SVM (Only for two classes)------------------------------------------------------
x <- features
y <- groups 

dat <- data.frame(x=x, y=y)

## Linear
svmfit <- svm(y~., data=dat, kernel="linear",cost=1,scale=FALSE)
summary(svmfit)

par(mfrow=c(1,1))
plot(svmfit, dat, col =c('salmon', 'light blue'), pch=19, asp=1)

## Non-linear case
# there is also the gamma parameter
svmfit <- svm(y~., data=dat, kernel ='radial', gamma =1, cost =1)
summary(svmfit)

# Set parameters via CV:
set.seed (1)
tune.out <- tune(svm , y~., data=dat, kernel ='radial',
                 ranges =list(cost=c(0.1 ,1 ,10 ,100 ,1000),
                              gamma=c(0.5,1,2,3,4) ))
summary(tune.out)


# support vectors are indicated with crosses
# they are:
svmfit$index

# Tuning the cost parameter
tune.out <- tune(svm, y~., data=dat, kernel = 'linear',
                  ranges = list(cost=c(0.001 , 0.01, 0.1, 1,10,100) ))
# Extract the best model from the result of tune
bestmod <- tune.out$best.model
summary(bestmod)


newobs <-cbind()
newobs <- data.frame(x=newobs)
colnames(newobs) <- colnames(dat) #colnames must be the same

prediction <- predict(svmfit, newobs)

