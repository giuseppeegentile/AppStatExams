library(MVN)
library(car)
library(heplots)

red <- read.table("red.txt", header=TRUE)
white <- read.table("white.txt", header=TRUE)

# MANOVA with g=2
nr <- dim(red)[1]
nw <- dim(white)[1]
p <- 3

meanR <- colMeans(red)
meanW <- colMeans(white)

SR <- cov(red)
SW <- cov(white)

Sp <- ((nr-1)*SR + (nw-1)*SW)/(nr+nw-2)
Spinv <- solve(Sp)

diffmean <- meanR - meanW

cfr.fisher <- (((nr+nw-2)*p)/(nr+nw-1-p) )* qf(0.95,p,nr+nw-1-p)

inverse <- (1/nr + 1/nw)^-1
# math expression
statistic <- inverse * t(diffmean) %*% Spinv %*% (diffmean)

statistic > cfr.fisher
# TRUE there's evidence to reject the null hypothesis
# there is difference

statistic
center <- diffmean

#math expression above
pvalue <- 1- pf((statistic*(((nr+nw-2)*p)/(nr+nw-1-p) )^-1),p,nr+nw+1-p)
pvalue
# assumption of the previous test are gaussianity for the two populations and same covariance

mvn(red)$multivariateNormality
mvn(white)$multivariateNormality

# we have evidence of gaussianity

# same covariance
SW/SR
# qualitatevily the two covariances are very similar

k<-3
alpha <- 0.05
cfr.t <- qt(1-alpha/(2*k),nr+nw-1)


BCI <- cbind(diffmean - cfr.t*sqrt(diag(Sp) * (1/nr + 1/nw)),
         diffmean + cfr.t*sqrt(diag(Sp) * (1/nr + 1/nw)))
BCI

# difference in acidity


