library(MVN)
library(car)
library(heplots)

## one-way ANOVA -------------------------------------------------------------------------------
data <- read.table("data.txt", header=TRUE)

# variable identifying groups
values <- data$
groups <- factor()

boxplot(data,groups)

n       <- length(groups)      # total number of obs.
ng      <- table(groups)       # number of obs. in each group
treat   <- levels(groups)      # levels of the treatment
g       <- length(treat)     # number of levels (i.e., of groups)

###Verify the assumptions ----------------------------------------------------------------
# 1) normality (univariate) in each group (6 tests)
# however, Koechlin said that ANOVA/MANOVA are robust wrt the assumption of gaussianity
# even if you do not have perfctly gaussian data, they still work fine
Ps <- c()
for(i in 1:g){
  Ps <- cbind(Ps, shapiro.test(values[groups==treat[1]])$p)
}
Ps
# for example if we have had a group with small p value we could have still proceeded
# with the analysis

# 2) same covariance structure (= same sigma^2)
Var <- c()
for(i in 1:g){
  Var <- cbind(Var,var(values[groups==treat[i]]) )
}
Var

# test of homogeneity of variances for normal samples (Bartlett's test)
# H0: sigma.1 = sigma.2 = sigma.3 = sigma.4 = sigma.5 = sigma.6 
# H1: there exist i,j s.t. sigma.i!=sigma.j
# WARNING: Test extremely sensitive to departures from normality (low robustness)
# Normality assumption mandatory !!!!
bartlett.test(values, groups) 

# to do ANOVA you shouldn't just rely on the results of the test, in general it has
# been demonstrated that is robust to its assumptions.
help(aov)
fit <- aov(values ~ groups)
summary(fit)
# if we reject, which group is different

###Bonferroni----------------------------------------------------------------------------
k <- g * (g - 1) / 2 # all the pairs of groups i consider
alpha = 0.05
Mediag  <- tapply(values, groups, mean) # group-wise means
SSres <- sum(residuals(fit) ^ 2)
S <- SSres / (n - g)              

# CI for all the differences
ICrange=NULL
for(i in 1:(g-1)) {
  for(j in (i+1):g) {
    print(paste(treat[i],"-",treat[j]))        
    print(as.numeric(c(Mediag[i]-Mediag[j] - qt(1-alpha/(2*k), n-g) * sqrt(S * ( 1/ng[i] + 1/ng[j])),
                       Mediag[i]-Mediag[j] + qt(1-alpha/(2*k), n-g) * sqrt(S * ( 1/ng[i] + 1/ng[j])))))
    ICrange=rbind(ICrange,as.numeric(c(Mediag[i]-Mediag[j] - qt(1-alpha/(2*k), n-g) * sqrt(S * (1/ng[i] + 1/ng[j])),
                                       Mediag[i]-Mediag[j] + qt(1-alpha/(2*k), n-g) * sqrt(S * (1/ng[i] + 1/ng[j])))))
  }}

# plot the CI
h <- 1
plot(c(1,g*(g-1)/2),range(ICrange), pch='',xlab='pairs treat', ylab='Conf. Int. tau weight')
for(i in 1:(g-1)) {
  for(j in (i+1):g) {
    ind <- (i-1)*g-i*(i-1)/2+(j-i)
    lines (c(h,h), c(ICrange[ind,1],ICrange[ind,2]), col='grey55'); 
    points(h, Mediag[i]-Mediag[j], pch=16, col='grey55'); 
    points(h, ICrange[ind,1], col=rainbow(6)[j], pch=16); 
    points(h, ICrange[ind,2], col=rainbow(6)[i], pch=16); 
    h <- h+1
  }}
abline(h=0)

## one-way MANOVA ---------------------------------------------------------------------
data <- read.table("data.txt", header=TRUE)

# variable identifying groups
values <- data[]
groups <- factor()

i1<-which(groups==levels(groups)[1])
i2<-which(groups==levels(groups)[2])
i3<-which(groups==levels(groups)[3])

n1 <- length(i1)
n2 <- length(i2)
n3 <- length(i3)
n  <- n1 + n2 + n3 +

g  <- length(levels(groups))
p  <- 4


plot(values)


### Verify the assumptions----------------------------------------------------------------
# 1)  normality (multivariate) in each group (3 tests)
mvn(values[i1,])$multivariateNormality
mvn(values[i2,])$multivariateNormality
mvn(values[i3,])$multivariateNormality


# 2) same covariance structure (= same covariance matrix Sigma)
cov1 <- values[which(groups == levels(group)[1]),]
cov2 <- values[which(groups == levels(group)[2]),]
cov3 <- values[which(groups == levels(group)[3]),]


# Box's M test for homogeneity of covariance matrices
# Should be done only if n_i > ~20 for all i, p < 5 and g < 5
# WARNING: Very sensitive to departure from normality and too restrictive for MANOVA
# especially if we have a high number of samples
summary(boxM(values,groups))



fit <- manova(as.matrix(values) ~ groups)
summary.manova(fit)

# Via ANOVA: for each of the p variables we perform an ANOVA test
#            to verify if the membership to a group has influence
#            on the mean of the variable (we explore separately the
#            4 axes directions in R^4)
summary.aov(fit)


### Bonferroni------------------------------------------------------------------------------
alpha <- 0.05
k <- p * g * (g - 1) / 2 # confidence intervals for every pair of groups and every variable
qT <- qt(1 - alpha / (2 * k), n - g)

W <- summary.manova(fit)$SS$Residuals
m  <- sapply(values, mean)         # estimates mu
m1 <- sapply(values[i1,], mean)    # estimates mu.1 = mu + tau.1
m2 <- sapply(values[i2,], mean)    # estimates mu.2 = mu + tau.2
m3 <- sapply(values[i3,], mean)    # estimates mu.3 = mu + tau.3

inf12 <- m1 - m2 - qT * sqrt(diag(W)/(n-g) * (1/n1+1/n2))
sup12 <- m1 - m2 + qT * sqrt(diag(W)/(n-g) * (1/n1+1/n2))
inf13 <- m1 - m3 - qT * sqrt(diag(W)/(n-g) * (1/n1+1/n3))
sup13 <- m1 - m3 + qT * sqrt(diag(W)/(n-g) * (1/n1+1/n3))
inf23 <- m2 - m3 - qT * sqrt(diag(W)/(n-g) * (1/n2+1/n3))
sup23 <- m2 - m3 + qT * sqrt(diag(W)/(n-g) * (1/n2+1/n3))

CI <- list(firstgroup    = cbind(inf12, sup12),
           secondgroup     = cbind(inf13, sup13),
           thirdgroup = cbind(inf23, sup23))
CI

# Two-way ANOVA ------------------------------------------------------------------
data <- read.table("data.txt", header=TRUE)
attach(data)

values <- data$
group1 <- factor()
group2 <- factor()
group1x2 <- factor(paste(group1,group2,sep="-"))

g <- length(levels(group1)) # number of factor1 levels
b <- length(levels(group2)) # number of factor2 levels
table1 <- table(group1)
table2 <- table(group2)
n1 <- sum(table(group1)) 
n2 <- sum(table(group2))
ng <- table(group1x2)

M           <- mean(values) # overall mean
Mgroup1      <- tapply(values, group1, mean) 
Mgroup2     <- tapply(values, group2, mean) 
Mgroup1x2 <- tapply(values, group1x2, mean) 

### Verify the assumptions----------------------------------------------------------
ps <- c()
for(i in 1:(g*b)){
  ps <- c(ps, shapiro.test(values[which(group1x2==levels(group1x2)[i])])$p)
}
ps
# homogeneity of variances
Var<- c()
for(i in 1:(g*b)){
  Var <- cbind(Var, values[which(group1x2==levels(group1x2)[i])])
}
Var

# test of homogeneity of variances for normal samples (Bartlett's test)
# H0: sigma.1 = sigma.2 = sigma.3 = sigma.4 = sigma.5 = sigma.6 
# H1: there exist i,j s.t. sigma.i!=sigma.j
# WARNING: Test extremely sensitive to departures from normality (low robustness)
# Normality assumption mandatory !!!!
bartlett.test(values, group1x2) 


###Boxcox transformations to make it gaussian-------------------------------------------------
lambdas <- c()
for(i in 1:(g*b)){
  lambdas <- c(lambdas,powerTransform(values[which(group1x2==levels(group1x2)[i])])$lambda)
}

for(i in 1:(g*b)){
  values[which(group1x2==levels(group1x2)[i])] <- bcPower(values[which(group1x2==levels(group1x2)[i])], lambdas[i])
}

# Two-ways ANOVA
### Model with interaction (complete model)------------------------------------------------ 
# X.ijk = mu + tau.i + beta.j + gamma.ij + eps.ijk; eps.ijk ~ N(0,sigma^2), 
#     i=1,2 (effect station), j=1,2 (effect gasoline)
fit.aov2.int <- aov(values ~ group1 + group2 + group1:group2)
summary.aov(fit.aov2.int)

# number of comparisons
k <- (g*b)*(g*b-1)/2 # Complete Model
alpha <-0.05
cfr.t <- qt(1-alpha/(2*k), fit.aov2.int$df.residual)

SSres <- sum(fit.aov2.int$residuals^2)/fit.aov2.int$df.residual

CIs <-c()
for(i in 1:((g*b)-1)){
  for(j in (i+1):(g*b)){
    inf <- Mgroup1x2[i]-Mgroup1x2[j] - cfr.t*sqrt(SSres*(1/ng[i] + 1/ng[j]))
    sup <- Mgroup1x2[i]-Mgroup1x2[j] + cfr.t*sqrt(SSres*(1/ng[i] + 1/ng[j]))
    ci <- cbind(inf,sup)
    rownames(ci) <- paste(levels(group1x2)[i],"-",levels(group1x2)[j])
    CIs <- rbind(CIs, ci)
  }
}  
CIs

### Additive model-------------------------------------------------------------------------------- 
# X.ijk = mu + tau.i + beta.j + eps.ijk; eps.ijk ~ N(0,sigma^2), 
#     i=1,2 (effect station), j=1,2 (effect gasoline)
fit.aov2.ad <- aov(values ~ group1 + group2)
summary.aov(fit.aov2.ad)

# ADDITTIVE CASE: confidence intervals for all the groups differences
Mgroup1x2

k <- (g*b)*(g*b-1)/2 - (g+b)*(g*b-g-b+1)/2 # Additive Model
alpha <-0.05
cfr.t <- qt(1-alpha/(2*k), fit.aov2.ad$df.residual)

SSres <- sum(fit.aov2.ad$residuals^2)/fit.aov2.ad$df.residual

CIs <-c()
for(i in 1:((g*b)-1)){
  for(j in (i+1):(g*b)){
    inf <- Mgroup1x2[i]-Mgroup1x2[j] - cfr.t*sqrt(SSres*(1/ng[i] + 1/ng[j]))
    sup <- Mgroup1x2[i]-Mgroup1x2[j] + cfr.t*sqrt(SSres*(1/ng[i] + 1/ng[j]))
    ci <- cbind(inf,sup)
    rownames(ci) <- paste(levels(group1x2)[i],"-",levels(group1x2)[j])
    CIs <- rbind(CIs, ci)
  }
}  
CIs


# One way ANOVA
fit.aov1 <- aov(values ~ group1)
summary.aov(fit.aov1)

#TWO-WAYS MANOVA--------------------------------------------------------------------

data <- read.table("data.txt", header=TRUE)
values <- data[,]
group1 <- factor(data$)
group2 <- factor(data$)
group1x2 <- factor(paste(group1,group2,sep="-"))

g <- length(levels(group1)) # number of factor1 levels
b <- length(levels(group2)) # number of factor2 levels
table1 <- table(group1)
table2 <- table(group2)
n1 <- sum(table(group1)) 
n2 <- sum(table(group2))
ng <- table(group1x2)

M           <- mean(values) # overall mean
Mgroup1      <- tapply(values, group1, mean) 
Mgroup2     <- tapply(values, group2, mean) 
Mgroup1x2 <- tapply(values, group1x2, mean) 

### Verify the assumptions-----------------------------------------------------------
# 1) normality (multivariate) in each group 
Ps <-NULL
for (i in 1:(g*b)){
  Ps <- c(Ps,values[which(group1x2==levels(group1x2)[i]),])
}
Ps
# here you can also plot pairs() of the different groups and qualitatevily assess if the points
# are distributed in strange ways

# ) homogeneity of the covariance (qualitatively)
Covs <-NULL
for(i in 1:(g*b)){
  Covs <- c(Covs, cov(values[which(group1x2==levels(group1x2)[i]),]))
}
Covs

###Model with interaction (complete model) -----------------------------------------------------------------
# Two-ways MANOVA
# Model with interaction (complete model): 
# X.ijk = mu + tau.i + beta.j + gamma.ij + eps.ijk; eps.ijk ~ N_p(0,Sigma), [p=3]
#     i=1,2 (effect Extrusion), j=1,2 (effect Additive),
#     X.ijs, mu, tau.i, beta.j, gamma.ij in R^3
fit <- manova(as.matrix(plastic3) ~ Ex + Ad + Ex:Ad)
summary.manova(fit)

# number of comparisons
g <- length(levels(group1))
b <- length(levels(group2))
k <- (g*b)*(g*b-1)/2 # Complete Model
alpha <-0.05
cfr.t <- qt(1-alpha/(2*k), fit$df.residual)

W <- summary.manova(fit2)$SS$Residuals

CIs <-c()
for(i in 1:((g*b)-1)){
  for(j in (i+1):(g*b)){
    inf <- Mgroup1x2[i]-Mgroup1x2[j] - cfr.t*sqrt(W[i,j]*(1/ng[i] + 1/ng[j]))
    sup <- Mgroup1x2[i]-Mgroup1x2[j] + cfr.t*sqrt(W[i,j]*(1/ng[i] + 1/ng[j]))
    ci <- cbind(inf,sup)
    rownames(ci) <- paste(levels(group1x2)[i],"-",levels(group1x2)[j])
    CIs <- rbind(CIs, ci)
  }
}  
CIs

### Model without interaction (additive model): --------------------------------------------------------------
### X.ijk = mu + tau.i + beta.j + eps.ijk; eps.ijk~N_p(0,Sigma), [p=3]
###     i=1,2 (effect Extrusion), j=1,2 (effect additive),
###     X.ijs, mu, tau.i, beta.j, in R^3
fit2<- manova(as.matrix(plastic3) ~ Ex + Ad)
summary.manova(fit2)

# Bonferroni
alpha <- 0.05
g <- length(levels(group1))
b <- length(levels(group2))
p <- dim(values)[2]
n <- dim(values)[1]
N <- n*g*b # 20

W <- summary.manova(fit2)$SS$Residuals

# how many comparisons?
k <- p*g*(g-1)/2 + p*b*(b-1)/2
# because we have: g levels on the first treatment on p components
#                  b levels on the second treatment on p components
k

qT <- qt(1 - alpha/(2*k), g*b*n-g-b+1)
# the degrees of freedom of the residuals on the additive model are
# g*b*n-g-b+1

# mean group1 level 1 and 2
m1.1  <- sapply(values[group1==levels(group1)[1],],mean)
m1.2  <- sapply(values[group1==levels(group1)[2],],mean)
infEx <- m1.1-m1.2 - qT * sqrt(diag(W)/(g*b*n-g-b+1) * (1/n1+1/n2))
supEx <- m1.1-m1.2 + qT * sqrt(diag(W)/(g*b*n-g-b+1) * (1/n1+1/n2))

# mean group1 level 1 and 2
m2.1  <- sapply(values[group2==levels(group2)[1],],mean)
m2.2  <- sapply(values[group2==levels(group2)[2],],mean)
infAd <- m2.1-m2.2 - qT * sqrt(diag(W)/(g*b*n-g-b+1) * (1/n1+1/n2))
supAd <- m2.1-m2.2 + qT * sqrt(diag(W)/(g*b*n-g-b+1) * (1/n1+1/n2))

IC2   <- list(group1.1_group1.2=cbind(infEx, supEx), group2.1_group2.2=cbind(infAd, supAd))
IC2
# to see which are the levels and in what order of the two groups
levels(group1)
levels(group2)
