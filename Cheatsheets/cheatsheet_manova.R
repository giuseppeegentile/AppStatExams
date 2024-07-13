###-----------------------------------###
### MULTIVARIATE ANALYSIS OF VARIANCE ###-------------------------------------------
###-----------------------------------###

#### One-way ANOVA ----

### Try It Out! ###
rm(list = ls())
graphics.off()
par(mfrow=c(1,1))
cat("\014")
data <- read.table('datasets/data_anova.txt', header = T)
data <- data[, 1:2]
### Try It Out! ###

target <- data$target
treatment <- factor(data$treatment)

n <- length(treatment)      
ng <- table(treatment)       
treat <- levels(treatment)      
g <- length(levels(treatment))


##### Data Visualization -----

boxplot(target ~ treatment, xlab = 'Treat Level', ylab = 'Target Variable', col = 'grey85')

par(mfrow=c(1,2))
barplot(rep(mean(target), g), names.arg = treat, ylim = c(0, max(target)),
        las = 2, col = 'grey85', main='Model under H0')
barplot(tapply(target, treatment, mean), names.arg = treat, ylim = c(0, max(target)),
        las = 2, col = rainbow(g), main='Model under a special case of H1')
par(mfrow=c(1,1))


##### Verify Assumptions -----

# 1) normality (univariate) in each group

Ps <- NULL
for(i in 1:g)
{
  Ps <- rbind(Ps, shapiro.test(target[treatment == treat[i]])$p)
}
dimnames(Ps)[[1]] <- treat
dimnames(Ps)[[2]] <- c("p-value")
Ps

# 2) same covariance structure (= same sigma^2)

Var <- NULL
for(i in 1:g)
{
  Var <- rbind(Var, var(target[treatment == treat[i]]))
}
dimnames(Var)[[1]] <- treat
dimnames(Var)[[2]] <- c("sigma^2")
Var

Pvt <- NULL
rownames <- NULL
for(i in 1:(g-1))
{
  for(j in (i+1):g)
  {
    Pvt <- rbind(Pvt, var.test(target[treatment == treat[i]], target[treatment == treat[j]])$p.value)
    rownames <- cbind(rownames, paste(treat[i], "v", treat[j]))
  }
}
dimnames(Pvt)[[1]] <- rownames
dimnames(Pvt)[[2]] <- c("p-value")
Pvt

bartlett.test(target, treatment)


##### Fit -----

fit <- aov(target ~ treatment)
summary(fit)
### How to read the summary:
#              Df   Sum Sq      Mean Sq      F value     Pr(>F)    
# treatment  (g-1) SStreat  SStreat/(g-1)  Fstatistic  p-value [H0: tau.i=0 for every i]
# Residuals  (n-g) SSres     SSres/(n-g)


##### Confidence Intervals -----

# Which treatment level is responsible for this?

dofs <- fit$df.residual # n-g

k <- g * (g - 1) / 2
alpha <- 0.05

qT <- qt(1 - alpha/(2*k), dofs)

Mediag <- tapply(target, treatment, mean)
SSres <- sum(residuals(fit) ^ 2)
S <- SSres / dofs

CI.range <- NULL
for(i in 1:(g-1)) 
{
  for(j in (i+1):g) 
  {
    inf <- Mediag[i] - Mediag[j] - qT * sqrt(S * (1/ng[i] + 1/ng[j]))
    sup <- Mediag[i] - Mediag[j] + qT * sqrt(S * (1/ng[i] + 1/ng[j]))
    interval_i <- cbind(inf, sup)
    rownames(interval_i) <- paste(treat[i], "-", treat[j])
    CI.range <- rbind(CI.range, interval_i)
  }
}
CI.range

h <- 1
plot(c(1, g*(g-1)/2), range(CI.range), pch = '', xlab = 'Pairs Treat', ylab = 'Conf. Int. tau Target')
for(i in 1:(g-1)) 
{
  for(j in (i+1):g) 
  {
    lines (c(h, h), c(CI.range[h, 1], CI.range[h, 2]), col = 'grey55'); 
    points(h, Mediag[i] - Mediag[j], pch = 16, col = 'grey55'); 
    points(h, CI.range[h, 1], col = rainbow(g)[j], pch = 16); 
    points(h, CI.range[h, 2], col = rainbow(g)[i], pch = 16); 
    h <- h + 1
  }
}
abline(h = 0)

# One-at-a-time

Auni <- matrix(0, g, g)
for(i in 1:g) {
  for(j in i:g) {
    Auni[i, j] <- Mediag[i] - Mediag[j] + qt(1 - alpha/2, n-g) * sqrt(S * (1/ng[i] + 1/ng[j]))}
  for(j in 1:i) {
    Auni[i, j] <- Mediag[j] - Mediag[i] - qt(1 - alpha/2, n-g) * sqrt(S * (1/ng[i] + 1/ng[j]))}
  Auni[i, i] <- 0
}

h <- 1
plot(c(1, g*(g-1)/2), range(Auni), pch = '', xlab = 'pairs treat', ylab = 'CI delta weight', main = 'Univariate Conf. Int.', col = 'grey55')
for(i in 1:(g-1)) 
{
  for(j in (i+1):g) 
  {
    lines (c(h, h), c(Auni[i, j], Auni[j, i])); 
    points(h, Mediag[i] - Mediag[j], pch = 16, col = 'grey55'); 
    points(h, Auni[i, j], col = rainbow(g)[i], pch = 16); 
    points(h, Auni[j, i], col = rainbow(g)[j], pch = 16); 
    h <- h + 1
  }
}
abline(h = 0)

# p-values

P <- matrix(0, g, g)
for(i in 1:g) {
  for(j in i:g) {
    P[i, j] <- (1 - pt(abs((Mediag[i] - Mediag[j]) / sqrt(S * (1/ng[i] + 1/ng[j]))), n-g))*2}
  for(j in 1:i) {
    P[i, j] <- (1 - pt(abs((Mediag[i] - Mediag[j]) / sqrt(S * (1/ng[i] + 1/ng[j]))), n-g))*2}
  P[i, i] <- 0
}

p <- NULL
for(i in 1:(g-2))
{
  p <- c(p, P[i, (i+1):g])
}
p <- c(p, P[g-1, g])
p

plot(1:k, p, ylim = c(0, 1), type = 'b', pch = 16, col = 'grey55', xlab = 'Pairs Treat', main='p-values')
abline(h = alpha, lty = 2)

## Bonferroni correction
p.bonf <- p.adjust(p, 'bonf') 
lines(1:k, p.bonf, col = 'blue', pch = 16, type = 'b')

## Correction according to the false discovery rate (Benjamini-Hockberg)
p.fdr <- p.adjust(p, 'fdr')
lines(1:k, p.fdr, col = 'red', pch = 16, type = 'b')

legend('topleft', c('Not corr.', 'Bonf.', 'BH'), col = c('grey55', 'blue', 'red'), pch = 16)

which(p.bonf < alpha)
which(p.fdr < alpha)


#### One-way MANOVA ----

### Try It Out! ###
rm(list = ls())
graphics.off()
par(mfrow=c(1,1))
cat("\014")
data <- read.table('datasets/data_manova.txt', header = T)
data <- data[, 1:3]
### Try It Out! ###

target <- data[, 1:2] # variables
treatment <- factor(data$treatment)

n <- length(treatment)
ng <- table(treatment)
treat <- levels(treatment)
g <- length(levels(treatment))
p <- dim(target)[2]


##### Data Visualization -----

par(mfrow=c(1,g))
for(i in 1:g)
{
  boxplot(target[treatment == treat[i], ], main = treat[i], ylim = range(target), col = rainbow(p))
}

par(mfrow=c(1,p))
for(i in 1:p)
{
  boxplot(target[, i] ~ treatment, main = colnames(target)[i], ylim = range(target), col = rainbow(g))
}

par(mfrow=c(1,1))


##### Verify Assumptions -----

# 1) normality (multivariate) in each group
Ps <- NULL
for(i in 1:g) {
  mvn.test <- mvn(data = target[treatment == treat[i], ])
  Ps <- rbind(Ps, mvn.test$multivariateNormality$`p value`)
}
dimnames(Ps)[[1]] <- treat
dimnames(Ps)[[2]] <- c("p-value")
Ps

# 2) same covariance structure (= same covariance matrix Sigma)

covs <- list()
covs[["S"]] <- cov(target)
for(i in 1:g)
{
  cov <- cov(target[treatment == treat[i], ])
  covs[[paste("S", i, sep = '')]] <- cov
}
covs

# Box's M test for homogeneity of covariance matrices
# Should be done only if n_i > ~20 for all i, p < 5 and g < 5
# WARNING: Very sensitive to departure from normality and too restrictive for MANOVA
# especially if we have a high number of samples

summary(boxM(target, treatment)) #!library(heplots)
boxM(target, treatment)$p.value

# Heatmap on the covariance matrices

par(mfrow=c(1,g))
quant <- NULL
for(i in 2:(g+1))
{
  quant <- rbind(quant, covs[[i]])
}
for(i in 1:g)
{
  Si <- paste("S", i, sep = '')
  image(covs[[Si]], col = heat.colors(100), main = paste('Cov.', Si), asp = 1, axes = FALSE, breaks = quantile(quant, (0:100)/100, na.rm = TRUE))
}
par(mfrow=c(1,1))

# Note: We can verify the assumptions a posteriori on the residuals of the estimated model 


##### Fit -----

fit <- manova(as.matrix(target) ~ treatment)
summary.manova(fit)
# Reject the test, i.e., we have statistical evidence to state that
# the treatment has an effect on the mean features 
# of the flowers.
# Who's the responsible for this?

summary.aov(fit)
# Note: this analysis does NOT say: 
#       a) which group differ
#       b) with respect to which variables the groups in (a) differ


##### Confidence Intervals -----

alpha <- 0.05
k <- p * g * (g - 1) / 2
qT <- qt(1 - alpha/(2 * k), n - g)

W <- summary.manova(fit)$SS$Residuals
m <- sapply(target, mean)

means <- NULL
for(i in 1:g) {
  means <- rbind(means, sapply(target[treatment == treat[i], ], mean)) 
}
dimnames(means)[[1]] <- treat
means

inf <- list()
sup <- list()
CI <- list()
pairs <- combn(g, 2)

for (i in 1:ncol(pairs)) {
  idx1 <- pairs[1, i]
  idx2 <- pairs[2, i]
  
  m1 <- means[idx1, ]
  m2 <- means[idx2, ]
  n1 <- ng[idx1]
  n2 <- ng[idx2]
  
  inf_val <- m1 - m2 - qT * sqrt(diag(W) / (n - g) * (1 / n1 + 1 / n2))
  sup_val <- m1 - m2 + qT * sqrt(diag(W) / (n - g) * (1 / n1 + 1 / n2))
  
  inf[[paste("inf", idx1, idx2, sep = '')]] <- inf_val
  sup[[paste("sup", idx1, idx2, sep = '')]] <- sup_val
  
  CI[[paste(treat[idx1], "_", treat[idx2], sep = '')]] <- cbind(inf_val, sup_val)
}

CI

par(mfrow=c(2,p))
for(i in 1:p)
{
  boxplot(target[, i] ~ treatment, main = colnames(target)[i], ylim = range(target), col = rainbow(g))
}

for(k in 1:p)
{
  plot(c(1, g*(g - 1)/2), pch = '', 
       xlim = c(1, g*(g - 1)/2), ylim = c(range(CI)[1] - 3, range(CI)[2] + 3),
       xlab = 'Pairs Treat', ylab = paste('CI tau', k), 
       main = paste('CI tau', colnames(target)[k]))
  for(i in 1:(g*(g - 1)/2))
  {
    j <- pairs[1, i]
    l <- pairs[2, i]
    lines(c(i, i), c(CI[[i]][k, 1], CI[[i]][k, 2]))
    points(i, means[j, k] - means[l, k], pch = 16); 
    points(i, CI[[i]][k, 1], col = rainbow(g)[l], pch = 16); 
    points(i, CI[[i]][k, 2], col = rainbow(g)[j], pch = 16); 
  }
  abline(h = 0)
}

par(mfrow=c(1,1))


#### Two-ways ANOVA ----

### Try It Out! ###
rm(list = ls())
graphics.off()
par(mfrow=c(1,1))
cat("\014")
data <- read.table('datasets/data_anova.txt', header = T)
### Try It Out! ###

target <- data$target

treatment1 <- factor(data$treatment1, labels = c('LvA', 'LvB', 'LvC')) 
treatment2 <- factor(data$treatment2, labels = c('LvX', 'LvY', 'LvZ')) 

treatments <- factor(paste(treatment1, treatment2, sep=''))
treatments

g <- length(levels(treatment1)) # number of factor1 levels
b <- length(levels(treatment2)) # number of factor2 levels
n <- length(target)/(g*b) # group sizes
treats <- levels(treatments)

M <- mean(target) # overall mean
M.treat1 <- tapply(target, treatment1, mean) # mean per treat 1
M.treat2 <- tapply(target, treatment2, mean) # mean per treat 2
M.treats <- tapply(target, treatments, mean) # mean per treat 1 x treat 2

M.add <- NULL
for(i in 1:g)
{
  for(j in 1:b)
  {
    M.add <- cbind(M.add, M.treat1[i] + M.treat2[j])
  }
}
dimnames(M.add)[[1]] <- c("M.add")
dimnames(M.add)[[2]] <- treats
M.add


##### Visualize Data -----

par(mfrow=c(2,3), las = 2)
barplot(rep(M,(g*b)), names.arg = treats, ylim = c(0, range(target)[2] + 3), main = 'No Treatments')
barplot(rep(M.treat1, each = b), names.arg = treats, ylim = c(0, range(target)[2] + 3), 
        col = rep(rainbow(g), each = b), main = 'Only Treat. 1')
barplot(rep(M.treat2, times = g), names.arg = treats, ylim = c(0,range(target)[2] + 3),
        col = rep(rainbow(b), times = g), main = 'Only Treat. 2')
barplot(M.add - M, names.arg = treats, ylim = c(0, range(target)[2] + 3), 
        col = rep(rainbow(b), times = g), density = rep(10, (g*b)), angle = 135, 
        main = 'Additive Model T1+T2')
barplot(M.add - M, names.arg = treats, ylim = c(0,range(target)[2] + 3), 
        col = rep(rainbow(g), each = b), density = rep(10, (g*b)), add = T)
barplot(M.treats, names.arg = treats, ylim = c(0,range(target)[2] + 3), 
        col = rainbow(g*b), main = 'Model with Interact. T1+T2')
plot(treatments, target, col = rainbow(g*b), ylim =c (0, range(target)[2] + 3), xlab='')
par(mfrow=c(1,1))


##### Verify Assumptions -----

# 1) normality (univariate) in each group

Ps <- NULL
for(i in 1:(g*b))
{
  Ps <- rbind(Ps, shapiro.test(target[treatments == treats[i]])$p)
}
dimnames(Ps)[[1]] <- treats
dimnames(Ps)[[2]] <- c("p-value")
Ps

# 2) same covariance structure (= same sigma^2)

Var <- NULL
for(i in 1:(g*b))
{
  Var <- rbind(Var, var(target[treatments == treats[i]]))
}
dimnames(Var)[[1]] <- treats
dimnames(Var)[[2]] <- c("sigma^2")
Var

Pvt <- NULL
rownames <- NULL
for(i in 1:((g*b)-1))
{
  for(j in (i+1):(g*b))
  {
    Pvt <- rbind(Pvt, var.test(target[treatments == treats[i]], target[treatments == treats[j]])$p.value)
    rownames <- cbind(rownames, paste(treats[i], "v", treats[j]))
  }
}
dimnames(Pvt)[[1]] <- rownames
dimnames(Pvt)[[2]] <- c("p-value")
Pvt

bartlett.test(target, treatments)


##### Fit -----

fit.aov2.int <- aov(target ~ treatment1 + treatment2 + treatment1:treatment2)
summary.aov(fit.aov2.int)

fit.aov2.ad <- aov(target ~ treatment1 + treatment2)
summary.aov(fit.aov2.ad)


###### Statistical Tests ------

M <- mean(target) # overall mean
M.treat1 <- tapply(target, treatment1, mean) # mean per treat 1
M.treat2 <- tapply(target, treatment2, mean) # mean per treat 2
M.treats <- tapply(target, treatments, mean) # mean per treat 1 x treat 2


# Variance Decomposition

SStot <- sum((target - M)^2)
SSres <- sum(residuals(fit.aov2.int)^2) # Complete Model
SSres <- sum(residuals(fit.aov2.ad)^2) # Additive Model


## Taking SS directly from the Summary

SS.treat1 <- summary.aov(fit.aov2.int)[[1]][1, "Sum Sq"]
SS.treat2 <- summary.aov(fit.aov2.int)[[1]]["treatment2", "Sum Sq"]  
SS.treats <- summary.aov(fit.aov2.int)[[1]]["treatment1:treatment2", "Sum Sq"] # Complete Model (only)


## Formulas

SS.treats <- SStot - (SS.treat1 + SS.treat2 + SSres) # Complete Model (only)
SSres <- SStot - (SS.treat1 + SS.treat2 + SS.treats) # Complete Model
SSres <- SStot - (SS.treat1 + SS.treat2) # Additive Model


## Calculating SS Manually [!!! THERE MIGHT BE SOME BUGS !!!]

### Balanced Design (look at table(treatments))

SS.treat1 <- sum(n * b * (M.treat1 - M)^2)
SS.treat2 <- sum(n * g * (M.treat2 - M)^2) 

SS.treat1 <- 0
for(i in 1:g)
{
  SS.treat1 <- SS.treat1 + n*b * (M.treat1[i] - M)^2
}

SS.treat2 <- 0
for(j in 1:b)
{
  SS.treat2 <- SS.treat2 + n*g * (M.treat2[j] - M)^2
}


SS.treats <- 0
h <- 1
for(i in 1:g)
{
  for(j in 1:b)
  {
    SS.treats <- SS.treats + n * (M.treats[h] - M.treat1[i] - M.treat2[j] + M)^2
    h <- h + 1
  }
}

SSres <- 0
h <- 1
l <- 1
for(i in 1:g)
{
  for(j in 1:b)
  {
    for(k in 1:n)
    {
      SSres <- SSres + sum((target[treatments == treats[h]][k] - M.treats[h])^2)
      l <- l + 1
    }
    h <- h + 1
  }
}


### Unbalanced Design

SS.treat1 <- 0
for(i in 1:g)
{
  SS.treat1 <- SS.treat1 + table(treatment1)[i] * (M.treat1[i] - M)^2
}

SS.treat2 <- 0
for(j in 1:b)
{
  SS.treat2 <- SS.treat2 + table(treatment2)[j] * (M.treat2[j] - M)^2
}

SS.treats <- 0
h <- 1
for(i in 1:g)
{
  for(j in 1:b)
  {
    SS.treats <- SS.treats + table(treatments)[h] * (M.treats[h] - M.treat1[i] - M.treat2[j] + M)^2
    h <- h + 1
  }
}

SSres <- 0
h <- 1
l <- 1
for(i in 1:g)
{
  for(j in 1:b)
  {
    for(k in 1:table(treatments)[h])
    {
      SSres <- SSres + sum((target[treatments == treats[h]][k] - M.treats[h])^2)
      l <- l + 1
    }
    h <- h + 1
  }
}


# Degrees of Freedom

dofs.res <- fit.aov2.int$df.residual # Complete Model -> g*b * (n - 1)
dofs.res <- fit.aov2.ad$df.residual # Additive Model -> g*b*n - g - b + 1

dofs.treat1 <- summary.aov(fit.aov2.int)[[1]][1, "Df"] # g - 1
dofs.treat2 <- summary.aov(fit.aov2.int)[[1]]["treatment2", "Df"] # b - 1
dofs.treats <- summary.aov(fit.aov2.int)[[1]]["treatment1:treatment2", "Df"] # Complete Model (only) -> (g - 1) * (b - 1)


# Tests

## Global test for the significance of the interactions (Complete Model only!)

F.INT <- (SS.treats / dofs.treats) / (SSres / dofs.res)
P.INT <- 1 - pf(F.INT, dofs.treats, dofs.res) 
P.INT

## Global test for the significance of the first treatment 

F.T1 <- (SS.treat1 / dofs.treat1) / (SSres / dofs.res)
P.T1 <- 1 - pf(F.T1, dofs.treat1, dofs.res) 
P.T1

## Global test for the significance of the second treatment 

F.T2 <- (SS.treat2 / dofs.treat2) / (SSres / dofs.res)
P.T2 <- 1 - pf(F.T2, dofs.treat2, dofs.res) 
P.T2

## Note: These aren't the only tests we can do!
## Global test for the joint significance of the two treatments 

F.TOT <- ((SS.treat1 + SS.treat2) / (dofs.treat1 + dofs.treat2)) / (SSres / dofs.res)
P.TOT <- 1 - pf(F.TOT, dofs.treat1 + dofs.treat2, dofs.res) 
P.TOT


##### Confidence Intervals -----

alpha <- 0.10
k <- (g*b)*(g*b-1)/2 # Complete Model
k <- (g*b)*(g*b-1)/2 - (g+b)*(g*b-g-b+1)/2 # Additive Model
dofs <- fit.aov2.int$df.residual # Complete Model: g*b*(n - 1)
dofs <- fit.aov2.ad$df.residual # Additive Model: g*b*n - g-b+1

qT <- qt(1 - alpha/(2*k), dofs)

Mediag <- tapply(target, treatments, mean)
SSres <- sum(residuals(fit.aov2.int)^2) # Complete Model
SSres <- sum(residuals(fit.aov2.ad)^2) # Additive Model
S <- SSres / dofs

ng <- table(treatments)

CI.range <- NULL
for(i in 1:((g*b)-1)) 
{
  for(j in (i+1):(g*b)) 
  {
    inf <- Mediag[i] - Mediag[j] - qT * sqrt(S * (1/ng[i] + 1/ng[j]))
    sup <- Mediag[i] - Mediag[j] + qT * sqrt(S * (1/ng[i] + 1/ng[j]))
    interval_i <- cbind(inf, sup)
    rownames(interval_i) <- paste(treats[i], "-", treats[j])
    CI.range <- rbind(CI.range, interval_i)
  }
}
CI.range

h <- 1
plot(c(1, (g*b)*(g*b-1)/2), range(CI.range), pch = '', xlab = 'Pairs Treat', ylab = 'Conf. Int. tau Target')
for(i in 1:((g*b)-1)) 
{
  for(j in (i+1):(g*b)) 
  {
    lines (c(h, h), c(CI.range[h, 1], CI.range[h, 2]), col = 'grey55'); 
    points(h, Mediag[i] - Mediag[j], pch = 16, col = 'grey55'); 
    points(h, CI.range[h, 1], col = rainbow(g*b)[j], pch = 16); 
    points(h, CI.range[h, 2], col = rainbow(g*b)[i], pch = 16); 
    h <- h + 1
  }
}
abline(h = 0)


##### Reduced additive model (ANOVA one-way) -----

# X.ik = mu + beta.i + eps.ik; eps.ik~N(0,sigma^2), 
#     i=1,2,3 (effect treatment1)
fit.aov1 <- aov(target ~ treatment1)
summary.aov(fit.aov1)

# Interval for the differences (reduced additive model)

SSres <- sum(residuals(fit.aov1)^2)
dofs <- fit.aov1$df.residual
dofs <- (n*b-1)*g # if fitted ANOVA against treatment1
dofs <- (n*g-1)*b # if fitted ANOVA against treatment2

k <- g * (g - 1) / 2 # Change "g" with "b" if fitted ANOVA against treatment2 
alpha <- 0.05
ng <- table(treatment1) # Change "1" with "2" if fitted ANOVA against treatment2 

Mediag <- tapply(target, treatment1, mean) # Change "1" with "2" if fitted ANOVA against treatment2 
S <- SSres / dofs

qT <- qt(1 - alpha/(2*k), dofs)

treat <- levels(treatment1) # Change "1" with "2" if fitted ANOVA against treatment2 

CI.range <- NULL
for(i in 1:(g-1)) # Change "g" with "b" if fitted ANOVA against treatment2 
{
  for(j in (i+1):g) # Change "g" with "b" if fitted ANOVA against treatment2 
  {
    inf <- Mediag[i] - Mediag[j] - qT * sqrt(S * (1/ng[i] + 1/ng[j]))
    sup <- Mediag[i] - Mediag[j] + qT * sqrt(S * (1/ng[i] + 1/ng[j]))
    interval_i <- cbind(inf, sup)
    rownames(interval_i) <- paste(treat[i], "-", treat[j])
    CI.range <- rbind(CI.range, interval_i)
  }
}
CI.range


#### Two-ways MANOVA ----

### Try It Out! ###
rm(list = ls())
graphics.off()
par(mfrow=c(1,1))
cat("\014")
data <- read.table('datasets/data_manova.txt', header = T)
### Try It Out! ###

target <- data[, 1:2] # variables

treatment1 <- factor(data$treatment1, labels = c('LvA', 'LvB', 'LvC')) 
treatment2 <- factor(data$treatment2, labels = c('LvX', 'LvY', 'LvZ')) 

treatments <- factor(paste(treatment1, treatment2, sep=''))
treatments

g <- length(levels(treatment1))
b <- length(levels(treatment2))
p <- dim(target)[2]
n <- dim(target)[1]/(g*b) # group sizes
N <- n*g*b
treats <- levels(treatments)


##### Visualize Data -----

for(i in 1:p)
{
  layout(matrix(c(1, 1, 2, 3), 2, byrow = T))
  boxplot(target[, i] ~ treatments, main = paste('Model with Interac. T1+T2 (', colnames(target)[i], ')', sep = ''),
          ylab = colnames(target)[i], col = 'grey95')
  boxplot(target[, i] ~ treatment1, main = 'Only Treatment 1', ylab = colnames(target)[i], col = c('red','blue'))
  boxplot(target[, i] ~ treatment2, main = 'Only Treatment 2', ylab = colnames(target)[i], col = c('forestgreen','gold'))
}

par(mfrow=c(1,1))


##### Verify Assumptions -----

# 1)  normality (multivariate) in each group
Ps <- NULL
for(i in 1:(g*b)) {
  mvn.test <- mvn(data = target[treatments == treats[i], ])
  Ps <- rbind(Ps, mvn.test$multivariateNormality$`p value`)
}
dimnames(Ps)[[1]] <- treats
dimnames(Ps)[[2]] <- c("p-value")
Ps

# 2) same covariance structure (= same covariance matrix Sigma)

covs <- list()
covs[["S"]] <- cov(target)
for(i in 1:(g*b))
{
  cov <- cov(target[treatments == treats[i], ])
  covs[[paste("S", i, sep = '')]] <- cov
}
covs

# Box's M test for homogeneity of covariance matrices
# Should be done only if n_i > ~20 for all i, p < 5 and g < 5
# WARNING: Very sensitive to departure from normality and too restrictive for MANOVA
# especially if we have a high number of samples

summary(boxM(target, treatments)) #!library(heplots)
boxM(target, treatments)$p.value

# Heatmap on the covariance matrices

par(mfrow=c(1,(g*b)))
quant <- NULL
for(i in 2:((g*b)+1))
{
  quant <- rbind(quant, covs[[i]])
}
for(i in 1:(g*b))
{
  Si <- paste("S", i, sep = '')
  image(covs[[Si]], col = heat.colors(100), main = paste('Cov.', Si), asp = 1, axes = FALSE, breaks = quantile(quant, (0:100)/100, na.rm = TRUE))
}
par(mfrow=c(1,1))


##### Fit -----

fit <- manova(as.matrix(target) ~ treatment1 + treatment2 + treatment1:treatment2)
summary.manova(fit)

fit2 <- manova(as.matrix(target) ~ treatment1 + treatment2)
summary.manova(fit2)

summary.aov(fit2)


##### Confidence Intervals -----

alpha <- 0.10
k <- p*(g*b)*(g*b-1)/2 # Complete Model
k <- p*((g*b)*(g*b-1)/2 - (g+b)*(g*b-g-b+1)/2) # Additive Model
dofs <- g*b*(n - 1) # Complete Model
dofs <- g*b*n - g-b+1 # Additive Model

W <- summary.manova(fit)$SS$Residuals # Complete Model
W <- summary.manova(fit2)$SS$Residuals # Additive Model
qT <- qt(1 - alpha/(2*k), dofs)

means <- NULL
for(i in 1:(g*b)) {
  means <- rbind(means, sapply(target[treatments == treats[i], ], mean)) 
}
dimnames(means)[[1]] <- treats
means

inf <- list()
sup <- list()
CI <- list()
pairs <- combn((g*b), 2)

for (i in 1:ncol(pairs)) {
  idx1 <- pairs[1, i]
  idx2 <- pairs[2, i]
  
  m1 <- means[idx1, ]
  m2 <- means[idx2, ]
  
  inf_val <- m1 - m2 - qT * sqrt(diag(W) / dofs * (1/n + 1/n))
  sup_val <- m1 - m2 + qT * sqrt(diag(W) / dofs * (1/n + 1/n))
  
  inf[[paste("inf", idx1, idx2, sep = '')]] <- inf_val
  sup[[paste("sup", idx1, idx2, sep = '')]] <- sup_val
  
  CI[[paste(treats[idx1], "_", treats[idx2], sep = '')]] <- cbind(inf_val, sup_val)
}

CI

par(mfrow=c(2,p))
for(i in 1:p)
{
  boxplot(target[, i] ~ treatments, main = colnames(target)[i], ylim = range(target), col = rainbow(g))
}

for(k in 1:p)
{
  plot(c(1, (g*b)*((g*b) - 1)/2), pch = '', 
       xlim = c(1, (g*b)*((g*b) - 1)/2), ylim = c(range(CI)[1] - 3, range(CI)[2] + 3),
       xlab = 'Pairs Treat', ylab = paste('CI tau', k), 
       main = paste('CI tau', colnames(target)[k]))
  for(i in 1:((g*b)*((g*b) - 1)/2))
  {
    j <- pairs[1, i]
    l <- pairs[2, i]
    lines(c(i, i), c(CI[[i]][k, 1], CI[[i]][k, 2]))
    points(i, means[j, k] - means[l, k], pch = 16); 
    points(i, CI[[i]][k, 1], col = rainbow(g*b)[l], pch = 16); 
    points(i, CI[[i]][k, 2], col = rainbow(g*b)[j], pch = 16); 
  }
  abline(h = 0)
}

par(mfrow=c(1,1))