###----------------------###
### Problem 1 (20220909) ###
###----------------------###

library(car)

rm(list = ls())
graphics.off()

data <- read.table('dinosaurs.txt', header = T)
head(data)


# a) ----------------------------------------------------------------------

target <- data$length

treatment1 <- factor(data$diet, labels = c('Car', 'Her', 'Omn')) 
treatment2 <- factor(data$period, labels = c('Cre', 'Jur')) 

treatments <- factor(paste(treatment1, treatment2, sep=''))
treatments

g <- length(levels(treatment1)) # number of factor1 levels
b <- length(levels(treatment2)) # number of factor2 levels
n <- length(target)/(g*b) # group sizes
treats <- levels(treatments)

Ps <- NULL
for(i in 1:(g*b))
{
  Ps <- rbind(Ps, shapiro.test(target[treatments == treats[i]])$p)
}
dimnames(Ps)[[1]] <- treats
dimnames(Ps)[[2]] <- c("p-value")
Ps
# All the p-values are small -> we need to transform the target variable

hist(target)

lambda.target <- powerTransform(target) 
bc.target <- bcPower(target, lambda.target$lambda[1]) 

hist(bc.target)

target <- bc.target

Ps <- NULL
for(i in 1:(g*b))
{
  Ps <- rbind(Ps, shapiro.test(target[treatments == treats[i]])$p)
}
dimnames(Ps)[[1]] <- treats
dimnames(Ps)[[2]] <- c("p-value")
Ps
# OK

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
# p-value = 0.2831 -> OK

fit.aov2.int <- aov(target ~ treatment1 + treatment2 + treatment1:treatment2)
summary.aov(fit.aov2.int)


# b) ----------------------------------------------------------------------

fit.aov2.ad <- aov(target ~ treatment1 + treatment2)
summary.aov(fit.aov2.ad)

fit.aov1 <- aov(target ~ treatment1)
summary.aov(fit.aov1)


# c) ----------------------------------------------------------------------

sum(residuals(fit.aov1)^2) / fit.aov1$df.residual # sigma^2
# 1.362989

mean(target) # mu
# 2.001893

tapply(target, treatment1, mean) - mean(target) # tau
# Car        Her        Omn 
# -0.3894968  0.3165435 -0.8451737


# d) ----------------------------------------------------------------------

SSres <- sum(residuals(fit.aov1)^2)
dofs <- fit.aov1$df.residual # (n*b-1)*g

k <- g * (g - 1) / 2
alpha <- 0.05
ng <- table(treatment1)

Mediag <- tapply(target, treatment1, mean)
S <- SSres / dofs

treat <- levels(treatment1)

CI.range <- NULL
for(i in 1:(g-1)) 
{
  for(j in (i+1):g) 
  {
    inf <- Mediag[i] - Mediag[j] - qt(1 - alpha/(2*k), dofs) * sqrt(S * (1/ng[i] + 1/ng[j]))
    est <- Mediag[i] - Mediag[j]
    sup <- Mediag[i] - Mediag[j] + qt(1 - alpha/(2*k), dofs) * sqrt(S * (1/ng[i] + 1/ng[j]))
    interval_i <- cbind(inf, est, sup)
    rownames(interval_i) <- paste(treat[i], "-", treat[j])
    CI.range <- rbind(CI.range, interval_i)
  }
}
CI.range
#                  inf        est        sup
# Car - Her -1.0811380 -0.7060404 -0.3309427
# Car - Omn -0.2053354  0.4556768  1.1166891
# Her - Omn  0.5359916  1.1617172  1.7874428


# Extra -------------------------------------------------------------------

# Inverse of Box-Cox Transformation

exp(log(lambda.target$lambda[1] * bc.target + 1) / lambda.target$lambda[1])
data$length

exp(log(lambda.target$lambda[1] * CI.range + 1) / lambda.target$lambda[1])


# Appropriate Statistical Tests

M <- mean(target) # overall mean
M.treat1 <- tapply(target, treatment1, mean) # mean per treat 1
M.treat2 <- tapply(target, treatment2, mean) # mean per treat 2
M.treats <- tapply(target, treatments, mean) # mean per treat 1 x treat 2

SStot <- sum((target - M)^2)


## Starting from the Complete Model

SSres <- sum(residuals(fit.aov2.int)^2)
SS.treats <- SStot - (SS.treat1 + SS.treat2 + SSres)

res.dofs <- fit.aov2.int$df.residual # g*b * (n - 1)

### Global test for the significance of the interactions 

F.INT <- (SS.treats / (g - 1) * (b - 1)) / (SSres / res.dofs)
P.INT <- 1 - pf(F.INT, (g - 1) * (b - 1), res.dofs) 
P.INT

### Global test for the significance of the first treatment 

F.T1 <- (SS.treat1 / (g - 1)) / (SSres / res.dofs)
P.T1 <- 1 - pf(F.T1, g - 1, res.dofs) 
P.T1

### Global test for the significance of the second treatment 

F.T2 <- (SS.treat2 / (b - 1)) / (SSres / res.dofs)
P.T2 <- 1 - pf(F.T2, b - 1, res.dofs) 
P.T2


## Starting from the Additive Model

SSres <- sum(residuals(fit.aov2.ad)^2)
res.dofs <- fit.aov2.ad$df.residual # g*b*n - g - b + 1

### Global test for the significance of the first treatment 

F.T1 <- (SS.treat1 / (g - 1)) / (SSres / res.dofs)
P.T1 <- 1 - pf(F.T1, g - 1, res.dofs) 
P.T1

### Global test for the significance of the second treatment 

F.T2 <- (SS.treat2 / (b - 1)) / (SSres / res.dofs)
P.T2 <- 1 - pf(F.T2, b - 1, res.dofs) 
P.T2


# Manual SS

SS.treat1 <- 0
for(i in 1:g)
{
  SS.treat1 <- SS.treat1 + table(treatment1)[i] * (M.treat1[i] - M)^2
}
SS.treat1


SS.treat2 <- 0
for(j in 1:b)
{
  SS.treat2 <- SS.treat2 + table(treatment2)[j] * (M.treat2[j] - M)^2
}
SS.treat2


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
SS.treats


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
SSres


# Debugging

sum(table(treatments)[((i-1)*b + 1):(i*b)]) # table(treatment1)[i]
sum(table(treatments)[seq(j, g*b, by = b)]) # table(treatment2)[j]


SS.treat1 <- 0
for (i in 1:g) {
  n_i <- table(treatment1)[i]
  mean_i <- M.treat1[i]
  deviation <- (mean_i - M)^2
  cat("Level:", i, "n_j:", n_i, sum(table(treatments)[((i-1)*b + 1):(i*b)]), "mean_j:", mean_i, "deviation:", deviation, "\n")
  SS.treat1 <- SS.treat1 + n_i * deviation
}
SS.treat1


SS.treat2 <- 0
for (j in 1:b) {
  n_j <- table(treatment2)[j]
  mean_j <- M.treat2[j]
  deviation <- (mean_j - M)^2
  cat("Level:", j, "n_j:", n_j, sum(table(treatments)[seq(j, g*b, by = b)]), "mean_j:", mean_j, "deviation:", deviation, "\n")
  SS.treat2 <- SS.treat2 + n_j * deviation
}
SS.treat2

summary.aov(fit.aov2.int)[[1]]$"Sum Sq"[1:2]


M.treat1
M.treat2
M.treats

model.tables(fit.aov2.int, type = "means")$tables

# Discrepancy found for treatment2 and treatments!

# > M.treat2
#      Cre      Jur 
# 1.923222 2.176821 

# > model.tables(fit.aov2.int, type = "means")$tables$treatment2
#      Cre      Jur 
# 1.938809 2.142164 

# > M.treats
#   CarCre   CarJur   HerCre   HerJur   OmnCre   OmnJur 
# 1.596270 1.643537 2.229752 2.507520 1.106794 1.489557

# > model.tables(fit.aov2.int, type = "means")$tables$'treatment1:treatment2'
#            treatment2
# treatment1 Cre      Jur     
#        Car 1.596270 1.643537
#        Her 2.229752 2.507520
#        Omn 1.106794 1.489556

# > model.tables(fit.aov2.int, type = "means")$tables$treatments
#   CarCre   CarJur   HerCre   HerJur   OmnCre   OmnJur 
# 2.056154 1.897115 1.979077 2.050540 1.978877 2.155335

# ... so?

