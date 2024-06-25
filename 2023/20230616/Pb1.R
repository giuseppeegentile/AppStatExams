###---------------------------------------------------###
### Problem 1: Noise pollution due to cars (20230616) ###
###---------------------------------------------------###

rm(list = ls())
graphics.off()

data <- read.table('noise.txt', header = T)
head(data)


# a) ----------------------------------------------------------------------

target <- data$noise

treatment1 <- factor(data$fuel, labels = c("Die", "Eth", "Gas")) 
treatment2 <- factor(data$category, labels = c("Com", "Pas")) 

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
# Normality (univariate) in each group: verified

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
# Same covariance structure (= same sigma^2): verified

fit.aov2.int <- aov(target ~ treatment1 + treatment2 + treatment1:treatment2)
summary.aov(fit.aov2.int)


# b) ----------------------------------------------------------------------

# By looking at the summary or...

M <- mean(target) # overall mean
M.treat1 <- tapply(target, treatment1, mean) # mean per treat 1
M.treat2 <- tapply(target, treatment2, mean) # mean per treat 2
M.treats <- tapply(target, treatments, mean) # mean per treat 1 x treat 2

SStot <- sum((target - M)^2)
SS.treat1 <- sum(n * b * (M.treat1 - M) ^ 2)
SS.treat2 <- sum(n * g * (M.treat2 - M) ^ 2)  
SSres <- sum(residuals(fit.aov2.int)^2)
SS.treats <- SStot - (SS.treat1 + SS.treat2 + SSres)

res.dofs <- g * b * (n - 1) # Complete Model

# Global test for the significance of the second treatment 

F.T2 <- (SS.treat2 / (b - 1)) / (SSres / res.dofs)
P.T2 <- 1 - pf(F.T2, b - 1, res.dofs) 
P.T2
# 0.1748649 -> not significant

# Global test for the significance of the interactions 

F.INT <- (SS.treats / (g - 1) * (b - 1)) / (SSres / res.dofs)
P.INT <- 1 - pf(F.INT, (g - 1) * (b - 1), res.dofs) 
P.INT
# 0.06992042 -> Reject at 10%, don't reject at 1%,5% -> ?

# ... we don't have strong evidence that the interaction has effect

fit.aov2.ad <- aov(target ~ treatment1 + treatment2)
summary.aov(fit.aov2.ad)

# Again, by looking at the summary or...

SSres <- sum(residuals(fit.aov2.ad)^2)
res.dofs <- g*b*n - g - b + 1 # Additive Model

# Global test for the significance of the second treatment 

F.T2 <- (SS.treat2 / (b - 1)) / (SSres / res.dofs)
P.T2 <- 1 - pf(F.T2, b - 1, res.dofs) 
P.T2
# 0.1811203 -> not significant

# ... we don't have strong evidence that the second treatment has effect

fit.aov1 <- aov(target ~ treatment1)
summary.aov(fit.aov1)


# c) ----------------------------------------------------------------------

M # mu
M.treat1 - M # tau_i
sum(residuals(fit.aov1)^2)/fit.aov1$df # sigma^2


# d) ----------------------------------------------------------------------

k <- g * (g - 1) / 2
alpha <- 0.05

SSres <- sum(residuals(fit.aov1)^2)

IC <- rbind(Eth_Die = c(M.treat1[2] - M.treat1[1] - qt(1 - alpha/(2*k), (n*b-1)*g) * sqrt(SSres/((n*b-1)*g) *(1/(n*b) + 1/(n*b))), 
                        M.treat1[2] - M.treat1[1] + qt(1 - alpha/(2*k), (n*b-1)*g) * sqrt(SSres/((n*b-1)*g) *(1/(n*b) + 1/(n*b)))),
            Gas_Die = c(M.treat1[3] - M.treat1[1] - qt(1 - alpha/(2*k), (n*b-1)*g) * sqrt(SSres/((n*b-1)*g) *(1/(n*b) + 1/(n*b))), 
                        M.treat1[3] - M.treat1[1] + qt(1 - alpha/(2*k), (n*b-1)*g) * sqrt(SSres/((n*b-1)*g) *(1/(n*b) + 1/(n*b)))),
            Gas_Eth = c(M.treat1[3] - M.treat1[2] - qt(1 - alpha/(2*k), (n*b-1)*g) * sqrt(SSres/((n*b-1)*g) *(1/(n*b) + 1/(n*b))), 
                        M.treat1[3] - M.treat1[2] + qt(1 - alpha/(2*k), (n*b-1)*g) * sqrt(SSres/((n*b-1)*g) *(1/(n*b) + 1/(n*b)))))  
colnames(IC) <- c('Inf', 'Sup')
IC
# We should consider only two groups: Diesel+Gasoline and Ethanol,
# since the CI for the mean difference between Diesel and Gasoline contains 0
# and both Diesel and Gasoline produce more noise than Ethanol


# Extra -------------------------------------------------------------------

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
SS.treats

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
SSres

SSres <- SStot - (SS.treat1 + SS.treat2 + SS.treats) # Complete Model
SSres <- SStot - (SS.treat1 + SS.treat2) # Additive Model
