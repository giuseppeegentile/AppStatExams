library(MVN)
library(car)
library(heplots)

data <- read.table("dinosaurs.txt", header=TRUE)
attach(data)
values <- data$length
values
# data could use some jitter
set.seed(1)
values <- values + cbind(rnorm(dim(data)[1], sd=0.025))
values

group1 <- factor(diet)
group2 <- factor(period)
group1x2 <- factor(paste(group1,group2,sep="-"))

g <- length(levels(group1)) # number of factor1 levels
b <- length(levels(group2)) # number of factor2 levels
table1 <- table(group1)
table2 <- table(group2)
n1 <- sum(table(group1)) 
n2 <- sum(table(group2))

for(i in 1:6){
  print(length(group1x2==levels(group1x2)[i]))
}

M           <- mean(values) # overall mean
Mgroup1      <- tapply(values, group1, mean) 
Mgroup2     <- tapply(values, group2, mean) 
Mgroup1x2 <- tapply(values, group1x2, mean) 

# Two-ways ANOVA
# Model with interaction (complete model): 
# X.ijk = mu + tau.i + beta.j + gamma.ij + eps.ijk; eps.ijk ~ N(0,sigma^2), 
#     i=1,2 (effect station), j=1,2 (effect gasoline)
fit.aov2.int <- aov(values ~ group1 + group2 + group1:group2)
summary.aov(fit.aov2.int)

fit.aov2.int$coefficients


ps <- c()
for(i in 1:(g*b)){
  ps <- c(ps, shapiro.test(values[which(group1x2==levels(group1x2)[i])])$p)
}
ps

#Boxcox transformations to make it gaussian
lambdas <- c()
for(i in 1:(g*b)){
  lambdas <- c(lambdas,powerTransform(values[which(group1x2==levels(group1x2)[i])])$lambda)
}

for(i in 1:(g*b)){
  values[which(group1x2==levels(group1x2)[i])] <- bcPower(values[which(group1x2==levels(group1x2)[i])], lambdas[i])
}

ps <- c()
for(i in 1:(g*b)){
  ps <- c(ps, shapiro.test(values[which(group1x2==levels(group1x2)[i])])$p)
}
ps

# Interactive model
fit.aov2.int <- aov(values ~ group1 + group2 + group1:group2)
summary.aov(fit.aov2.int)

# remove the interaction
# Additive model:
# X.ijk = mu + tau.i + beta.j + eps.ijk; eps.ijk ~ N(0,sigma^2), 
#     i=1,2 (effect station), j=1,2 (effect gasoline)
fit.aov2.ad <- aov(values ~ group1 + group2)
summary.aov(fit.aov2.ad)

Mgroup1x2
k <- (g*b)*(g*b-1)/2 - (g+b)*(g*b-g-b+1)/2 # Additive Model
alpha <-0.05
cfr.t <- qt(1-alpha/(2*k), fit.aov2.ad$df.residual)
SSres <- sum(fit.aov2.ad$residuals^2)/fit.aov2.ad$df.residual
ng <- table(group1x2)

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
