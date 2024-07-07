###----------------------###
### Problem 3 (20210706) ###
###----------------------###

library(car)

rm(list = ls())
graphics.off()

data <- read.table('pc.txt', header = T)
head(data)

n <- dim(data)[1]


# a) ----------------------------------------------------------------------

m0 <- lm(price ~ freq:OS + cache_acc:OS, data = data)
summary(m0)

m0$coefficients
#         (Intercept)        freq:OSLinux          freq:OSMac      freq:OSWindows   OSLinux:cache_acc     OSMac:cache_acc OSWindows:cache_acc 
#         1345.656963           34.622543          121.212860           76.202523           -2.588699           -4.238733           -9.002267 

sum((m0$residuals)^2)/m0$df
# 11497.83


# b) ----------------------------------------------------------------------

par(mfrow=c(2,2))
plot(m0)
par(mfrow=c(1,1))

shapiro.test(m0$residuals)
# p-value = 0.1274 -> OK

C <- rbind(c(0,1,0,0,0,0,0),
           c(0,0,1,0,0,0,0),
           c(0,0,0,1,0,0,0),
           c(0,0,0,0,1,0,0),
           c(0,0,0,0,0,1,0),
           c(0,0,0,0,0,0,1))

linearHypothesis(m0, C, c(0,0,0,0,0,0))
# 4.48e-10 *** -> the factor "OS" has a significant impact on the mean price


# c) ----------------------------------------------------------------------

C <- rbind(c(0,0,0,0,1,0,0),
           c(0,0,0,0,0,1,0),
           c(0,0,0,0,0,0,1))

linearHypothesis(m0, C, c(0,0,0))
# 0.3696 -> the factor "cache_acc" doesn't have a significant impact on the mean price


# d) ----------------------------------------------------------------------

m1 <- lm(price ~ freq:OS, data = data)
summary(m1)

m1$coefficients
#    (Intercept)   freq:OSLinux     freq:OSMac freq:OSWindows 
#     1275.64332       44.16969      123.06016       59.93425 

sum((m1$residuals)^2)/m1$df
# 11541.06


# e) ----------------------------------------------------------------------

alpha <- 0.10

new.datum <- data.frame(freq = 3.2, cache_acc = 10, OS = "Windows")

# Conf. int. for the mean
Conf <- predict(m1, new.datum, interval = 'confidence', level = 1 - alpha)  
Conf
#        fit      lwr      upr
# 1 1467.433 1427.011 1507.855


# Extra -------------------------------------------------------------------

treatment <- factor(data$OS)
target <- data[, 2:3] # variables

n <- length(treatment)
ng <- table(treatment)
treat <- levels(treatment)
g <- length(levels(treatment))
p <- dim(target)[2]

library(MVN)

Ps <- NULL
for(i in 1:g) {
  mvn.test <- mvn(data = target[treatment == treat[i], ])
  Ps <- rbind(Ps, mvn.test$multivariateNormality$`p value`)
}
dimnames(Ps)[[1]] <- treat
dimnames(Ps)[[2]] <- c("p-value")
Ps
#           p-value
# Linux   0.3412804
# Mac     0.4395498
# Windows 0.4218403

library(heplots)

summary(boxM(target, treatment))
boxM(target, treatment)$p.value
# 0.9685212

fit <- manova(as.matrix(target) ~ treatment)
summary.manova(fit)
# Pr(>F) = 0.7856

# - 

target <- m0$fitted
treatment <- factor(data$treatment)

n <- length(treatment)      
ng <- table(treatment)       
treat <- levels(treatment)      
g <- length(levels(treatment))

Ps <- NULL
for(i in 1:g)
{
  Ps <- rbind(Ps, shapiro.test(target[treatment == treat[i]])$p)
}
dimnames(Ps)[[1]] <- treat
dimnames(Ps)[[2]] <- c("p-value")
Ps
#           p-value
# Linux   0.2423236
# Mac     0.4963602
# Windows 0.9976613

Var <- NULL
for(i in 1:g)
{
  Var <- rbind(Var, var(target[treatment == treat[i]]))
}
dimnames(Var)[[1]] <- treat
dimnames(Var)[[2]] <- c("sigma^2")
Var
#           sigma^2
# Linux    1145.459
# Mac     13890.637
# Windows  5855.855

bartlett.test(target, treatment)
# p-value = 6.431e-06 -> mi sa che non si anovizza... :(

# -

treatment <- factor(data$OS)
target <- data[, 1:3] # variables

n <- length(treatment)
ng <- table(treatment)
treat <- levels(treatment)
g <- length(levels(treatment))
p <- dim(target)[2]

library(MVN)

Ps <- NULL
for(i in 1:g) {
  mvn.test <- mvn(data = target[treatment == treat[i], ])
  Ps <- rbind(Ps, mvn.test$multivariateNormality$`p value`)
}
dimnames(Ps)[[1]] <- treat
dimnames(Ps)[[2]] <- c("p-value")
Ps
#           p-value
# Linux   0.5585009
# Mac     0.5875098
# Windows 0.9038388

library(heplots)

summary(boxM(target, treatment))
boxM(target, treatment)$p.value
# 0.645222

fit <- manova(as.matrix(target) ~ treatment)
summary.manova(fit)
# Pr(>F) = 1.105e-05 ***

# -

treatment <- factor(data$OS)
target <- cbind(m0$fitted, data[, 2:3]) # variables

n <- length(treatment)
ng <- table(treatment)
treat <- levels(treatment)
g <- length(levels(treatment))
p <- dim(target)[2]

library(MVN)

Ps <- NULL
for(i in 1:g) {
  mvn.test <- mvn(data = target[treatment == treat[i], ])
  Ps <- rbind(Ps, mvn.test$multivariateNormality$`p value`)
}
dimnames(Ps)[[1]] <- treat
dimnames(Ps)[[2]] <- c("p-value")
Ps
#           p-value
# Linux         0
# Mac           0
# Windows       0

# -

treatment <- factor(data$OS)
target <- cbind(lm(price ~ OS + freq:OS + cache_acc:OS, data = data)$fitted, data[, 2:3]) # variables

n <- length(treatment)
ng <- table(treatment)
treat <- levels(treatment)
g <- length(levels(treatment))
p <- dim(target)[2]

library(MVN)

Ps <- NULL
for(i in 1:g) {
  mvn.test <- mvn(data = target[treatment == treat[i], ])
  Ps <- rbind(Ps, mvn.test$multivariateNormality$`p value`)
}
dimnames(Ps)[[1]] <- treat
dimnames(Ps)[[2]] <- c("p-value")
Ps
#         p-value
# Linux         0
# Mac           0
# Windows       0