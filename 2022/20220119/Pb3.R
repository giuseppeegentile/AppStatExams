###----------------------###
### Problem 3 (20220119) ###
###----------------------###

library(car)

rm(list = ls())
graphics.off()

data <- read.table('tattoo.txt', header = T)
head(data)

n <- dim(data)[1]


# a) ----------------------------------------------------------------------

m0 <- lm(price ~ -1 + method + dimension:method + ncolors:method, data = data) # easier to extract coeffs

m0$coefficients
#           methodhandmade            methodmachine methodhandmade:dimension  methodmachine:dimension   methodhandmade:ncolors    methodmachine:ncolors 
#                11.817743                16.683624                15.737418                 8.487567                 3.553972                 2.561518

m0 <- lm(price ~ method + dimension:method + ncolors:method, data = data)
summary(m0)

sum((m0$residuals)^2)/m0$df
# 68.20368


# b) ----------------------------------------------------------------------

par(mfrow=c(2,2))
plot(m0)
par(mfrow=c(1,1))

shapiro.test(m0$residuals)
# p-value = 0.2647

C <- rbind(c(0,1,0,0,0,0))

linearHypothesis(m0, C, c(0))["Pr(>F)"]
# 0.5505 -> at level 5% we can state that the factor "method" doesn't have a significant impact


# c) ----------------------------------------------------------------------

C <- rbind(c(0,0,0,0,1,0),
           c(0,0,0,0,0,1))

linearHypothesis(m0, C, c(0,0))["Pr(>F)"]
# 2.916e-11 *** -> the number of colors has a significant impact on the cost of a tattoo


# d) ----------------------------------------------------------------------

m1 <- lm(price ~ dimension:method + ncolors:method, data = data)
summary(m1)

m1$coefficients
#              (Intercept) dimension:methodhandmade  dimension:methodmachine   methodhandmade:ncolors    methodmachine:ncolors 
#                14.202578                15.422247                 8.715589                 3.506168                 2.646478

sum((m1$residuals)^2)/m1$df
# 67.96563


# e) ----------------------------------------------------------------------

alpha <- 0.05
df <- m0$df # n - (r+1)

C <- rbind(c(1,0,0,0,0,0),
           c(1,1,0,0,0,0))

p <- 3

center <- C %*% m0$coefficients
shape <- C %*% vcov(m0) %*% t(C)

qT <- qt(1 - alpha/(2*p), df)

BF.I <- cbind(center - sqrt(diag(shape)) * qT,
              center,
              center + sqrt(diag(shape)) * qT)
colnames(BF.I) <- c('inf', 'center', 'sup')
rownames(BF.I) <- c("Fixed Handmade", "Fixed Machine")
BF.I
#                      inf   center      sup
# Fixed Handmade -1.942338 11.81774 25.57783
# Fixed Machine   2.648723 16.68362 30.71852

new.datum <- data.frame(dimension = 6.5, ncolors = 1, method = "handmade")

# Conf. int. for the mean
Conf <- predict(m1, new.datum, interval = 'confidence', level = 1 - alpha/p)  
Conf
#        fit      lwr      upr
#   117.9534 114.8999 121.0068

# OR (if the fixed cost doesn't depend on the method)

alpha <- 0.05
df <- m1$df # n - (r+1)

C <- rbind(c(1,0,0,0,0))

p <- 2

center <- C %*% m1$coefficients
shape <- C %*% vcov(m1) %*% t(C)

qT <- qt(1 - alpha/(2*p), df)

BF.I <- cbind(center - sqrt(diag(shape)) * qT,
              center,
              center + sqrt(diag(shape)) * qT)
colnames(BF.I) <- c('inf', 'center', 'sup')
rownames(BF.I) <- c("Fixed Cost")
BF.I
#                 inf   center      sup
# Fixed Cost 5.028641 14.20258 23.37651

new.datum <- data.frame(dimension = 6.5, ncolors = 1, method = "handmade")

# Conf. int. for the mean
Conf <- predict(m1, new.datum, interval = 'confidence', level = 1 - alpha/p)  
Conf
#        fit      lwr      upr
# 1 117.9534 115.0973 120.8094