###----------------------###
### Problem 3 (20220209) ###
###----------------------###

library(car)

rm(list = ls())
graphics.off()

data <- read.table('wine.txt', header = T)
head(data)

n <- dim(data)[1]


# a) ----------------------------------------------------------------------

m0 <- lm(alcohol ~ -1 + type + sugar:type, data = data)
summary(m0)

m0$coefficients
#         typeRed        typeRose       typeWhite   typeRed:sugar  typeRose:sugar typeWhite:sugar 
#      -1.2533849      -1.7119121      -0.7004341       0.7272803       0.6621416       0.5962484 

sqrt(sum((m0$residuals)^2)/m0$df)
# 0.6726414

par(mfrow=c(2,2))
plot(m0)
par(mfrow=c(1,1))

shapiro.test(m0$residuals)
# p-value = 0.7562


# b) ----------------------------------------------------------------------

C <- rbind(c(1,0,0,0,0,0),
           c(0,1,0,0,0,0),
           c(0,0,1,0,0,0))

linearHypothesis(m0, C, c(0,0,0))["Pr(>F)"]
# 0.00493 ** -> at 1% we reject the hypothesis that there is not a significant dependence of the mean alcohol content on the type of wine
  
C <- rbind(c(0,0,0,1,0,0),
           c(0,0,0,0,1,0),
           c(0,0,0,0,0,1))

linearHypothesis(m0, C, c(0,0,0))["Pr(>F)"]
# < 2.2e-16 *** -> at 1% we reject the hypothesis that there is not a significant dependence of the mean alcohol content on the sugar content


# c) ----------------------------------------------------------------------

C <- rbind(c(0,0,1,0,0,0))

linearHypothesis(m0, C, c(0))["Pr(>F)"]
# 0.2981 -> the intercept for typeWhite is not significantly relevant

# With another formulation...
m0.alt <- lm(alcohol ~ type + sugar:type, data = data)
summary(m0.alt)

C <- rbind(c(0,1,0,0,0,0),
           c(0,0,1,0,0,0))

linearHypothesis(m0.alt, C, c(0,0))["Pr(>F)"]
# 0.5486 -> at 1% we don't reject the hypothesis that there is not a significant dependence of the mean alcohol content on the type of wine

m1 <- lm(alcohol ~ sugar:type, data = data)
summary(m1)

m1$coefficients
#     (Intercept)   sugar:typeRed  sugar:typeRose sugar:typeWhite 
#      -1.2432932       0.7267769       0.6392491       0.6223343 

sqrt(sum((m1$residuals)^2)/m1$df)
# 0.6711115


# d) ----------------------------------------------------------------------

new.datum <- data.frame(sugar = 20, type = "Red")
alpha = 0.01

# Pred. int. for a new obs
Pred <- predict(m1, new.datum, interval = 'prediction', level = 1-alpha) 
Pred
#        fit      lwr      upr
#   13.29224 11.53151 15.05298


# Extra -------------------------------------------------------------------

col.lab <- ifelse(data[, 3] %in% c("Rose", "Red"), 
                  ifelse(data[, 3] %in% c("Rose"), 
                         'pink', 'red'), 
                  'grey')

plot(data[, c(2, 1)], xlab = 'Sugar', ylab = 'Alcohol', col = col.lab, pch = 19, las = 1, xlim = c(-1, 28), ylim = c(-3, 20))

x <- seq(0, 28, by = 0.1)
b <- coef(m0)
lines(x, b[1] + b[4]*x, col = "red", lwd = 2)
lines(x, b[2] + b[5]*x, col = "pink", lwd = 2)
lines(x, b[3] + b[6]*x, col = "grey", lwd = 2)
abline(h = 0, v = 0)
