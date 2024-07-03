###----------------------###
### Problem 3 (20220616) ###
###----------------------###

library(car)
library(lme4)
library(insight)
library(lattice)

rm(list = ls())
graphics.off()

data <- read.table('danceability.txt', header = T)
head(data)

n <- dim(data)[1]


# a) ----------------------------------------------------------------------

m0 <- lm(danceability ~ loudness + energy + tempo, data = data)
summary(m0)

m0$coefficients
# (Intercept)    loudness      energy       tempo 
#  9.18676786  0.09930218  0.07258300 -0.00888967 

summary(m0)$sigma
# 0.9350057


# b) ----------------------------------------------------------------------

par(mfrow=c(2,2))
plot(m0)
par(mfrow=c(1,1))

shapiro.test(m0$residuals)
# p-value = 0.6654 -> OK


# c) ----------------------------------------------------------------------

linearHypothesis(m0,
                 rbind(c(0,1,0,0),
                       c(0,0,1,0)),
                 c(0,0))["Pr(>F)"]
# 2.112e-06 ***


# d) ----------------------------------------------------------------------

m1 <- lm(danceability ~ tempo, data = data)
summary(m1)

m1$coefficients
# (Intercept)        tempo 
# 8.119606059 -0.008706886 

summary(m1)$sigma
# 0.9639437


# e) ----------------------------------------------------------------------

m2 <- lmer(danceability ~ tempo + (1|genre),
           data = data)
summary(m2)

sigma2_eps <- as.numeric(get_variance_residual(m2))
sigma2_b <- as.numeric(get_variance_random(m2))

PVRE <- sigma2_b/(sigma2_b+sigma2_eps)
PVRE
# 0.09995861


# f) ----------------------------------------------------------------------

dotplot(ranef(m2, condVar = T))
# R&B is the genre associated to the highest danceability