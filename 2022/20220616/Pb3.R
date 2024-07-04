library(MASS)
library(nlmeU)
library(corrplot)
library(nlme)
library(lattice)
library(plot.matrix)
library(lme4)
library(insight)
library(car)
library(rgl)
library(glmnet)

data <- read.table("danceability.txt", header=TRUE)

attach(data)
names(data)
genre <- factor(genre)
# model fitting
lm1 <- lm(danceability ~ loudness +  energy + tempo)
summary(lm1)

# Assumptons: residuals gaussianity and homoscedastic residuals
par(mfrow=c(2,2))
plot(lm1)
shapiro.test(lm1$residuals)

# coefficients estimation
coefficients(lm1) # betas
sqrt(sum(lm1$residuals^2)/lm1$df.residual)
summary(lm1) # residual standard error is sigma



AIC(lm1)
BIC(lm1)

# linearHypothesis ----------------------------------------------------
# ncol = nparameters
# nrow tested linear combinations
linearHypothesis(lm1, rbind(c(0,1,0,0),
                            c(0,0,1,0)),   c(0,0) )
# no, together they are significant
summary(lm1)
# we can see however that alone energy is not significant at 5% level
# so we remove it
lm2 <- lm(danceability ~ loudness + tempo)
summary(lm2)
coefficients(lm2)
sqrt(sum(lm2$residuals^2)/lm2$df.residual)


##LMM random intercept-----------------------------------------------------------------------------
lmm <- lmer(danceability ~ loudness + tempo + (1|genre), data = data)
summary(lmm)

sigma2_eps <- as.numeric(get_variance_residual(lmm))
sigma2_eps
sigma2_b <- as.numeric(get_variance_random(lmm))
sigma2_b
PVRE <- sigma2_b/(sigma2_b+sigma2_eps)
PVRE 

dotplot(ranef(lmm, condVar=T))
# R&B is the one associated with the highest danceability
