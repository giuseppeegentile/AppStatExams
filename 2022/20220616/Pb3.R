setwd("C:/Users/Giuseppe/Documents/.magistrale/secondo anno/AppStat/Exams/2022/20220616/20220616")
data <- read.table("danceability.txt",header=T)
head(data)

library(nlmeU) 
library(nlme) 

library(corrplot)
library(lattice)
library(insight)
library(plot.matrix)
library(MASS)
library(car)
library(rgl)
library(glmnet)
head(data)

# Correlation between energy and loudness
plot(data, pch=19)

n   <- dim(data)[[1]]


# Target variable
y   <- data$danceability 

fm <- lm(y ~ loudness + energy + tempo ,data=data)
summary(fm) 

# Residual standard error: (estimated variance of residual )^/1/2
sqrt(sum(residuals(fm)^2)/fm$df)  # s estimate of sigma
coefficients(fm)

## Model diagnostic, Verify assumptions
{
  par(mfrow=c(2,2))
  plot(fm)
  
  
  shapiro.test(residuals(fm))
  par(mfrow=c(1,1))
  
}

linearHypothesis(fm, rbind(c(0,1,0,0), c(0,0,1,0)), c(0,0))
#no, we can't remove both of them simultaneously


# We can remove energy one at a time
fm <- lm(y ~ loudness  + tempo ,data=data)
summary(fm) 


## Model diagnostic, Verify assumptions
{
  par(mfrow=c(2,2))
  plot(fm)
  
  
  shapiro.test(residuals(fm))
  par(mfrow=c(1,1))
  
}# ok again
# Residual standard error: (estimated variance of residual )^/1/2
sqrt(sum(residuals(fm)^2)/fm$df)  # s estimate of sigma
coefficients(fm)



# d)
lmm1 = lmer(y ~ loudness  + tempo +  (1|genre),  data = data)
summary(lmm1)

# PVRE
{
  # Also called the intraclass correlation (ICC), since is also an estimate of the within 
  # cluster correlation.
  sigma2_eps <- as.numeric(get_variance_residual(lmm1))
  sigma2_eps
  sigma2_b <- as.numeric(get_variance_random(lmm1))
  sigma2_b
  
  PVRE <- sigma2_b/(sigma2_b+sigma2_eps)
  PVRE
}
# low (at least not very high) each genre is not explaining much in terms of danceability
# variability within genre
#           is much greater than the variability between genres
{
  # effect of the energy on the danceability
  dotplot(ranef(lmm1))
  
  # Random intercepts and fixed slopes: (beta_0+b_0i, beta_1, beta_2)
  coef(lmm1)
  head(coef(lmm1)$genre)
} # we can see what we sayed before: most of the intervals accept the 0
# the random contribution to danceability by genre is low

# R&B


  