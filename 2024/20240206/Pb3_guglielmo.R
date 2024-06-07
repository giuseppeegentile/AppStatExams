setwd("C:/Users/Giuseppe/Documents/.magistrale/secondo anno/AppStat/Exams/2024/20240206/20240206")
data <- read.table("asthma.txt",header=T)

library(MASS)
library(car)
library(rgl)
library(glmnet)
library(insight)

library(nlmeU) ## --> for the dataset
library(nlme)  ## --> for models implementation

library(corrplot)
library(lattice)
library(plot.matrix)
library(lme4)


n   <- dim(data)[[1]]

# Target variable
y   <- data$asthma


fm <- lm(y ~ urban + age + pollution + sunny + income + education
         + tobacco # messo solo per avere stessi risultati di guglielo, ma non c'era nel pdf
         ,data=data)
summary(fm) 
sqrt(sum(residuals(fm)^2)/fm$df)  # s estimate of sigma
# 16.389
coefficients(fm)


# b_0,1
coefficients(fm)[1]+ coefficients(fm)[2]



par(mfrow=c(2,2))
plot(fm)

shapiro.test(residuals(fm))


# b
{
  alpha = 0.1
  confint(fm, level= 1-alpha)[3,] 
  
  # yes, we can [0.2554552, 5.5143070]
}

# b
{
  alpha = 0.05
  confint(fm, level= 1-alpha)[2,] 
  
  # yes, we can [1.892556, 14.611153 ]
}

# c
fm <- lm(y ~ urban + age + pollution + sunny + income + education
         + tobacco
         ,data=data)
summary(fm) 

fm1 <- lm(y ~ urban + age + pollution + sunny + income
         + tobacco
         ,data=data)
summary(fm1) 

fm2 <- lm(y ~ urban + pollution + sunny + income
          + tobacco
          ,data=data)
summary(fm2) 

linearHypothesis(fm, rbind(c(0,0,1,0,0,0,0,0), 
                           c(0,0,0,0,0,0,1,0)), c(0,0))
# we can remove those two: age and education


fm12.1 <- gls(y ~ urban + pollution + sunny + income + tobacco, 
              correlation = corCompSymm(form = ~1|region_id),
              data = data)
summary(fm12.1)

# Confidence intervals for rho and sigma
{
  significance = 0.99
  intervals(fm12.1, which = "var-cov",level=significance)
  
  #[0.911, 0.985]
  #[11.877, 26.738]
}

lmm1 = lmer(y ~ urban + pollution + sunny + income + tobacco + (1|region_id),  
            data = data)
summary(lmm1)
# notice that has the same coefficient of M1, same AIC too
AIC(lmm1,fm12.1)
BIC(lmm1,fm12.1) #we may prefer the corr comp sym model
anova(lmm1,fm12.1)

VarCorr(lmm1)




