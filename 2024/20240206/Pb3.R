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
pairs(data[,-7],pch=19)
# Target variable
y   <- data$asthma


fm <- lm(y ~ urban + age + pollution+ sunny + income + education,data=data)
summary(fm) 
# we have very low fitted value, we're missing something for sure

sqrt(sum(residuals(fm)^2)/fm$df)  # s estimate of sigma
coefficients(fm)

## Model diagnostic, Verify assumptions
{
  par(mfrow=c(2,2))
  plot(fm)
  # we have a pattern on the residual: the more the fitted value increase
  #   the more the residual decrease
  
  
  shapiro.test(residuals(fm))
  # normality ok
}

# Confidence interval for beta_age
alpha = 0.1
confint(fm, level= 1-alpha)[3,]
# No we can't affirm it

#confidence interval for the mean di↵erence between the asthma prevalence in 
# an urban province and in a non-urban one
{
  alpha = 0.05
  confint(fm, level= 1-alpha)[2,]
} 




# c)
# reducing
# remove sunny
fm <- lm(y ~ urban + age + pollution + income + education,data=data)
summary(fm) 
# remove urban
fm <- lm(y ~ age + pollution + income + education,data=data)
summary(fm) 
# remove age
fm <- lm(y ~ pollution + income + education,data=data)
summary(fm) 

# remove education
fm <- lm(y ~ pollution + income,data=data)
summary(fm) 

fm12.1 <- gls(y ~ pollution + income, 
              correlation = corCompSymm(form = ~1|region_id),
              data = data)
summary(fm12.1)

# Confidence intervals for rho and sigma
{
  intervals(fm12.1, which = "var-cov",level=0.99)
  # -> There is correlation among the residuals within regions
  # -> the repeated measures within the same regions  are correlated
}
anova(fm12.1, fm)
AIC(fm12.1, fm)



# d)
lmm1 = lmer(y ~ pollution + income + (1|region_id),  
            data = data)
summary(lmm1)

# We have negative correlation between asthma and pollution 
# and between asthma and income
confint(lmm1, oldNames=TRUE)


dotplot(ranef(lmm1))


VarCorr(lmm1)


