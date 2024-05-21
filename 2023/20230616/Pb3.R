library(MASS)
library(car)
library(rgl)
library(glmnet)


library(nlmeU)
library(corrplot)
library(nlme)
library(lattice)
library(plot.matrix)
library(lme4)
library(insight)

options(rgl.printRglwidget = TRUE)

setwd("C:/Users/Giuseppe/Documents/.magistrale/secondo anno/AppStat/Exams/2023/20230616/20230616")
data <- read.table("Lakes_pollution.txt",header=T)
head(data)
dim(data)
n   <- dim(data)[[1]]
plot(data, pch=19)
# DO corr. with depth 


# Target variable
y   <- data$DO



dummy  = ifelse(data$wastewater_discharge == "No",0,1)

fm <- lm(y ~ depth + mercury_conc + ph + turbidity + wastewater_discharge,data = data )
summary(fm) 

# the model can be reduced by removing the mercury_conc
fm.red <- lm(y ~ depth + ph + turbidity + wastewater_discharge,data = data )
summary(fm.red) 


# Unknown
# betas
coefficients(fm.red)
# sigma
sqrt(sum(residuals(fm.red)^2)/fm.red$df)

par(mfrow=c(2,2))
plot(fm.red)
# yes, there is no pattern in the residuals so we can assume homoschedasticity

shapiro.test(residuals(fm.red))
# we have normality of residuals

# b)
# The increase is:
3*coefficients(fm.red)[4]

# Is it significant?
linearHypothesis(fm.red, rbind(c(0,0,0,1,0)), c(0))
#low pval -> yes is significant


help(lmer)



# c)
fm3 <- gls(y ~ depth + ph + turbidity + wastewater_discharge,
               correlation = corCompSymm(form = ~1|italian_lakes),    
               data =data)
summary(fm3)

# Estimated rho 0.03924728

# Confidence intervals for rho and sigma
intervals(fm3, which = "var-cov")
# rho's interval is containing 0
# we expect an almost diagonal matrix for marignal var-covar

fm12.1vcov <- getVarCov(fm3)  #estimate of R_i, e.g. i=2
par(mfrow=c(1,1))
print(fm12.1vcov)


# d)
lmm1 <- lmer(y ~ depth + ph + turbidity + wastewater_discharge + (1|italian_lakes), data = data)

sigma2_eps <- as.numeric(get_variance_residual(lmm1))
sigma2_b   <- as.numeric(get_variance_random(lmm1))
PVRE <- sigma2_b/(sigma2_b+sigma2_eps)
PVRE 

# 3.9% interpretation?

dotplot(ranef(lmm1, condVar=T))
# The one with lowest concentration of DO is lake levico

