# Marco Scarpelli

# Exam 2022/09/09

# Exercise 1

library(MASS)
library(car)
library(rgl)
library(leaps)
library(tree)
library(corrplot)
library(glmnet)
library(mvnormtest)
library(MVN)
library(heplots)
library(ggplot2)
library(mvtnorm)
library(nlme)
library(lme4)
library(insight)
library(nlmeU)
library(lattice)
library(class)
library(dbscan)
library(cluster)

# ATTENZIONE: questa libreria interferisce con biplot
# library(plot.matrix) 


rm(list=ls())
graphics.off()

df <- read.table('dinosaurs.txt', header=T)

predicted_v <- df$length # e.g. i km/litro per l'esempio dei benzinai

factor.name.1 = "Diet"
factor.labels.1 = NULL
for (i in 1 : length(levels(factor(df$diet)))) {
     factor.labels.1 = rbind(factor.labels.1, 
                             paste(factor.name.1, 
                                   levels(factor(df$diet))[i],
                                   sep="-"))
}
factor.name.2 = "Period"
factor.labels.2 = NULL
for (i in 1 : length(levels(factor(df$period)))) {
  factor.labels.2 = rbind(factor.labels.2, 
                          paste(factor.name.2, 
                                levels(factor(df$period))[i],
                                sep="-"))
}
     
factor_1   <- factor(df$diet, labels=factor.labels.1) 
factor_2   <- factor(df$period, labels=factor.labels.2) 

combined_factors<-factor_1:factor_2

head(df)
g1 <- length(levels(factor_1))
g1
g2 <- length(levels(factor_2)) # lei la chiama b
g2

M            <-  mean(predicted_v)
Mfactor1     <-  tapply(predicted_v, factor_1, mean)   #average considering only factor 1
Mfactor2     <-  tapply(predicted_v, factor_2, mean)   #average considering only factor 2
Mcomb_factors <- tapply(predicted_v, combined_factors, mean) #average considering combined factors
comb_levels<-levels(combined_factors)

M
Mfactor1
Mfactor2
Mcomb_factors

##########################################
# Point A

#  1. Data of different levels each possible combination of factors are 
#     univariate gaussian
Ps<-NULL
for (i in 1:length(comb_levels))  #for each level of each possible combination of factors level
  Ps <- c(Ps, shapiro.test(predicted_v[ combined_factors==comb_levels[i] ])$p) 
data.frame(Ps)

# La nostra soglia globale di significatività va divisa per il numero di test fatti
global_alpha <- 0.05 / length(comb_levels)
global_alpha
Ps
# 6.567342e-04 1.294190e-01 5.599272e-10 4.213367e-04 5.620537e-03 8.546472e-01
Ps > global_alpha # FALSE  TRUE FALSE FALSE FALSE  TRUE

# Low p-values! Let us check how the data is distributed in general.
hist(predicted_v)

# And also within each group:
par(mfrow=c(1,length(comb_levels)))
for (i in 1:length(comb_levels))  #for each level of each possible combination of factors level
  hist(predicted_v[ combined_factors==comb_levels[i] ])
par(mfrow=c(1,1))

# Running a Box-Cox transformation:
lambda.pred <- powerTransform(predicted_v) 
lambda.pred$lambda[1]
bc.predicted_v <- bcPower(predicted_v, lambda.pred$lambda[1]) 

hist(bc.predicted_v)

predicted_v <- bc.predicted_v

par(mfrow=c(1,length(comb_levels)))
for (i in 1:length(comb_levels))  #for each level of each possible combination of factors level
  hist(predicted_v[ combined_factors==comb_levels[i] ])
par(mfrow=c(1,1))

Ps<-NULL
for (i in 1:length(comb_levels))  #for each level of each possible combination of factors level
  Ps <- c(Ps, shapiro.test(predicted_v[ combined_factors==comb_levels[i] ])$p) 
data.frame(Ps)
Ps > global_alpha # TRUE TRUE TRUE TRUE TRUE TRUE
# The data is now sufficiently Gaussian.

#  2. Data of different levels each possible combination of factors 
#     have the same covariance structure
# The variance ratio should not be greater than 4.
var_combined_levels<-NULL
for (i in 1:length(comb_levels))  #for each treatment level
  var_combined_levels <- c(var_combined_levels, var(predicted_v[ combined_factors==comb_levels[i] ]) )
data.frame(var_combined_levels)
#   var_combined_levels
# 1           1.3943673
# 2           1.2958086
# 3           1.3418038
# 4           1.7228353
# 5           0.6172420
# 6           0.7957951

# Also run bartlett's test. This is very conservative, so we are
#   happy if this succeeds.
bartlett.test(predicted_v, combined_factors) # p = 0.2831, OK.

fit.aov2.complete <- aov(predicted_v ~ factor_1 + factor_2 + factor_1:factor_2) #equivalent to (check) fit.aov2.complete <- aov(predicted_v ~ factor_1*factor_2)
summary.aov(fit.aov2.complete)
#                    Df Sum Sq Mean Sq F value   Pr(>F)    
# factor_1            2   46.0  22.979  16.819 1.32e-07 ***
# factor_2            1    2.5   2.460   1.800    0.181    
# factor_1:factor_2   2    0.7   0.374   0.274    0.761    
# Residuals         268  366.2   1.366                     

##########################################
# Point B

# First, we will remove the interaction.
fit.aov2.additive <- aov(predicted_v ~ factor_1 + factor_2)
summary.aov(fit.aov2.additive)
#              Df Sum Sq Mean Sq F value   Pr(>F)    
# factor_1      2   46.0  22.979   16.91 1.21e-07 ***
# factor_2      1    2.5   2.460    1.81     0.18    
# Residuals   270  366.9   1.359                     


# The second factor ("period") is still not significant.

fit.aov1 <- aov(predicted_v ~ factor_1)
summary.aov(fit.aov1)
#              Df Sum Sq Mean Sq F value   Pr(>F)    
# factor_1      2   46.0  22.979   16.86 1.26e-07 ***
# Residuals   271  369.4   1.363   

##########################################
# Point C
tau<-Mfactor1-M
tau
# Diet-carnivorous Diet-herbivorous  Diet-omnivorous 
#            -2.02             1.60            -4.08 

##########################################
# Point D
k <- 3
alpha<-.05
factor_v<-factor_1
factor_v
ng <- table(factor_v)  #levels of remained factor
ng
Mediag  <- tapply(predicted_v, factor_v, mean)
Mediag
n <- dim(df)[1]
n
g_rem<- g1
  
dof <- fit.aov1$df
# Check: n-g_rem == dof
dof 
g_rem
n-g_rem

Spooled <- sum(fit.aov1$residuals^2)/dof
Spooled

# One-way ANOVA ONLY!
#   Pretty-print
ICrange=NULL
rowNamesVec=NULL
for(i in 1:(g_rem-1)) {
  rowname.first=levels(factor_v)[i]
  for(j in (i+1):g_rem) {
    ICrange=rbind(ICrange, as.numeric(c(Mediag[i]-Mediag[j] - qt(1-alpha/(2*k), dof) * sqrt( Spooled * ( 1/ng[i] + 1/ng[j] )),
                                        Mediag[i]-Mediag[j] + qt(1-alpha/(2*k), dof) * sqrt( Spooled * ( 1/ng[i] + 1/ng[j] )))))
    
    rowNamesVec = rbind(rowNamesVec, paste(rowname.first, levels(factor_v)[j], sep="-"))
    
  }
}
colnames(ICrange)<-c('Inf','Sup')
rownames(ICrange)<-rowNamesVec
ICrange
#                                          Inf        Sup
# Diet-carnivorous-Diet-herbivorous -1.0811380 -0.3309427
# Diet-carnivorous-Diet-omnivorous  -0.2053354  1.1166891
# Diet-herbivorous-Diet-omnivorous   0.5359916  1.7874428


# It seems (with 95% confidence) that there is no difference in the means
#   of the lengths of carnivorous and omnivorous dinosaurs, while we can see that
#   on average, herbivorous dinosaurs are bigger of both omnivorous and 
#   carnivorous dinosaurs.
