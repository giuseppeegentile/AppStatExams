# Marco Scarpelli

# Exam 16/06/2023

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

# ATTENZIONE: questa libreria interferisce con biplot
# library(plot.matrix) 


rm(list=ls())
graphics.off()

df <- read.table('noise.txt', header=T)

head(df)

n<-dim(df)[1]
p<-dim(df)[2]

predicted_v <- df$noise # e.g. i km/litro per l'esempio dei benzinai

factor_1    <- factor(df$fuel) #1: e.g. il fattore "marca del distributore"
factor_2    <- factor(df$category)  #2: e.g. il fattore "tipo di benzina"
combined_factors<-(factor_1:factor_2)
comb_levels<-levels(combined_factors)
g1 <- length(levels(factor_1))
g2 <- length(levels(factor_2)) # lei la chiama b

##########################################
# Point A
M            <-  mean(predicted_v)
Mfactor1     <-  tapply(predicted_v, factor_1, mean)   #average considering only factor 1
Mfactor2     <-  tapply(predicted_v, factor_2, mean)   #average considering only factor 2
Mcomb_factors <- tapply(predicted_v, combined_factors, mean) #average considering combined factors

# Check assumptions for the complete model.
#   Normality in each group:
Ps<-NULL
for (i in 1:length(comb_levels))  #for each level of each possible combination of factors level
  Ps <- c(Ps, shapiro.test(predicted_v[ combined_factors==comb_levels[i] ])$p) 
data.frame(Ps) # OK

#   Same covariance structure
bartlett.test(predicted_v, combined_factors) # OK

fit.aov2.complete <- aov(predicted_v ~ factor_1 + factor_2 + factor_1:factor_2) #equivalent to (check) fit.aov2.complete <- aov(predicted_v ~ factor_1*factor_2)
summary.aov(fit.aov2.complete)

##########################################
# Point B

# It seems that factor 2 is not too significant;
#   first, we need to get rid of the interaction.

fit.aov2.additive <- aov(predicted_v ~ factor_1 + factor_2)
summary.aov(fit.aov2.additive)

# Let us get rid of factor 2.

fit.aov2.single <- aov(predicted_v ~ factor_1)
summary.aov(fit.aov2.single)

factor_v<- factor_1
g_rem<- g1
Ps <- NULL
for (i in 1:g_rem)  #for each treatment level
  Ps <- c(Ps, shapiro.test(predicted_v[ factor_v==levels(factor_v)[i] ])$p) 
Ps # 0.9144642 0.7071619 0.4181352

# Bartlett's test of homogeneity of variances
bartlett.test(predicted_v, factor_v) # 0.08121

##########################################
# Point C
i1 <- which(df$fuel==levels(factor_v)[1])

DF <- fit.aov2.single$df  #n-g  
SSres<-sum(fit.aov2.single$res^2)
Spooled <- SSres/DF
m  <- mean(df[,3])         # estimated mu
m1 <- mean(df[i1,3])    # estimated mu.1=mu+tau.1

tau.1<-m1-m

means<-rbind('mean'=m, 'tau.1'=tau.1)
means
Spooled

##########################################
# Point C
k <- g1*(g1-1)/2 #+1  if i also want the variance 
globalalpha <- .05
alpha= globalalpha/k

#Mean
Mediag  <- tapply(predicted_v, factor_v, mean)
Mediag

# S (Spooled)
Spooled <- sum(fit.aov2.single$res^2)/DF
S<-Spooled
S

levels_v <- levels(factor_1)
g <- g1
ng <- table(df$fuel)   

dof <- DF

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
#                         Inf        Sup
# diesel-ethanol    0.2536173  4.6572626
# diesel-gasoline  -4.0678733  0.3357721
# ethanol-gasoline -6.5233132 -2.1196679

# Seeing these confidence intervals, we could group
#   together diesel and gasoline and create a group
#   called DieselOrGasoline.

