# Marco Scarpelli

# Exam 04/09/2023

# Exercise 2

library(MASS)
library(car)
library(rgl)   #3D plots
library(leaps) #best subset selection
library(tree)  #decision trees
library(corrplot) #correlation
library(glmnet)
library(mvnormtest)
library(MVN)
library(heplots)
library(ggplot2)
library(mvtnorm)

rm(list=ls())
graphics.off()

df <- read.table('doping.txt', header=T)

head(df)

##########################################
# Point A

group<-factor(df$result)
class.A<- levels(group)[1] # name of class A
class.B<- levels(group)[2] # name of class B
iA <- which(df$result==levels(group)[1])  
iB <- which(df$result==levels(group)[2])

df<-df[,1:4] # keep the quantitative columns

head(df) # 'nameofgroupcolumn'
n<-dim(df)[1]
p<-dim(df)[2]

#df$pH <- df$pH + cbind(rnorm(n, sd=sapply(df$pH, mean)*0.01)) 
#df$creatinine   <- df$creatinine   + cbind(rnorm(n, sd=sapply(df$creatinine, mean)*0.01)) 
#df$rdensity <- df$rdensity + cbind(rnorm(n, sd=sapply(df$rdensity, mean)*0.01)) 
#df$turbidity <- df$pH + cbind(rnorm(n, sd=sapply(df$turbidity, mean)*0.01)) 

# Check multivariate normality within groups
p1<-mvn(df[iA,])$multivariateNormality # p=0.1810792 
p2<-mvn(df[iB,])$multivariateNormality # p=0.3010205
rbind(p1,p2)

SA<-cov(df[iA,])
SB<-cov(df[iB,])

round(diag(SA/SB), 3)
#     pH  creatinine   rdensity  turbidity 
# 2.415      0.607      2.914      1.620 

# We can use LDA, but it is a bit of a stretch.

##########################################
# Point B
pA<-.99
pB<-.01
priors<-c(pA,pB)

c.AB = 50000
c.BA = 1000
priors.corrected <- c( A = c.BA*pA  / (c.BA*pA+c.AB*pB),
                       B = c.AB*pB / (c.BA*pA+c.AB*pB))
priors.corrected # A=0.66, B=0.34 
df.da <- lda(df, group, prior=priors.corrected)
da.on.df  <- predict(df.da, df)
G <- 2 #Number of groups
daCV <- lda(df, group, prior = priors, CV=TRUE)  

miscCV <- table(class.true=group, class.assigned=daCV$class)
miscCV

AER_CV <- 0
G <- 2 #number of groups, change accordingly
for(g in 1:G)
  AER_CV <- AER_CV + (miscCV[g,-g]/sum(miscCV[g,])) * priors[g] 
AER_CV # 0.01. Everyone classified as clean

##########################################
# Point C

# In my case the model always predicts "clean", so we have
#   to budget 0 blood tests (0€)

##########################################
# Point D

# With the previous test, we spent 1000€ for each cyclist and, assuming
#   blood-based tests were 100% accurate, we caught all doped cyclists, so 
#   we had no loss and gained 1000€ per doped cyclist (1% of the population)
#   that was correctly identified.
# -> 1000€ * 200 - 0.01 * 200 * 1000€ = 198'000€
# With the new approach, we perform no tests but allow doped cyclists
#   (1% of the population) anyway and lose 50'000€ for each:
# -> 50'000€ * 0.01 * 200 = 100'000€