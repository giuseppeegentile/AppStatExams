rm(list=ls())
#Lab of reference: Lab 7
#_______________________________________________________________________________
rm(list=ls())
library(MVN)
library(car)
library(heplots)
data<-read.table("dinosaurs.txt",header=T)

head(data)
dim(data)

n<-dim(data)[1]
n
p<-dim(data)[2]
p

head(data)

predicted_v<-data$length
predicted_v

head(data)

factor_1    <- factor(data$diet) #1: e.g. il fattore "marca del distributore"
factor_1

factor_2    <- factor(data$period)  #2: e.g. il fattore "tipo di benzina"
factor_2

combined_factors<-(factor_1:factor_2)
combined_factors

g1 <- length(levels(factor_1))
g1
g2 <- length(levels(factor_2)) # lei la chiama b
g2

data



head(data)


### jitter data$length
data$length <- jitter(data$length)

lambda.x <- powerTransform(data$length)
data$length <- bcPower(data$length, 0.1822562)
predicted_v <- data$length
head(predicted_v)
data$combined_factors <- combined_factors




head(data)

i1 <- which(data$diet=="herbivorous"&data$period=="Jurassic")
i2 <- which(data$diet=="herbivorous"&data$period=="Cretaceous")
i3 <- which(data$diet=="carnivorous"&data$period=="Jurassic")
i4 <- which(data$diet=="carnivorous"&data$period=="Cretaceous")
i5 <- which(data$diet=="omnivorous"&data$period=="Jurassic")
i6 <- which(data$diet=="omnivorous"&data$period=="Cretaceous")

#create a vector when I do have 1 for position specified in i1, 2 for position specified in i2, 3 for i3 and so on

bartlett.test(data$length~factor_1)
bartlett.test(data$length~factor_2)
bartlett.test(data$length~combined_factors)
combined_factors



Ps <-c(shapiro.test(predicted_v[i1])$p,
       shapiro.test(predicted_v[i2])$p,
       shapiro.test(predicted_v[i3])$p,
       shapiro.test(predicted_v[i4])$p, 
       shapiro.test(predicted_v[i5])$p,
       shapiro.test(predicted_v[i6])$p
       )

Ps


fit.aov2.complete <- aov(predicted_v ~ factor_1 + factor_2 + factor_1:factor_2) #equivalent to (check) fit.aov2.complete <- aov(predicted_v ~ factor_1*factor_2)
summary.aov(fit.aov2.complete)

fit.aov2.additive <-aov(predicted_v ~ factor_1 + factor_2)
summary(fit.aov2.additive)

fit.aov2.only1 <-aov(predicted_v ~ factor_1)
summary(fit.aov2.only1)


levels(factor_1)





######



k <- 3
alpha<- 0.05

ng <- table(factor_1)  #levels of remained factor
ng
factor_v = factor_1
Mediag  <- tapply(predicted_v, factor_1, mean)
Mediag
N <- dim(data)[1]
N

dof <- fit.aov2.only1$df
dof #N-g_rem
g_rem<-N-dof
N-g_rem
g_rem

Spooled <- sum(fit.aov2.only1$residuals^2)/dof
Spooled

ICrange=NULL
for(i in 1:(g_rem-1)) {
  for(j in (i+1):g_rem) {
    print(paste(levels(factor_v)[i],"-",levels(factor_v)[j]))        
    print(as.numeric(c(Mediag[i]-Mediag[j] - qt(1-alpha/(2*k), dof) * sqrt( Spooled * ( 1/ng[i] + 1/ng[j] )),
                       Mediag[i]-Mediag[j] + qt(1-alpha/(2*k), dof) * sqrt( Spooled * ( 1/ng[i] + 1/ng[j] )))))
    ICrange=rbind(ICrange,as.numeric(c(Mediag[i]-Mediag[j] - qt(1-alpha/(2*k), dof) * sqrt( Spooled * ( 1/ng[i] + 1/ng[j] )),
                                       Mediag[i]-Mediag[j] + qt(1-alpha/(2*k), dof) * sqrt( Spooled * ( 1/ng[i] + 1/ng[j] )))))
  }
}
colnames(ICrange)<-c('Inf','Sup')

ICrange
#Transform the IC range back inverse of Box-Cox transformation

###### Esercizio 2
graphics.off()
rm(list=ls())
library(MASS)      #for LDA
library(rgl) 
library(MVN)#for 3D visualization
library(mvtnorm)   #for multivariate normality
df <- read.table("fossil.txt", header=T)
head(df)
group<-factor(df$type)
df<-df[,1:2] #keep the quantitative columns
head(df) # 'nameofgroupcolumn'

n<-dim(df)[1]
n
p<-dim(df)[2]
p



#SELECTING A AND B
class.A<- 'nerinea' # name of class A
class.B<- 'perisphinctes' # name of class B


iA <- which(group==class.A)  
iB <- which(group==class.B)  

col.lab<-rep(0,n)
col.lab[iA]<-'red'
col.lab[iB]<-'blue'



plot(df, col=col.lab,main='', xlab='', ylab='', pch=19) 
# plot if p=1 or 2, pairs otherwise
legend("topright", legend=levels(group), fill=c('red','blue'), lty=c(1,1)) #only in plot

# MANOVA: controlla quanto è significativa la differenza tra i gruppi. Assieme al plot ci fa capire se i dati sono classificabili.
fit<-manova(as.matrix(df)~group) #if univariate anov<-aov((df~group)) summary(anov)
# fit <- manova(as.matrix(iris2) ~ species.name)
summary(fit, test='Wilks')
mvn(df[iA,])
mvn(df[iB,])


SA<-cov(df[iA,])
SB<-cov(df[iB,])
SA
SB
bartlett.test(df[iA,],df[iB,])

#Visual check
par(mfrow=c(1,2))
image(SA, col=heat.colors(100),main='Cov. SA', asp=1, axes = FALSE, breaks = quantile(rbind(SA,SB), (0:100)/100, na.rm=TRUE))
image(SB, col=heat.colors(100),main='Cov. SB', asp=1, axes = FALSE, breaks = quantile(rbind(SA,SB), (0:100)/100, na.rm=TRUE))


nA <- length(iA)
nB <- length(iB)
n  <- nA + nB
PA <- nA / n
PB <- nB / n

#ESTIMATING priors from the data
pA<-length(iA)/(length(iA)+length(iB)) 
pB<-1-pA
priors <- c(pA,pB)

df.lda <- lda(df, group, prior=priors)#.corrected if I have misclass cost)  #also lda (group~df, prior=priors)#.corrected) 
df.lda




Lda.on.df <- predict(df.lda, df) #I use my classifier to predict the training set

#Per APER vanno considerati i costi o no? direi di NO, quindi usare priors normali, NON aggiustati per i costi!
errors<-sum(Lda.on.df$class!=group)
# per esempio sum(Lda.iris$class != species.name)
APER<-errors/length(group)
APER


ldaCV<- lda(df, group, CV=TRUE) #priors estimated from dataset 

errorsqCV <- (ldaCV$class != group)
errorsqCV
AERlCV   <- sum(errorsqCV)/length(group)
AERlCV

#check: sum of first diagonal / total sum
miscCV <- table(classe.vera=group, classe.allocata=ldaCV$class)
miscCV


x  <- seq(min(df[,1]), max(df[,1]), length=200)
y  <- seq(min(df[,2]), max(df[,2]), length=200)
xy <- expand.grid(nomevarX=x, nomevarY=y) #change accordingly
names(df)

z  <- predict(df.lda, xy)$post  # P_i*f_i(x,y)  
# Uno z per ogni gruppo
z1 <- z[,1] - pmax(z[,2]) #, z[,3])  # P_1*f_1(x,y)-max{P_j*f_j(x,y)} #pmax Returns the (regular or parallel) maxima and minima of the input values.
z2 <- z[,2] - pmax(z[,1]) #, z[,3])  # P_2*f_2(x,y)-max{P_j*f_j(x,y)}  
#z3 <- z[,3] - pmax(z[,1], z[,2])    # P_3*f_3(x,y)-max{P_j*f_j(x,y)}

# Plot the contour line of level (levels=0) of z1, z2, z3: 
# P_i*f_i(x,y)-max{P_j*f_j(x,y)}=0 i.e., boundary between R.i and R.j where j realizes the max.

plot(df, col=col.lab,main='', xlab='', ylab='', pch=19) # plot if p=1 or 2, pairs otherwise
# Un contour per ogni gruppo
# Plot the contour line of level (levels=0) of z1, z2, z3: 
#   P_i*f_i(x,y)-max{P_j*f_j(x,y)}=0 i.e., boundary between R.i and R.j 
#   where j realizes the max.
contour(x, y, matrix(z1, 200), levels=0, drawlabels=F, add=T)  
contour(x, y, matrix(z2, 200), levels=0, drawlabels=F, add=T)
legend("topright", legend=levels(group), fill=c('red','blue'), lty=c(1,1)) #only in plot


###KNN



#KNN on all df, given k

k<-myk
data.knn <- knn(train = df, test = df, cl = group, k = myk)

#Controllare, fare doppio check con esercizio su occupancy
misc<-table(data.knn, group)

errors<-data.knn!=group 


#AERknn: no priors
APERknn<-sum(errors)/n
APERknn

#plot the model #ricontrollare
quartz()
plot(df[,1], df[,2], col=1+(group == "0")+(data.knn!=group)) 


#-----------------------------------------------------------------------
# Cross validation - Model selection and AERkCV
#---------------------------------------------------------------------
library(class)     #for KNN

n_obs<- dim(df)[1] #n
k_range<- 5:20 #range of k to test, estremi inclusi

Aerr<-NULL
set.seed(9)  #run seed every time!
for (k in k_range) {
  df.knn <- knn.cv(train = df, cl = group, k = k)
  errorsqCV <- (df.knn != group)
  Aerr   <- c(Aerr,(sum(errorsqCV))/n_obs) #controllare che stiamo parlando di n_obs
}
min(Aerr)
which(Aerr==min(Aerr)) 


#best k
best.index=which.min(Aerr)
kbest<-k_range[best.index]
kbest


#AERkCV
# no priors!
AERkCV<-min(Aerr)
AERkCV


#Best model trained on best k
best <- knn.cv(train = df, cl = group, k = best_k)

#--------------------------------------------------------------------------------------
#Aper - controllare
#--------------------------------------------------------------------------------------
#given a K
k_given<-
  model <- knn.cv(train = df, cl = group, k = k_given)
APER.knn<-sum(model!=group)/length(group)
APER.knn




x  <- seq(min(df[,1]), max(df[,1]), length=200)
y  <- seq(min(df[,2]), max(df[,2]), length=200)
xy <- expand.grid(lon=x, lat=y) #change accordingly
names(df)

selected_k<-kbest  # best_k or given_k
  data.knn <- knn(train = df, test = xy, cl = group, k = selected_k)
z  <- as.numeric(data.knn)

quartz()
plot(df, main='', xlab='', ylab='', pch=20)
points(df[iA,], col='red', pch=19)
points(df[iB,], col='blue', pch=19)
legend("topright", legend=c(levels(group)), fill=c('red','blue'))
contour(x, y, matrix(z, 200), levels=c(1.5, 2.5), drawlabels=F, add=T) #change if more than 2 levels
#--------------------------------------------------------------------------------------
###EX 3

library(MASS)
library(car)
library(rgl)   #3D plots
library(leaps) #best subset selection
library(tree)  #decision trees
library(corrplot) #correlation
library(glmnet)
rm(list=ls())
graphics.off()
df<-read.table("cameo.txt",header=T)
head(df)
df$dw<-df$dimension * df$weight
head(df)
#create dummy variable for processing
df$dummy<-ifelse(df$process=='ultrasonic',1,0)
head(df)
fit <- lm(price ~ dimension + weight + dw + dummy + dummy:weight+dummy:dimension+ dummy:dw, data=df)
summary(fit)
vif(fit)
help(vif)


par(mfrow=c(2,2))
plot(fit)
shapiro.test(residuals(fit))
par(mfrow=c(1,1))
fit1 <- lm(price ~ dimension + weight + dw, data=df)
summary(fit1)
anova(fit1,fit)

fit2 <- lm(price ~ dimension + weight + dw + dummy + dummy:weight+dummy:dimension, data=df)
summary(fit2)
fit3 <- lm(price ~ dimension + weight +dummy, data=df)
summary(fit3)
anova(fit3,fit)
vif(fit3)


par(mfrow=c(2,2))
plot(fit3)
shapiro.test(residuals(fit3))
par(mfrow=c(1,1))

#Compute the pointwise estimate for the price of an handmade cameo with d = 10cm and w = 80g. Is the
#prediction reliable?
newdata<-data.frame(dimension=10, weight=80, dummy=0)
predict(fit3,newdata)
summary(fit3)
Pred <- predict(fit3, newdata, 
                interval='prediction', level=1-0.05)  
t(Pred) # fit i


summary(fit3)
