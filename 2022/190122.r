#############EX1
#############EX1
rm(list = ls())
library(car)
library(mvtnorm) 
library(mvnormtest)
library(MVN)
da <- read.table('acoruna.txt', header=T)

dp <- read.table('pontevedra.txt', header=T)


# The distribution of two different tapas are independent -> two differernt gaussian population!
# I can't do the dataset of differences

t1 <- da
t2 <- dp

head(t1[,1:2])
mvn(t1)
mvn(t2)
d<-c(1,0)
d[0]
df1 = t1
df2 = t2

n1 <- dim(df1)[1] # n1
n2 <- dim(df2)[1] # n2
p  <- dim(df1)[2] # p=2 (same for t1 and t2)
n1
n2
p

#We compute the sample mean, covariance matrices and the matrix Spooled
# Matrici di covarianza
df1.mean <- sapply(df1,mean)
df1.mean
df2.mean <- sapply(df2,mean)
df2.mean
df1.cov  <-  cov(df1)
df2.cov  <-  cov(df2)
Sp      <- ((n1-1)*df1.cov + (n2-1)*df2.cov)/(n1+n2-2)
Spinv    <-solve(Sp)
list(S1=df1.cov, S2=df2.cov, Spooled=Sp)


list(S1 = df1.cov, S2 = df2.cov, Spooled = Sp)


# SE ABBIAMO ABBASTANZA DATI

# Matrici di covarianza calcolate sopra
df1.cov
df2.cov

#Visual check
par(mfrow=c(1,2))
#add df3.cov if any!
myquantile<-quantile(rbind(df1.cov,df2.cov), (0:100)/100, na.rm=TRUE)
image(df1.cov, col=heat.colors(100),main='Cov. df1', asp=1, axes = FALSE, 
      breaks = myquantile)
image(df2.cov, col=heat.colors(100),main='Cov. df2', asp=1, axes = FALSE, 
      breaks = myquantile)



delta<-c(0,0) #vector in R^p - change accordingly
alpha<-0.01
cfr.fisher <- (p*(n1+n2-2)/(n1+n2-1-p))*qf(1-alpha,p,n1+n2-1-p)

#Statistics
T2 <- n1*n2/(n1+n2) * (df1.mean-df2.mean-delta) %*% Spinv %*% (df1.mean-df2.mean-delta)
T2 < cfr.fisher # TRUE: no statistical evidence to reject H0 at confidence level 1-alpha%

#P value
P <- 1 - pf(T2/(p*(n1+n2-2)/(n1+n2-1-p)), p, n1+n2-1-p)
P  



k <- 2 
alpha<-0.01
cfr.t <- qt(1-alpha/(2*k),n1+n2-2)

Bf <- cbind(inf = df1.mean - df2.mean - cfr.t*sqrt(diag(Sp)*(1/n1+1/n2)),
            center = df1.mean - df2.mean,
            sup = df1.mean - df2.mean + cfr.t*sqrt(diag(Sp)*(1/n1+1/n2)))
Bf


p1 = 0.5*(t1[,1]+t1[,2])
head(p1)
p2 = 0.5*(t2[,1]+t2[,2])
head(p2)

#create a dataset merging p1 and p2 d labeling the two groups
df = data.frame(p = c(p1,p2), group = factor(rep(1:2, c(n1,n2))))
#bartlett test on df
head(df)
bartlett.test(p~group, data = df)

t.test(p1,p2, var.equal = TRUE, alternative = "greater", conf.level = 0.99)



#####EX 2
graphics.off()
rm(list = ls())
library(MVN)
df <- read.table('fish.txt', header=T)
head(df)
df
head(df)



group<-factor(df$abundance) #df$nameofgroupcolumn
df<-df[,1:2] #keep the quantitative columns
head(df) # 'nameofgroupcolumn'
n<-dim(df)[1]
n
p<-dim(df)[2]
p

# Mettere dimensioni corrette?


#SELECTING A AND B
class.A<- 'H' # name of class A
class.B<- 'L' # name of class B


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


mA<-colMeans(df[iA,])
mB<-colMeans(df[iB,])
m = colMeans(df)
m1 = mA
m2 = mB

#check


SA<-cov(df[iA,])
SB<-cov(df[iB,])
n1 <-length(iA)
n2 <-length(iB)

g <-2
p <-2
s <- min(g-1,p)



#Print
#Cluster means
rbind(mA,mB)
#Cluster covariances
SA
SB
S1 <-SA
S2 <-SB
#Priors
Sp  <- ((n1-1)*S1+(n2-1)*S2)/(n-g)




# Compute amount of coordinates for the canonical directions


# Matrix Spooled^(-1/2), used for reparametrisation. Note: \ref{def:FisherClassifier}
val.Sp <- eigen(Sp)$val
vec.Sp <- eigen(Sp)$vec

invSp.2 <- 1/sqrt(val.Sp[1])*vec.Sp[,1]%*%t(vec.Sp[,1]) + 1/sqrt(val.Sp[2])*vec.Sp[,2]%*%t(vec.Sp[,2])
invSp.2


B <- 1/g*(cbind(m1 - m) %*% rbind(m1 - m) +
            cbind(m2 - m) %*% rbind(m2 - m))
B

# spectral decomposition of Sp^(-1/2) B Sp^(-1/2)
spec.dec <- eigen(invSp.2 %*% B %*% invSp.2)

# first canonical coordinate
a1 <- invSp.2 %*% spec.dec$vec[,1]
a1

# second canonical coordinate
a2 <- invSp.2 %*% spec.dec$vec[,2]
a2





cc1.data <- as.matrix(df)%*%a1
cc2.data <- as.matrix(df)%*%a2
coord.cc <- cbind(cc1.data,cc2.data)

# Compute the coordinates of the mean within groups along the canonical directions
cc.m1 <- c(m1%*%a1, m1%*%a2)
cc.m2 <- c(m2%*%a1, m2%*%a2)

# Assign data to groups
f.class=rep(0, n)

for(i in 1:n) # for each datum
{
  # Compute the Euclidean distance of the i-th datum from mean within the groups
  dist.m=c(d1=sqrt(sum((coord.cc[i,]-cc.m1)^2)),
           d2=sqrt(sum((coord.cc[i,]-cc.m2)^2)))
  # Assign the datum to the group whose mean is the nearest
  f.class[i]=which.min(dist.m)
}

table(class.true=group, class.assigned=f.class)

errors <- n - sum(diag(table(class.true=group, class.assigned=f.class)))
errors
length(groups.name)

APERf   <- errors/length(group)
APERf


####Never Performed CV with Fisher, so I guees he wanted LDA instaed wchis is robust to 
## NOn Gaussian data
library(MASS)      #for LDA
library(rgl)       #for 3D visualization
library(mvtnorm)

priors <-c(12/25,13/25)
priors

#!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
####Farei QDA perchè non ho normalità!!
df.lda <- lda(df, group, prior=priors)#.corrected if I have misclass cost)  #also lda (group~df, prior=priors)#.corrected) 

df.lda

Lda.on.of  <- predict(df.lda, df)
head(df)

x  <- seq(min(df[,1]), max(df[,1]), length=200)
y  <- seq(min(df[,2]), max(df[,2]), length=200)
xy <- expand.grid(x=x, y=y) #change accordingly
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


ldaCV<- lda(df, group, CV=TRUE) #priors estimated from dataset 

errorsqCV <- (ldaCV$class != group)
errorsqCV
AERlCV   <- sum(errorsqCV)/length(group)
AERlCV

#check: sum of first diagonal / total sum
miscCV <- table(classe.vera=group, classe.allocata=ldaCV$class)
miscCV


###############
library(class)     #for KNN


#KNN on all df, given k

myk<-10
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
k_range<- 10:30 #range of k to test, estremi inclusi

Aerr<-NULL
set.seed(19)  #run seed every time!
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
best <- knn.cv(train = df, cl = group, k = kbest)

#--------------------------------------------------------------------------------------
#Aper - controllare
#--------------------------------------------------------------------------------------
#given a K
k_given<-
  model <- knn.cv(train = df, cl = group, k = k_given)
APER.knn<-sum(model!=group)/length(group)
APER.knn

#--------------------------------------------------------------------------------------
#Plot of the classification regions
#--------------------------------------------------------------------------------------
#KNN on all grid for decision regions
x  <- seq(min(df[,1]), max(df[,1]), length=200)
y  <- seq(min(df[,2]), max(df[,2]), length=200)
xy <- expand.grid(nameofX=x, nameofX=y) #change accordingly
names(df)

selected_k<-  # best_k or given_k
  data.knn <- knn(train = df, test = xy, cl = group, k = 13)
z  <- as.numeric(data.knn)

quartz()
plot(df, main='', xlab='', ylab='', pch=20)
points(df[iA,], col='red', pch=19)
points(df[iB,], col='blue', pch=19)
legend("topright", legend=c(levels(group)), fill=c('red','blue'))
contour(x, y, matrix(z, 200), levels=c(1.5, 2.5), drawlabels=F, add=T) #change if more than 2 levels


#-----------------------------------------------------------------------
#Prediction with KNN
#------------------------------------------------------------------------
new <- data.frame(nameofvarx = 10.8, nameofvary= 39.4)    #change accordingly
knn(train = df, test = new, cl = group, k=13)   


######### Exercise 3
##################################
rm(list = ls())
graphics.off()
library(MASS)
library(car)
library(rgl)   #3D plots
library(leaps) #best subset selection
library(tree)  #decision trees
library(corrplot) #correlation
library(glmnet)
df <- read.table('tattoo.txt', header=T)
head(df)
df$dummyh<-ifelse(df$method=='handmade',1,0)

head(df)
fit1 <- lm(price ~ dummyh +dimension+ncolors+ dummyh:dimension+dummyh:ncolors, data = df)
summary(fit1)



fit11 <-lm(price ~ dimension + ncolors+method+ method:ncolors+method:dimension, data = df)


fit2 <-lm (price ~ dimension+ ncolors, data = df)
summary(fit2)

anova(fit1,fit2)

fit3 <- lm(price ~ dummyh +dimension+ dummyh:dimension, data = df)
summary(fit3)

anova(fit1,fit3)

fit4 <- lm(price ~ ncolors+ dummyh:dimension+dimension, data = df)
summary(fit4)
anova(fit4,fit1)

bestmodel = fit4


# per ogni fit1,fit2,fit3,fit4
quartz()
par(mfrow=c(2,2))
plot(fit4)
shapiro.test(residuals(fit4))
par(mfrow=c(1,1))


summary(fit4)
new_data <- data.frame(method = 'handmade', dimension = 6.5, ncolors = 1,dummyh = 1)
# Fixed effects: (beta_0, beta_1, beta_2)

# Confidence 
Conf <- predict(fit4, new_data, 
                interval='confidence', level=1-0.05)  
t(Conf)

