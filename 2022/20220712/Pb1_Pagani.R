#Lab of reference: Lab 2
#---------------------------------------------------------------------------
#---------------------------------------------------------------------------
#                                   PCA
#-----------------------------------------------------------------------------
#---------------------------------------------------------------------------
#Packages
rm(list=ls())
#load("~/Documents/Ingegneria Matematica/II_semestre/AppStat/Lab 5/mcshapiro.test.RData")
library(mvtnorm)  # to deal with multivariate normal distributions
library(car) # "Companion to Applied Regression" for regression analysis
library(rgl) # for 3D plots

#Considerations:
# - Gaussianity not necessary!
# - Standardize the variables if different/uncomparable units of measures

#TERMINOLOGY
## loadings: coefficients of the linear combination of the original 
#            variables that defines each principal component)
##  Scores:   coefficient values WRT the loadings 
#             of the projections of the centered (- colmeans) data 
#             on the PCs 

#DATA PREPROCESSING-----------------------
# - Nans removal (by default it ignores them)
# - Transform the data based on their distribution, scale if data is
#   Extremely sensitive to the unit of measure

#-------------------------------------------------------
#1. Read and plot data
#-------------------------------------------------------
df <- read.table('.txt', header=T)
head(df)
dim(df)
#Transform your data into a df
df <- data.frame(df)

#var.names <- c("i nomi ORDINATI delle mie variabili") 
#colnames(df) <- var.names

#If there are groups-------------------------------------
#Separate groups from df and update
group <- factor(df$nameofgroupcolumn)
group
df<-df[,:] #remove the group column(s)!
head(df)
names(df)

n<-dim(df)[1]
n
p<-dim(df)[2]
p

#------------------------------------------------
#Plot

## NOTA: nel lab c'è anche un plot per la varianza lungo tutte le direzioni
##    e anche dei plot 3D che proiettano sulle PC di diverse dimensionalità
##    e.g. punto, retta, piano...

# Scatter plot of the original data
quartz()
pairs(df, col=rainbow(dim(df)[1]), pch=16, main='Scatter plot of original data')

# Altro plot
plot(X, asp = 1, xlab = 'Var 1', ylab = 'Var 2', pch = 19, xlim = c(-6, 6), ylim = c(-5, 10))

quartz()
# pch = 20 means filled circles (pch should be the ASCII character code)
## Qui l'importante è la variabilità: se alcuni boxplot sono molto più grandi (ordini di grandezza) di altri bisogna standardizzare
boxplot(scale(df, center=TRUE, scale=FALSE), las=2, col='gold',pch=20
        , main='CENTERED data')  

# Plot 3D
#   dove X sono i dati
#   level è il livello di confidenza
#   alpha è la trasparenza
open3d()
points3d(X, asp=1, size=4)  # plot the points
axes3d()                    # add the axes
plot3d(ellipse3d(S, centre=M, level= 9/10), alpha=0.15, add=TRUE) # add the ellipsoid

#boxplot(df~group, las=2, col='gold',pch=20)  

#Remark: features with high variability --> higher loadings on the first PCs
#Remark: maintaining vs removing the outliers! #see gaussianity 1 pop hyptest
#Consider if the distribution is symmetric

#mean
mean<-colMeans(df)
mean 
#Covariance matrix
cov<- cov(df)
cov
#SD
SD<-diag(cov) #if necessary for standardization
SD
#Correlation matrix
R <- cor(df)
R
#Generalized variance
var.gen <- det(cov)
var.gen
#Total variance
var.tot <- sum(diag(cov))
var.tot

#----------------------------------------------------------------
#                               PCA 
#----------------------------------------------------------------
mydf <- scale(df) # per scalare i dati
# !!! NON SCALARE I DATI SE NON NECESSARIO: FA PERDERE INFORMAZIONE !!!
# Equivalente a fare PCA sulla matrice di correlazione (i.e. al matrice di covarianza dei dati standardizzati è uguale alla matrice di correlazione dei dati originali)

mydf<- #df or scaled df
pca.df <- princomp(mydf, scores=T)  #or prcomp
summary(pca.df)

#--------------------------------------------------------
#Loadings
#--------------------------------------------------------
#report the first loadings
load.df <- pca.df$loadings #leggo le principal components per colonne #Full list: load.df[,]
load.df 

#Plot of the first n.PCs loadings 
quartz()
n.PCs<-  
par(mfrow=c(n.PCs,1))
for(i in 1:n.PCs) 
  barplot(pca.df$loadings[,i], ylim = c(-1, 1), main=paste("PC",i), 
          cex.axis = 0.4) #to change dimension of labels

#Interpretation of the loadings: - spiegare cosa significano punteggi positivi 
# e cosa punteggi negativi
# First PC: weighted average of the number of nights in 3,4 stars hotel and residences
# Second PC: contrast between the number of nights in 3 and 4 stars hotel
# Third PC: residences

# When ALL loadings are positive, it means that that specific PC is about the average of the variables; if SOME PCs have the same sign together, that means that that PC is about the correlation between them; if a PC has some variables with positive loadings and some with negative loadings, that means that that PC is about the contrast between the two groups of variables.

# If the first component explains 98% of the variability alone, that means that either ALL our data is 
#   positively-correlated (i.e. we can actually represent the data with a single direction), but most often
#   this is due to one of the variables having a large scale w.r.t. the others and "masking" their contributions. In this case, 
#   we should standardise the data before running PCA.



#--------------------------------------------------------
# Explained variance - Screeplot
#--------------------------------------------------------
#mydf è sempre quello su cui ho fatto PCA

quartz() 
plot(cumsum(pca.df$sd^2)/sum(pca.df$sd^2), type='b', axes=F, xlab='number of components', 
     ylab='contribution to the total variance', ylim=c(0,1), main="Screeplot")  
abline(h=0.8, lty=2, col='blue')
box()
axis(2,at=0:10/10,labels=0:10/10)
axis(1,at=1:ncol(mydf),labels=1:ncol(mydf),las=2)

#Anlayze the plots:
# The first XXXXX PCs explains more than xxxxxxxxxxxxx% of the total variability. 
# This is due to ? 
# chiedersi sempre se c'è masking effect quando i loadings sono sbilanciati!

#You should keep the components that explain at least 80% of the variance usually,
#or where you see an elbow

#proportion of Variance explained
data.frame(variance=((pca.df$sd^2)/sum(pca.df$sd^2)))

#CUMULATIVE proportion of explained variance
data.frame(cumulative.var=cumsum(pca.df$sd^2)/sum(pca.df$sd^2))

#-------------------------------------------------------
#Scores
#-----------------------------------------------------
scores.df <- pca.df$scores
scores.df
# Gli scores sono i valori delle proiezioni dei dati sui PC, mentre i loadings sono i coefficienti della combinazione lineare che definisce i PC

#example 1 - with color code!---------------------------------
# scatter plot of the data along the first two principal components 
col.lab<-factor(as.numeric(group))

quartz()
plot(scores.df[,1:2], col=col.lab, main="scores along the first two PCs", pch=19)
abline(h=0, v=0, lty=2, col='grey')
legend("topright", legend=c(levels(group)), col=levels(col.lab), lty=1) #only in plot
#To make them more interpretable plotting them with their name
#text(scores.df[,1:2],dimnames(df)[[1]],cex=1)


#Comments 
#occhio ai segni dei loadings! Se il segno su una variabile era negativo e ora
#lo score è positivo, si deve intendere come trend
#negativo rispetto alla media

#Upper-left:
#Upper-right:
#Lower-left:
#Lower-right:


#example 2--------------------------------------
quartz()
plot(scores.df[,1:2]) #along the first 2
abline(h=0, v=0, lty=2, col='grey')
#color by group --> 
#col.lab<-rep(0, dim(mydf))
#Legend
#legend('topleft', legend=colnames(mydf), lwd=2, cex=0.85)

#To make them more interpretable plotting them with their name
#text(scores.df[,1:2],dimnames(df)[[1]],cex=1)

#proportion of Variance explained
data.frame(variance=((pca.df$sd^2)/sum(pca.df$sd^2)))

#CUMULATIVE proportion of explained variance
data.frame(cumulative.var=cumsum(pca.df$sd^2)/sum(pca.df$sd^2))

#------------------------------------------------
#Biplot
#------------------------------------------------
quartz()
biplot(pca.df)

#In general, food with high first and principal components have
#higher Kcal content  compared to the mean, linked mostly to
#sugar and carbs for cereals with high scores pn PC1, and fats
#in cereals with high scores in PC2


#----------------------------------------------------------
# Projection of new data onto the space generated on all PCS 
#--------------------------------------------------------
#Reference:lab2-extra example_PCA_NO_R.R

#Creation of a newpoint
newdatapoint <-mynewdatapoint  # c(13, 10, 11, 13) # vars in order

#Scale the newpoint ONLY IF it is to be scaled! Always df
newdatapoint<-(newdatapoint- colMeans(df))/(sapply(df, sd))


#Computing the scores
mydf<- #original or scaled df,  SCRIVERLO!!!
#projection on the first 3 PC
newdatapoint.proj <- t(pca.df$loadings[,1:3])%*%(newdatapoint-colMeans(mydf)) #mydf è quello su cui ho fatto la pca
newdatapoint.proj

#Plot on the fist 2 PCs of my projection
quartz()
plot(pca.df$scores[,1],pca.df$scores[,2],col='grey',
     pch=19,xlab='Comp.1',ylab='Comp.2')
points(newdatapoint.proj[1],newdatapoint.proj[2],col='red',pch=19) 
abline(v=0, lty=2)
abline(h=0, lty=2)










#EXTRA

### Projection on the space generated by the ONLY k-th principal component
#DA SISTEMARE ASSOLUTAMENTE

par(mfrow=c(2,5)) #cambia
matplot(t(mydf), type='l', main = 'Data', ylim=range(mydf))
meanF <- colMeans(mydf)
matplot(meanF, type='l', main = '0 PC', lwd=2, ylim=range(mydf))

for(i in 1:xxxx) #on the first xxx
{
  projection <- matrix(meanF, dim(mydf)[[1]], dim(mydf)[[2]], byrow=T) 
  + scores.df[,i] %*% t(load.df[,i])
  matplot(t(projection), type='l', main = paste(i, 'PC'), ylim=range(mydf))
  matplot(meanF, type='l', lwd=2, add=T)
}

### Projection on the space generated by the first k principal components
par(mfrow=c(2,5))
mydf<-scale(df)
matplot(t(mydf), type='l', main = 'Data', ylim=range(mydf))
meanF <- colMeans(mydf)
matplot(meanF, type='l', main = 'First 0 PCs', lwd=2, ylim=range(mydf))
projection <- matrix(meanF, dim(mydf)[[1]], dim(mydf)[[2]], byrow=T)
for(i in 1:3)
{
  projection <- projection + scores.df[,i] %*% t(load.df[,i])
  matplot(t(projection), type='l', main = paste('First', i, 'PCs'), 
          ylim=range(mydf))
  matplot(meanF, type='l', lwd=2, add=T)
}

head(projection)
dim(projection)

#----------------------------------------------------------
#----------------------------------------------------------
#                          AFTER PCA
#-----------------------------------------------------------
#----------------------------------------------------------
#1) Have I seen some outliers that I could remove?
# - I can remove them and rerun the whole analysis
