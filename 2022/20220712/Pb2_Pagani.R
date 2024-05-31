rm(list=ls())
library(mvtnorm)
library(rgl) 
library(car)
library(MVN)

#---------------------------------------------------------------------
#1. Clustering
#---------------------------------------------------------------------
df<-read.table("demogorgons.txt",header=T)
head(df)
n<-dim(df)[1]
p<-dim(df)[2]

#remove the group column from data
n<-dim(df)[1]
p<-dim(df)[2]





#---------------------------------------------------------------------------------------------------------------
#---------------------------------------------------------------------------------------------------------------
# HCLUST PROCEDURE  - 
#---------------------------------------------------------------------------------------------------------------
#---------------------------------------------------------------------------------------------------------------

# Computing distance matrix (dissimilarity matrix)
dist.matrix <- dist(df, method='euclidean') #euclidean, manhattan, camberra

# ylim = rev(range(1:150)) specchia la matrice per averla come ce la aspettiamo, altrimenti sarebbe un grafico e (0,0) sarebbe in basso a sinistra. In questo modo, rimane in ALTO a sinistra
par(mfrow=c(1,1)) # Se vogliamo plottare più metriche una a fianco all'altra
image(1:n, 1:n, as.matrix(dist.matrix), main = "metrics: NAME OF METRIC", asp = 1, xlab = "i", ylab = "j", ylim = rev(range(1:n)))
par(mfrow=c(1,1))

# Running hclust algoritm
hclust.output <- hclust(dist.matrix, method = "average") # complete, single, average, ward.D2


# plot dendrogram
plot(hclust.output, main = "Test", 
     hang = -0.1, xlab = "", labels = F, cex = 0.6, sub = "")
# Add rectangle with with k=myk clusters
myk=2

rect.hclust(hclust.output, k = myk)

# cut the cluster with k clusters
clusters <- cutree(hclust.output, k = myk)

# Table with output

# plot clusters 
plot(df, col = clusters, pch = 19)
legend('topright', legend=c(levels(factor(clusters))), col = c(levels(factor(clusters))), lty=1)



#---------------------------------------------------------------------
#1. Manova
#---------------------------------------------------------------------
df
clusters
df <-data.frame(df,clusters)
df
df1 <- df[df$clusters == 1, ]
df2 <- df[df$clusters == 2, ]
df1
df2




df1 <-df1[,1:2]
df2 <-df2[,1:2]
g=2
p=2

mvn(df1)
mvn(df2)
head(df)

bartlett.test(df[,1:2], df$clust) # 0.5642


S <- cov(df)
S1 <-  cov(df1)
S2 <-  cov(df2)

S
S1
S2
# Plot covariance matrices' heatmaps
par(mfrow=c(1,3))
#SE CE NE SONO 3 aggiungi S3 a myquantile!!!!
myquantile<-quantile(rbind(S1,S2), (0:100)/100, na.rm=TRUE) #case 2
image(S1, col=heat.colors(100),main='Cov. S1', asp=1, axes = FALSE, breaks = myquantile)
image(S2, col=heat.colors(100),main='Cov. S2', asp=1, axes = FALSE, breaks = myquantile)
#image(S3, col=heat.colors(100),main='Cov. S3', asp=1, axes = FALSE, breaks = myquantile)
par(mfrow=c(1,1))

typeof(clusters)
clusters


typeof(clusters)
clusters <- factor(clusters)
typeof(clusters)
clusters
length(clusters)
df
fit <- manova(as.matrix(df[,1:2]) ~ clusters)
summary.manova(fit,test="Wilks") # Exact tests for p<=2 or g<=3 already implemented in R
summary.aov(fit)

plot(fitted(fit),residuals(fit), xlab="fitted values", ylab="residuals")

# To see the ANOVA on each individual feature
# Via ANOVA: for each of the p=4 variables we perform an ANOVA test
#            to verify if the membership to a group has influence
#            on the mean of the variable (we explore separately the
#            4 axes directions in R^4)
summary.aov(fit)
fit$coefficients
# estimated mu,mu1,mu2
m  <- sapply(df,mean)  
m
m1 <- sapply(df1,mean) # estimates mu.1=mu+tau.1
m1
m2 <- sapply(df2,mean)  # estimates mu.2=mu+tau.2
m2

# plot clusters 
dfplot<-read.table("demogorgons.txt",header=T)

plot(dfplot, col = clusters, pch = 19)
points(m1[1],m1[2], pch=17,col="green")
points(m2[1],m2[2], pch=17,col="green")
legend('topright', legend=c(levels(factor(clusters))), col = c(levels(factor(clusters))), lty=1)


#For each cluster, provide a confidence region at level 95% for the mean of
#the longitude and latitude and plot them on a scatterplot with the data.
#The confidence region is an ellipse centered at the mean with axes given by the

n1 = 96
n2 = 97
SI1 <- solve(S1)
SI2 <- solve(S2)

# Significance level
alpha   <- .05
p = 2

# Fisher quantile
cfr.fisher1 <- ((n1-1)*p/(n1-p))*qf(1-alpha,p,n1-p)
cfr.fisher2 <- ((n2-1)*p/(n2-p))*qf(1-alpha,p,n2-p)

#### CONFIDENCE/REJECTION REGION
# Ellipsoidal confidence region with confidence level (1-alpha)100%

plot(dfplot, col = clusters, pch = 19)
legend('topright', legend=c(levels(factor(clusters))), col = c(levels(factor(clusters))), lty=1)
# Ellipse centered around our sample mean
ellipse(m1, S1/n1, sqrt(cfr.fisher1), col = 'black', lty = 2, lwd=2, center.cex=1)
ellipse(m2, S2/n2, sqrt(cfr.fisher2), col = 'red', lty = 2, lwd=2, center.cex=1)
