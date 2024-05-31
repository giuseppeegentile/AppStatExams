# Load necessary libraries
library(mvtnorm)
library(car)
library(rgl)
library(e1071)

# Load data
df <- read.table('dnd_monsters.txt', header=T)
df <- data.frame(df)

# Separate groups from df and update
group <- factor(df$size)
df <- df[,1:8] # Remove the group column
n <- dim(df)[1]
p <- dim(df)[2]

# Pair plot of original data
pairs(df, col=rainbow(dim(df)[1]), pch=16, main='Scatter plot of original data')

# Boxplot of centered data
boxplot(scale(df, center=TRUE, scale=FALSE), las=2, col='gold', pch=20, main='CENTERED data')

# Perform PCA
mydf <- scale(df) # Standardize the data
pca.df <- princomp(mydf, scores=T)
summary(pca.df)

# Extract scores and loadings
scores.df <- pca.df$scores
load.df <- pca.df$loadings

# Interpretation of loadings
# The first loading is related to the mean of all the characteristics, so positively correlated with all 8 characteristics.
# The second one shows the contrast between physical and non-physical characteristics, with "dexterity" not considered physical.
# It indicates the contrast between physical prowess and other qualities.

# Plot of the first n.PCs loadings
n.PCs <- 2  
par(mfrow=c(n.PCs,1))
for(i in 1:n.PCs) 
  barplot(pca.df$loadings[,i], ylim = c(-1, 1), main=paste("PC",i), 
          cex.axis = 0.4) #to change dimension of labels

# Plot PCA Scores
col.lab <- factor(as.numeric(group))

plot(scores.df[,1:2], col=col.lab, main="Scores along the first two PCs", pch=19)
abline(h=0, v=0, lty=2, col='grey')
legend("topright", legend=c(levels(group)), col=1:length(levels(group)), pch=19)

# Interpretation of PCA scores plot
# The bigger animals have higher component 1 on average and lower component 2.
# This suggests that bigger animals have higher characteristics overall, with more emphasis on physical prowess and lower dexterity and mental qualities.


###Without plotting and projecting

data <- read.table('dnd_monsters.txt', header=T)
data <- subset(data, size %in% c("Tiny", "Huge"))
data

i1 <- which(data$group == "Tiny")
i2 <- which(data$group == "Huge")
data$size<-factor(data$size)
data
d1 <- subset(data, size %in% c("Tiny"))
d1
d2 <- subset(data, size %in% c("Huge"))
head(data)
m <-  colMeans(data[,1:8])
m1 <- colMeans(d1[,1:8])
m2 <- colMeans(data[,1:8])

S1 <- cov(data[,1:8])
S2 <- cov(d1[,1:8])
S3 <- cov(d2[,1:8])
n1 = 26
n2 = 40
g = 2
Sp  <- ((n1-1)*S1+(n2-1)*S2)/(n-g)

s <- min(g-1,p)
s = 2

B <- 1/g*(cbind(m1 - m) %*% rbind(m1 - m) +cbind(m2 - m) %*% rbind(m2 - m))

B

B# Matrix Spooled^(-1/2), used for reparametrisation. Note: \ref{def:FisherClassifier}
val.Sp <- eigen(Sp)$val
vec.Sp <- eigen(Sp)$vec

invSp.2 <- 1/sqrt(val.Sp[1])*vec.Sp[,1]%*%t(vec.Sp[,1]) + 1/sqrt(val.Sp[2])*vec.Sp[,2]%*%t(vec.Sp[,2])
invSp.2
B

# spectral decomposition of Sp^(-1/2) B Sp^(-1/2)
spec.dec <- eigen(invSp.2 %*% B %*% invSp.2)

# first canonical coordinate
a1 <- invSp.2 %*% spec.dec$vec[,1]
a1

# second canonical coordinate
a2 <- invSp.2 %*% spec.dec$vec[,2]
a2




cc1.data <- as.matrix(data[,1:8])%*%a1
cc2.data <- as.matrix(data[,1:8])%*%a2
coord.cc <- cbind(cc1.data,cc2.data)

dfsvm = data.frame(cc1.data,cc2.data, size = data$size)

head(dfsvm)

svmfit <- svm(size~., data=dfsvm , kernel ='linear', cost =1, scale =FALSE )
summary(svmfit)
plot(svmfit, dfsvm, col =c('salmon', 'light blue'), pch=19)

