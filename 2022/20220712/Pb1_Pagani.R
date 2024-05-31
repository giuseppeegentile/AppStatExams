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
data$size


svmfit <- svm(size~., data=data , kernel ='linear', cost =1, scale =FALSE )
summary(svmfit)
new_entry <- data.frame(armor.class=14,hit.points=50,strength=19,dexterity=10,constitution=16,intelligence=8,wisdom=12,charisma=13)
predict(svmfit, new_entry)


##With Plotting and projecting


# Prepare data for SVM
data <- read.table('dnd_monsters.txt', header=T)
value_scores = data.frame(pca.df$scores)
head(value_scores)
new_entry <- data.frame(armor.class=14,hit.points=50,strength=19,dexterity=10,constitution=16,intelligence=8,wisdom=12,charisma=13)




i1 <- which(data$size == "Tiny")
i2 <- which(data$size == "Huge")
df_class1 = data.frame(value_scores[data$size=="Tiny",1:2], y=rep(0,dim(value_scores[data$size=="Tiny",1:2])[1]))
df_class2 = data.frame(value_scores[data$size=="Huge",1:2], y=rep(1,dim(value_scores[data$size=="Huge",1:2])[1]))
dat = rbind(df_class1, df_class2)

head(dat)
svmfit <- svm(as.factor(y)~., data=dat , kernel ='linear', cost =1, scale =FALSE )
summary(svmfit)

par(mfrow=c(1,2))
plot(svmfit, dat, col =c('salmon', 'light blue'), pch=19)


###Projection

data <- read.table('dnd_monsters.txt', header=T)
head(data)
value_scores = data.frame(pca.df$scores)
i1 <- which(data$size == "Tiny")
i2 <- which(data$size == "Huge")
df_class1 = data.frame(value_scores[data$size=="Tiny",1:2], y=rep(0,dim(value_scores[data$size=="Tiny",1:2])[1]))
df_class2 = data.frame(value_scores[data$size=="Huge",1:2], y=rep(1,dim(value_scores[data$size=="Huge",1:2])[1]))
dat = rbind(df_class1, df_class2)

svmfit <- svm(as.factor(y)~., data=dat , kernel ='linear', cost =1, scale =FALSE )
summary(svmfit)

par(mfrow=c(1,2))
plot(svmfit, dat, col =c('salmon', 'light blue'), pch=19)
dat

new_entry <- c(armor.class=14,hit.points=50,strength=19,dexterity=10,constitution=16,intelligence=8,wisdom=12,charisma=13)
new_entry<-(new_entry- colMeans(df))/(sapply(df, sd))


#Computing the scores
mydf<- scale(data[,1:8])

M = pca.df$loadings[,1:2]
Mt= t(as.matrix(pca.df$loadings[,1:2]))

is.numeric(pca.df$loadings[, 1:2])
is.matrix(pca.df$loadings[, 1:2])
is.numeric(new_entry)
is.vector(new_entry)



#projection on the first 3 PC+
mydf
newdatapoint.proj <- Mt%*%as.matrix(new_entry-colMeans(mydf)) #mydf è quello su cui ho fatto la pca
newdatapoint.proj

a = predict(svmfit, t(newdatapoint.proj))

