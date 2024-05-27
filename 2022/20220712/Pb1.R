setwd("C:/Users/Giuseppe/Documents/.magistrale/secondo anno/AppStat/Exams/2022/20220712/20220712")
library(mvtnorm) # to deal with multivariate normal distributions
library(car) # "Companion to Applied Regression" for regression analysis


data <- read.table('dnd_monsters.txt', header=T)

head(data)
dim(data)

n <- dim(data)[1]
p <- dim(data)[2]

# if there are labels (two in this case), skip otherwise
{
  # store them and remove from data
  data.label <- data$size
  data       <- data[, -dim(data)[2]]
}



{
  par(mfrow = c(1,1))
  boxplot(data, las = 2, col = 'gold')
  ## centred boxplot
  boxplot(scale(x = data, center = T, scale = F), las = 2, col = 'gold')
  # there is more variability for hit points than all the other variables
  # we must standardize otherwise that feature will mask all the others
}

{
  data.sd <- scale(data)
  data.sd <- data.frame(data.sd)
  head(data.sd)
  # Boxplot
  par(mfrow = c(1, 1))
  boxplot(data.sd, las = 2, col = 'gold')
  # now every box has similar height
  # you can see in the next plot how every feature has variance 1
  barplot(sapply(data.sd, sd)^2, las = 2, main = 'Standardized Variables', ylim = c(0, 7),
          ylab = 'Variances')
}

# PCA
{ 
  pc.data <- princomp(data.sd, scores = T)
  pc.data
  summary(pc.data)
}

# Loadings
{
  load.data <- pc.data$loadings
  
  
  # Graphical representation of loadings of the first 3 PC
  par(mar = c(2,2,2,1), mfrow=c(2,1))
  for(i in 1:2)barplot(load.data[,i], ylim = c(-1, 1), main=paste('Loadings PC ',i,sep=''))
  # the first component is a weighted average of all the features,
  # but is not accounting much for dexterity
  # the second component is taking into account was was left by first component
  # and also penalize physical characteristic, whilst giving positive weights to 
  # tactical properties: intelligence, charisma, ecc
}
scores.data <- pc.data$scores

par(mfrow=c(1,1))
# CASE 2: only a label provided
{
  value_scores = data.frame(scores.data)
  # Assign a color to each possible label
  colors <- c("green","orange","red","purple","grey","black")
  all_possible_labels <- c(unique(data.label))
  data.label <- factor(data.label, levels = all_possible_labels)
  
  col.lab1 <- rep(NA, n)
  for(i in 1:n)
    col.lab1[i] <- colors[which(data.label[i] == levels(data.label))]
  
  plot(value_scores[, 1:2], col = col.lab1, pch = 19)
  abline(h = min(value_scores[,2]), v = min(value_scores[,1]), col = 1)
  points(value_scores[, 1], rep(min(value_scores[,2]), n), col = col.lab1, pch = 19)
  points(rep(min(value_scores[,1]), n), value_scores[, 2], col = col.lab1, pch = 19)
  abline(h = 0, v = 0, lty = 2, col = 'grey')
  legend('topright', levels(data.label), fill = colors, bty = 'n')
}
# we can see a clear incrementing in size with the component 1
# gargantuan have negative component 2, that indicate that very very big monsters
# have less tactical skills


library(MASS)
library(class)
library(rgl)
library(mvtnorm)
library(MVN)
library(e1071)


data <- read.table('dnd_monsters.txt', header=T)
head(data)

value_scores = data.frame(pc.data$scores)
i1 <- which(data$size == "Tiny")
i2 <- which(data$size == "Huge")
df_class1 = data.frame(value_scores[data$size=="Tiny",1:2], y=rep(0,dim(value_scores[data$size=="Tiny",1:2])[1]))
df_class2 = data.frame(value_scores[data$size=="Huge",1:2], y=rep(1,dim(value_scores[data$size=="Huge",1:2])[1]))
dat = rbind(df_class1, df_class2)

svmfit <- svm(as.factor(y)~., data=dat , kernel ='linear', cost =1, scale =FALSE )
summary(svmfit)

par(mfrow=c(1,2))
plot(svmfit, dat, col =c('salmon', 'light blue'), pch=19)



new_entry <- data.frame(armor.class=14,hit.points=50,strength=19,dexterity=10,constitution=16,intelligence=8,wisdom=12,charisma=13)
x.mean = colMeans(data[,-dim(data)[2]])
x.cov = sapply(data[,-dim(data)[2]], FUN = sd)

new_entry = (new_entry - x.mean) / x.cov
pc_proj = as.matrix(new) %*% pc.data$loadings[,1:2]
pc_proj
predict(svmfit, pc_proj)
new_entry_projected
