setwd("C:/Users/Giuseppe/Documents/.magistrale/secondo anno/AppStat/Exams/2024/20240206/20240206")
library(MVN)
library(car)
library(heplots)
data <- read.table('diet.txt',header=T)
head(data)
dim(data)
summary(data)
p = dim(data)[2] - 2 # 2 are the columns of the two factors

# Preprocess: depends on how is organized
{
  v = 'X1'
  for(i in 2:p){
    v = c(v, paste('X', i, sep= ''))
  }
  # If labels are on first two columns:
  colnames(data) = c(v, 'label_1','label_2')
  
  # If labels are on last two columns:
  # colnames(data) = c(v,'label_1','label_2')
}

head(data) # check the correct renaming

# X1 <- pressure
# X2 <- colesterol




# Specify the factors label
{
  Label_1   <- factor(data$label_1) # Treat.1
  Label_2   <- factor(data$label_2) # Treat.2
  
  # all combinations of labels
  Label_1_2 <- factor(paste(Label_1, Label_2, sep=''))
  Label_1_2
}


data.feats = data[,1:p] 

fit2.int  = manova( as.matrix(data.feats) ~ Label_1 + Label_2 + Label_1:Label_2)
summary.manova(fit2.int, test="Wilks")
# the diet and the vitamin intake has significant impact on the two health indicators, but not their interaction
# the interaction between vitamin intake and being vegetarian is not statistically significant


# Assumptions (although we may have few data in each group!)
# Groups in the sense of ALL COMBINATIONS OF ALL LABELS
{
  # 1) normality (multivariate) in each group (4 test) (length(treat) = 4)
  {
    treat = levels(Label_1_2)
    Ps = 0*(1:length(treat))
    for(i in 1:length(treat)){
      Ps[i] <- mvn(data.feats[Label_1_2==levels(Label_1_2)[i],],)$multivariateNormality$`p value`
    }
    Ps
  }
  
  # 2) homogeneity of the covariance (qualitatively)
  {
    S1 <-  cov(data.feats[ Label_1_2==levels(Label_1_2)[1],])
    S2 <-  cov(data.feats[ Label_1_2==levels(Label_1_2)[2],])
    S3 <-  cov(data.feats[ Label_1_2==levels(Label_1_2)[3],])
    S4 <-  cov(data.feats[ Label_1_2==levels(Label_1_2)[4],])
    
    par(mfrow=c(1,4))
    image(S1, col=heat.colors(100),main='Cov. S1', asp=1, axes = FALSE, breaks = quantile(rbind(S1,S2,S3,S4), (0:100)/100, na.rm=TRUE))
    image(S2, col=heat.colors(100),main='Cov. S2', asp=1, axes = FALSE, breaks = quantile(rbind(S1,S2,S3,S4), (0:100)/100, na.rm=TRUE))
    image(S3, col=heat.colors(100),main='Cov. S3', asp=1, axes = FALSE, breaks = quantile(rbind(S1,S2,S3,S4), (0:100)/100, na.rm=TRUE))
    image(S4, col=heat.colors(100),main='Cov. S4', asp=1, axes = FALSE, breaks = quantile(rbind(S1,S2,S3,S4), (0:100)/100, na.rm=TRUE))
    
  }
  
}


# c) as sayed in a
fit2.add  = manova( as.matrix(data.feats) ~ Label_1 + Label_2)
summary.manova(fit2.add, test="Wilks")


# d
alpha <- 0.05
g <- length(levels(factor(Label_1))) # levels on the first treatment on p components
b <- length(levels(factor(Label_2))) # levels on the second treatment on p components

# Supposing symmetric design: dim(data)[1]/(g*b)
n <- dim(data)[1]/(g*b) # How many units per label combination

N <- g*b*n

# Within 
W <- summary.manova(fit2.add)$SS$Residuals
# W <- (n1-1)S_1 + ... + (ng-1)*S_g

# Comparisons
k <- p*g*(g-1)/2 + p*b*(b-1)/2
# because we have: g levels on the first treatment on p components
#                  b levels on the second treatment on p components
k


qT <- qt(1 - alpha / (2 * k), g*b*n-g-b+1)
# the degrees of freedom of the residuals on the additive model are g*b*n-g-b+1


mLabel_1_L  <- sapply(data.feats[Label_1=='FALSE',],mean)
mLabel_1_H  <- sapply(data.feats[Label_1=='TRUE',],mean)

mLabel_2_L  <- sapply(data.feats[Label_2=='FALSE',],mean)
mLabel_2_H  <- sapply(data.feats[Label_2=='TRUE',],mean)

# Due to balanced design: N/2
infLabel_1 <- mLabel_1_H-mLabel_1_L - qT * sqrt( diag(W)/(g*b*n-g-b+1) * (1/(N/2)+1/(N/2)) )
supLabel_1 <- mLabel_1_H-mLabel_1_L + qT * sqrt( diag(W)/(g*b*n-g-b+1) * (1/(N/2)+1/(N/2)) )


infLabel_2 <- mLabel_2_H-mLabel_2_L - qT * sqrt( diag(W)/(g*b*n-g-b+1) * (1/(N/2)+1/(N/2)) )
supLabel_2 <- mLabel_2_H-mLabel_2_L + qT * sqrt( diag(W)/(g*b*n-g-b+1) * (1/(N/2)+1/(N/2)) )

# the first is the difference on the measurment 1 (label1) given by H - L
# the second is the difference on the measurment 2 (label2) given by "+" - "-"
IC2   <- list(effectOfLabel1OnFeatures=cbind(infLabel_1, supLabel_1), effectOfLabel2OnFeatures=cbind(infLabel_2, supLabel_2))
IC2
# being vegetarian doesn't impact pressure, but reduce cholesterol significantly
# taking vitamins doesn't impact the cholesterol, but reduce pressure rate







