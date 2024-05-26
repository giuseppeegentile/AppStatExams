setwd("C:/Users/Giuseppe/Documents/.magistrale/secondo anno/AppStat/Exams/2024/20240206/20240206")
data <- read.table("diet.txt", header=T)

library(MVN)
library(car)
library(heplots)
# Two way MANOVA: 
# two features:   X1= pressure X2=cholesterol (p = 2)
# two "treatments": label_1=vegetarian label_2=vitamin (g=2)

# Exploration 
head(data)          
p = dim(data)[2] - 2 

# Preprocess
{
  v = 'X1'
  for(i in 2:p){
    v = c(v, paste('X', i, sep= ''))
  }
  # If labels are on first two columns:
  colnames(data) = c(v,'label_1','label_2')
  
}

head(data) # check the correct renaming

# Specify the factors label
{
  Label_1   <- factor(data$label_1, labels=c('FALSE','TRUE')) # Treat.1
  Label_2   <- factor(data$label_2, labels=c('FALSE','TRUE')) # Treat.2
  
  # all combinations of labels
  Label_1_2 <- factor(paste(Label_1, Label_2, sep=''))
  Label_1_2
}

data.feats = data[,1:p] 

# b)
# Assumptions 
{
  # 1) normality (multivariate) in each group (4 test) (length(treat) = 4)
  {
    treat = levels(Label_1_2)
    Ps = 0*(1:length(treat))
    for(i in 1:length(treat)){
      Ps[i] <- mvn(data.feats[Label_1_2==levels(Label_1_2)[i],],)$multivariateNormality$`p value`
    }
    Ps
  } # we have normality
  
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

# assumptions satisfied. Only the TRUE-TRUE group has different covariance matrix,
# but we have normality and MANOVA is robust to its assumptions. we proceed, indeed.

### Model with interaction (complete model): 
{
  ### X.ijk = mu + tau.i + beta.j + gamma.ij + eps.ijk; eps.ijk~N_p(0,Sigma),
  ###                                           i=1,2 (effect label_1 ),
  ###                                           j=1,2 (effect label_2 ),
  ###     (X.ijs, mu, tau.i, beta.j, gamma.ij in R^p)
  
  fit2.int  = manova( as.matrix(data.feats) ~ Label_1 + Label_2 + Label_1:Label_2)
  summary.manova(fit2.int, test="Wilks")
}
# Yes, dietary habits have significant effect on the health indicators, but their interaction
# doesn't -> c) we remove the interaction

fit2.add  = manova( as.matrix(data.feats) ~ Label_1 + Label_2 )
summary.manova(fit2.add, test="Wilks")

# d)
{
  # See on which feature the group (label) has effect
  summary.aov(fit2.add) 
  # low pval-> that group (label_x) affect that feature X_k 
  # vitamin intake affect the pressure
  # being vegetarian affect the cholesterol (probably reducing, from prior knowledge assumptions)
  #             to prove it in a statistical POV we need bonferroni to see if it reduce
  #             or increase it
}


# Bonferroni: get the levels of treatment (labels) that induces the difference
{
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
  
  
  mLabel_1_FALSE  <- sapply(data.feats[Label_1=='FALSE',],mean)
  mLabel_1_TRUE  <- sapply(data.feats[Label_1=='TRUE',],mean)
  
  mLabel_2_FALSE  <- sapply(data.feats[Label_2=='FALSE',],mean)
  mLabel_2_TRUE  <- sapply(data.feats[Label_2=='TRUE',],mean)
  
  # Due to balanced design: N/2
  infLabel_1 <- mLabel_1_TRUE-mLabel_1_FALSE - qT * sqrt( diag(W)/(g*b*n-g-b+1) * (1/(N/2)+1/(N/2)) )
  supLabel_1 <- mLabel_1_TRUE-mLabel_1_FALSE + qT * sqrt( diag(W)/(g*b*n-g-b+1) * (1/(N/2)+1/(N/2)) )
  
  
  infLabel_2 <- mLabel_2_TRUE-mLabel_2_FALSE - qT * sqrt( diag(W)/(g*b*n-g-b+1) * (1/(N/2)+1/(N/2)) )
  supLabel_2 <- mLabel_2_TRUE-mLabel_2_FALSE + qT * sqrt( diag(W)/(g*b*n-g-b+1) * (1/(N/2)+1/(N/2)) )
  
  IC2   <- list(effectOfLabel1OnFeatures=cbind(infLabel_1, supLabel_1), effectOfLabel2OnFeatures=cbind(infLabel_2, supLabel_2))
  IC2
}
# We can see that vitamin intake reduce the pressure without affecting the cholesterol significantly
# Being vegetarian reduce the cholesterol but doesn't significatly impact pressure


