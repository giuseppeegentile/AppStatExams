setwd("C:/Users/Giuseppe/Documents/.magistrale/secondo anno/AppStat/Exams/2021/20210618/20210618")
library(MVN)
library(car)
library(heplots)
data <- read.table('holiday.txt',header=T)
head(data)
dim(data)
summary(data)

# 2 way ANOVA


colnames(data) <- c("measure", "label_1", "label_2")
attach(data)
# Consider also the interactions
label_1_2  <- factor(paste(label_1, label_2))

# gbn
{
  #g <- length(levels(label_1))
  #b <- length(levels(label_2))
  # se sono a 0 fai cosi:
  g <- length(levels(as.factor(label_1)))
  b <- length(levels(as.factor(label_2)))
  
  n <- length(measure)/(g*b)
}

# means of groups and overall
{
  M           <- mean(measure)                    # overall mean
  M_label_1   <- tapply(measure, label_1, mean)   # mean per label_1
  M_label_2   <- tapply(measure, label_2, mean)   # mean per label_2
  
  M_label_1_2 <- tapply(measure, label_1_2, mean) # mean per gas station per gasoline type
}

# Plot all the hypothesis:
{
  par(mfrow=c(2,3), las=2)
  
  barplot(rep(M,g*b), names.arg=levels(label_1_2), main='Hp: No factor affect measure')
  
  barplot(rep(M_label_1,each=b), names.arg=levels(label_1_2),
          col=rep(c('blue','red'),each=2), main='Hp: Only label_1 affect measure')
  
  barplot(rep(M_label_2,times=g), names.arg=levels(label_1_2), 
          col=rep(c('darkgreen','orange'),times=2), main='Hp: Only label_2 affect measure')
  
  barplot(c(M_label_1[1]+M_label_2[1]-M, 
            M_label_1[1]+M_label_2[2]-M, 
            M_label_1[1]+M_label_2[3]-M, 
            M_label_1[2]+M_label_2[1]-M, 
            M_label_1[2]+M_label_2[2]-M,
            M_label_1[2]+M_label_2[3]-M
            ## add more of this if g>2 or b >2
  ), names.arg=levels(label_1_2), 
  col=rep(c('darkgreen','orange'),times=2), density=rep(10,4), angle=135, 
  main='Additive model label_1 + label_2: both (summed) affect measure')
  
  barplot(M_label_1_2, names.arg=levels(label_1_2),
          col=rainbow(5)[2:5], main='Model with Interaction label_1 & label_2 affect measure')
  # interaction means that the 95 in Esso is assumed different from the 95 in Shell
  plot(label_1_2, measure, col=rainbow(5)[2:5], xlab='')
}
# seems that only label_2 affect the measure



# Assumptions
{
  # If you have more than two data per group
  {
    # Check normality and variance homogeneity for the complete model
    Ps <- c(
      shapiro.test(measure[which(label_1 == "Chiavari"   & label_2 == "apt")])$p,
      shapiro.test(measure[which(label_1 == "Chiavari"   & label_2 == "bb")])$p,
      shapiro.test(measure[which(label_1 == "Chiavari"   & label_2 == "hotel")])$p,
      shapiro.test(measure[which(label_1 == "Rapallo" & label_2 == "apt")])$p,
      shapiro.test(measure[which(label_1 == "Rapallo" & label_2 == "bb")])$p,
      shapiro.test(measure[which(label_1 == "Rapallo" & label_2 == "hotel")])$p
    )
    Ps
    
    bartlett.test(measure, label_1)
    bartlett.test(measure, label_2)
    bartlett.test(measure, label_1_2)
  }
}

# Model with interaction (complete model): 
{
  #   X.ijk = mu + tau.i + beta.j + gamma.ij + eps.ijk; eps.ijk~N(0,sigma^2), 
  #         i=1,2 (effect station), j=1,2 (effect gasoline)
  fit.aov2.int <- aov(measure ~ label_1 + label_2 + label_1:label_2)
  summary.aov(fit.aov2.int)
}

# We reduce to additive model first 
fit.aov2.add <- aov(measure ~ label_1 + label_2)
summary.aov(fit.aov2.add)

# Only location affect the price
fit.aov<- aov(measure ~ label_2)
summary.aov(fit.aov)


n       <- dim(data)[1]      # total observations, LEVELS
ng      <- table(data$label_2)   # observations in each group
# treat   <- levels(data$label)  
treat   <- levels(factor(data$label_2)) # levels of the treatment
g       <- length(treat)     # number of levels (of groups)



# Point estimate for mean and variance
{
  X_bar = mean(data$measure)
  #  Mean
  {
    
    tau1 = mean((data[data$label_2 == treat[1],])$measure) - X_bar
    tau2 = mean((data[data$label_2 == treat[2],])$measure) - X_bar
    tau3 = mean((data[data$label_2 == treat[3],])$measure) - X_bar
    m_1 <- X_bar + tau1
    m_2 <- X_bar + tau2 
    m_3 <- X_bar + tau3 
    m_1
    m_2
    m_3
  }
  
  
  {
    W <- sum(fit.aov$residuals^2)
    var <- W/(fit.aov$df.residual)   
    var
  }
}

{
  # if you come from reduced model, replace with the dimension of the
  # group of that label
  # in this case we considered g, but if the only variable influent is label_2
  # replace everything with b
  fit <- fit.aov
  k = g*(g-1)/2 
  alpha = 0.05
  Mediag = tapply(data$measure, data$label_2, mean) # group-wise means
  SSres  = sum(residuals(fit)^2)
  S      = SSres/(n-g)  
  
  # interval for the difference mean of the treatments
  # CI for difference mean                                        (mean difference)
  ICrange=NULL
  for(i in 1:(g-1)) {
    for(j in (i+1):g) {
      cat(paste(treat[i],"-",treat[j], "\t"))        
      print(as.numeric(c(Mediag[i]-Mediag[j] - qt(1-alpha/(2*k), n-g) * sqrt(S * ( 1/ng[i] + 1/ng[j])),
                         Mediag[i]-Mediag[j] + qt(1-alpha/(2*k), n-g) * sqrt(S * ( 1/ng[i] + 1/ng[j])))))
      ICrange=rbind(ICrange,as.numeric(c(Mediag[i]-Mediag[j] - qt(1-alpha/(2*k), n-g) * sqrt(S * (1/ng[i] + 1/ng[j])),
                                         Mediag[i]-Mediag[j] + qt(1-alpha/(2*k), n-g) * sqrt(S * (1/ng[i] + 1/ng[j])))))
    }
  }
  # apt prices are significatly lower than hotel's
  # bb prices are significatly lower than hotel's
  # apt and bb have no significat difference
  
  # Plot Bonferroni for the mean difference
  {
    par(mfrow=c(1,2))
    #plot(data$label, data$measure, xlab='treatment', ylab='response', col = rainbow(g), las=2)
    
    h <- 1
    plot(c(1,g*(g-1)/2),range(ICrange), pch='',xlab='pairs treat', ylab='Conf. Int. tau response') 
    for(i in 1:(g-1)) {
      for(j in (i+1):g) {
        ind <- (i-1)*g-i*(i-1)/2+(j-i)
        lines (c(h,h), c(ICrange[ind,1],ICrange[ind,2]), col='grey55'); 
        points(h, Mediag[i]-Mediag[j], pch=16, col='grey55'); 
        points(h, ICrange[ind,1], col=rainbow(g)[j], pch=16); 
        points(h, ICrange[ind,2], col=rainbow(g)[i], pch=16); 
        h <- h+1
      }
    }
    abline(h=0)
  }
  

}



