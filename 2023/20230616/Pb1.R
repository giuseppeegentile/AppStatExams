
library(MVN)
library(car)
library(heplots)

setwd("C:/Users/Giuseppe/Documents/.magistrale/secondo anno/AppStat/Exams/2023/20230616/20230616")
data <- read.table("noise.txt",header=T)
head(data)
dim(data)


colnames(data) <- c("label_1", "label_2","measure")
attach(data)
# label_1 <- fuel
# label_2 <- category
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
            M_label_1[2]+M_label_2[1]-M, 
            M_label_1[2]+M_label_2[2]-M,
            M_label_1[3]+M_label_2[1]-M,
            M_label_1[3]+M_label_2[2]-M
  ), names.arg=levels(label_1_2), 
  col=rep(c('darkgreen','orange'),times=2), density=rep(10,4), angle=135, 
  main='Additive model label_1 + label_2: both (summed) affect measure')
  
  
  barplot(M_label_1_2, names.arg=levels(label_1_2),
          col=rainbow(5)[2:5], main='Model with Interaction label_1 & label_2 affect measure')
  
  plot(label_1_2, measure, col=rainbow(5)[2:5], xlab='')
  #we can already see that within variability is too high to assert that the means of the
  #group is different, we will prove this very likely
  
  # Instead, the type of fuel seems to affect the noise
}


### Model with interaction (complete model): 
{
  #   X.ijk = mu + tau.i + beta.j + gamma.ij + eps.ijk; eps.ijk~N(0,sigma^2), 
  #         i=1,2 (effect station), j=1,2 (effect gasoline)
  fit.aov2.int <- aov(measure ~ label_1 + label_2 + label_1:label_2)
  summary.aov(fit.aov2.int)
}



### Two Way ANOVA assumptions check:
# If you have more than two data per group
{
  # Check normality and variance homogeneity for the complete model
  Ps <- c(
    shapiro.test(measure[which(label_1 == "diesel"   & label_2 == "commercial")])$p,
    shapiro.test(measure[which(label_1 == "diesel"   & label_2 == "passenger")])$p,
    shapiro.test(measure[which(label_1 == "ethanol"   & label_2 == "commercial")])$p,
    shapiro.test(measure[which(label_1 == "ethanol" & label_2 == "passenger")])$p,
    shapiro.test(measure[which(label_1 == "gasoline" & label_2 == "commercial")])$p,
    shapiro.test(measure[which(label_1 == "gasoline" & label_2 == "passenger")])$p
  )
  Ps
  bartlett.test(measure, label_1)
  bartlett.test(measure, label_2)
  bartlett.test(measure, label_1:label_2)
  
  # we have all high pvalues, can't reject at 1% -> assumptions satisfied
}



### reduced model 1
{
  fit.aov2.red1 <- aov(measure ~ label_1 +label_1:label_2)
  summary.aov(fit.aov2.red1)
}

### reduced model 2
{
  fit.aov2.red2 <- aov(measure ~ label_1)
  summary.aov(fit.aov2.red2)
}


# c)
# Point estimates of mean and variance 
{
  # for each pair of treatment
  X_bar = mean(data$measure)
  
  
  {
    treat_label_1 = levels(factor(data$label_1))
    
    tau1 = mean((data[data$label_1 == treat_label_1[1],])$measure) - X_bar
    tau2 = mean((data[data$label_1 == treat_label_1[2],])$measure) - X_bar
    tau3 = mean((data[data$label_1 == treat_label_1[3],])$measure) - X_bar
    tau1
    tau2
    tau3
  
    # diesel and gasoline are increasing the noise
    # ethanol is less noisy 
    
    # m_1 <- X_bar + tau1 
    # m_2 <- X_bar + tau1 
    # m_3 <- X_bar + tau2 
    # m_1
    # m_2
    # m_3
  }
  # Estimate variance
  mse <- sum(summary(fit.aov2.red2)[[1]]$'Mean Sq')
  
  # Calculating the estimated standard deviation (sigma) total variance
  sigma <- sqrt(mse)
  sigma
}

n       <- dim(data)[1]      # total observations, LEVELS
ng      <- table(data$label_1)   # observations in each group
treat   <- levels(factor(data$label_1))  # levels of the treatment
g       <- length(treat)  


#Method 1: Bonferroni 
# Note: this doesn't answer the question: "Bonferroni intervals for means and variances"
{
  k = g*(g-1)/2
  alpha = 0.05
  Mediag = tapply(data$measure, data$label_1, mean) # group-wise means
  SSres  = sum(residuals(fit.aov2.red2)^2)
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
      ICrange
    }
  }
 
  
  # Plot Bonferroni for the mean difference
  {
    par(mfrow=c(1,1))
    
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
  # For lines that don't intersect the 0
  # we have evidence for those pair of treatment that they are different:
  # - above the 0:
  #     -> for each segment
  #        -> the point on top is significantly more contirbuting to the ""label""
  #           than the point on the bottom
  # - under the 0:
  #     -> for each segment
  #        -> the point on top is significantly less contributing to the ""label""
  #           than the point on the bottom
  


