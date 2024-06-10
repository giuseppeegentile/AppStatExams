setwd("C:/Users/Giuseppe/Documents/.magistrale/secondo anno/AppStat/Exams/2018/20180913/20180913")
library(MVN)
library(car)
library(heplots)
data <- read.table('Waiting.txt',header=T)
head(data)
dim(data)



# Rename accordingly
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



# Assumptions
{
  # If you have more than two data per group
  {
    # Check normality and variance homogeneity for the complete model
    Ps <- c(
      shapiro.test(measure[which(label_2 == "Bucarest"   & label_1 == "Dessert")])$p,
      shapiro.test(measure[which(label_2 == "Bucarest"   & label_1 == "Main"   )])$p,
      shapiro.test(measure[which(label_2 == "Bucarest"   & label_1 == "Starter")])$p,
      shapiro.test(measure[which(label_2 == "Iasi"       & label_1 == "Dessert")])$p,
      shapiro.test(measure[which(label_2 == "Iasi"       & label_1 == "Main"   )])$p,
      shapiro.test(measure[which(label_2 == "Iasi"       & label_1 == "Starter")])$p
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

# additive
fit.aov2.add <- aov(measure ~ label_1 + label_2 )
summary.aov(fit.aov2.add)


# anova one way
fit <- aov(measure ~ label_1)
summary.aov(fit)

n       <- dim(data)[1]      # total observations, LEVELS
ng      <- table(data$label_1)   # observations in each group
# treat   <- levels(data$label)  
treat   <- levels(factor(data$label_1)) # levels of the treatment
g       <- length(treat)     # number of levels (of groups)

# Bonferroni for the mean difference 
{
  # if you come from reduced model, check that g is actually the one of the label influencing
  # in this case we considered g, but if the only variable influent is label_2
  # replace everything with b
  
  k = g*(g-1)/2 + g # bonferroni for the mean different and for the variacne within groups
  alpha = 0.05
  Mediag = tapply(data$measure, data$label_1, mean) # group-wise means
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
  ICrange
  
  # no significant difference of waiting times between the order and service of both main and starter 
  # witing times between order and service is instead lower for dessert respect to starter dish and main
  
  
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


D <- data[which(data$label_1 == "Dessert"),]$measure
D.cov = var(D)
D.mean = mean(D)
ICvar1 <- cbind(inf=(D.cov)*(n-1) / qchisq(1 - alpha/(2*k), n-1),
               center=(D.cov),
               sup=(D.cov)*(n-1) / qchisq(alpha/(2*k), n-1))

ICvar1

D <- data[which(data$label_1 == "Main"),]$measure
D.cov = var(D)
D.mean = mean(D)
ICvar2 <- cbind(inf=(D.cov)*(n-1) / qchisq(1 - alpha/(2*k), n-1),
               center=(D.cov),
               sup=(D.cov)*(n-1) / qchisq(alpha/(2*k), n-1))

ICvar2
D <- data[which(data$label_1 == "Starter"),]$measure
D.cov = var(D)
D.mean = mean(D)
ICvar3 <- cbind(inf=(D.cov)*(n-1) / qchisq(1 - alpha/(2*k), n-1),
               center=(D.cov),
               sup=(D.cov)*(n-1) / qchisq(alpha/(2*k), n-1))

ICvar3

ICvar1
ICvar2
ICvar3











# c) 
n <- dim(data)[1]
g <- 3
k <- 4
alpha=.05
ng <- table(course)
treat <- levels(course)

Media   <- mean(waiting)
Mediag  <- tapply(waiting, course, mean)

SSres <- sum(residuals(fit.aov1)^2)

S <- SSres/(n-g)

# CI for all the differences
ICrange=NULL
for(i in 1:(g-1)) {
  for(j in (i+1):g) {
    print(paste(treat[i],"-",treat[j]))        
    print(as.numeric(c(Mediag[i]-Mediag[j] - qt(1-alpha/(2*k), n-g) * sqrt( S * ( 1/ng[i] + 1/ng[j] )),
                       Mediag[i]-Mediag[j] + qt(1-alpha/(2*k), n-g) * sqrt( S * ( 1/ng[i] + 1/ng[j] )))))
    ICrange=rbind(ICrange,as.numeric(c(Mediag[i]-Mediag[j] - qt(1-alpha/(2*k), n-g) * sqrt( S * ( 1/ng[i] + 1/ng[j] )),
                                       Mediag[i]-Mediag[j] + qt(1-alpha/(2*k), n-g) * sqrt( S * ( 1/ng[i] + 1/ng[j] )))))
  }
}





