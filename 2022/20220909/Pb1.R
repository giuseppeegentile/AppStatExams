setwd("C:/Users/Giuseppe/Documents/.magistrale/secondo anno/AppStat/Exams/2022/20220909/20220909")

library(MVN)
library(car)
library(heplots)
library(mvtnorm)
data <- read.table('dinosaurs.txt',header=T)
head(data)
dim(data)
summary(data)

names <- data[,1]
data <- data[,2:4]

# If data provided, rename accordingly
colnames(data) <- c("label_1", "label_2","measure")
attach(data)
# Consider also the interactions
label_1_2  <- factor(paste(label_1, label_2))

# gbn
{

  g <- length(levels(as.factor(label_1))) # 3
  b <- length(levels(as.factor(label_2)))# 2
  
  n <- length(measure)/(g*b)
}


# means of groups and overall
{
  M           <- mean(measure)                    # overall mean
  M_label_1   <- tapply(measure, label_1, mean)   # mean per label_1
  M_label_2   <- tapply(measure, label_2, mean)   # mean per label_2
  
  M_label_1_2 <- tapply(measure, label_1_2, mean) # mean per gas station per gasoline type
}

{
  # Check normality and variance homogeneity for the complete model
  Ps <- c(
    shapiro.test(measure[which(label_1 == "carnivorous"   & label_2 == "Cretaceous")])$p,
    shapiro.test(measure[which(label_1 == "carnivorous"   & label_2 == "Jurassic")])$p,
    shapiro.test(measure[which(label_1 == "herbivorous"   & label_2 == "Cretaceous")])$p,
    shapiro.test(measure[which(label_1 == "herbivorous" & label_2 == "Jurassic")])$p,
    shapiro.test(measure[which(label_1 == "omnivorous" & label_2 == "Cretaceous")])$p,
    shapiro.test(measure[which(label_1 == "omnivorous" & label_2 == "Jurassic")])$p
  )
  Ps
  # very far from normality
  bartlett.test(measure, label_1)
  bartlett.test(measure, label_2)
  bartlett.test(measure, label_1_2)
}

par(mfrow=c(2,3))
boxplot(measure[which(label_1 == "carnivorous"   & label_2 == "Cretaceous")])
boxplot(measure[which(label_1 == "carnivorous"   & label_2 == "Jurassic")])
boxplot(measure[which(label_1 == "herbivorous"   & label_2 == "Cretaceous")])
boxplot(measure[which(label_1 == "herbivorous" & label_2 == "Jurassic")])
boxplot(measure[which(label_1 == "omnivorous" & label_2 == "Cretaceous")])
boxplot(measure[which(label_1 == "omnivorous" & label_2 == "Jurassic")])


# Box Cox transformation
lambda.mult1 <-powerTransform(measure)
measure <-bcPower(measure,lambda.mult1$lambda)
Ps <- c(
  shapiro.test(measure[which(label_1 == "carnivorous"   & label_2 == "Cretaceous")])$p,
  shapiro.test(measure[which(label_1 == "carnivorous"   & label_2 == "Jurassic")])$p,
  shapiro.test(measure[which(label_1 == "herbivorous"   & label_2 == "Cretaceous")])$p,
  shapiro.test(measure[which(label_1 == "herbivorous" & label_2 == "Jurassic")])$p,
  shapiro.test(measure[which(label_1 == "omnivorous" & label_2 == "Cretaceous")])$p,
  shapiro.test(measure[which(label_1 == "omnivorous" & label_2 == "Jurassic")])$p
)
Ps
# can't reject normality at 1%


par(mfrow=c(2,3))
boxplot(measure[which(label_1 == "carnivorous"   & label_2 == "Cretaceous")])
boxplot(measure[which(label_1 == "carnivorous"   & label_2 == "Jurassic")])
boxplot(measure[which(label_1 == "herbivorous"   & label_2 == "Cretaceous")])
boxplot(measure[which(label_1 == "herbivorous" & label_2 == "Jurassic")])
boxplot(measure[which(label_1 == "omnivorous" & label_2 == "Cretaceous")])
boxplot(measure[which(label_1 == "omnivorous" & label_2 == "Jurassic")])

#We also have homogeneity of variance
bartlett.test(measure, label_1)
bartlett.test(measure, label_2)
bartlett.test(measure, label_1_2)


# Complete model
fit.aov2.int <- aov(measure ~ label_1 + label_2 + label_1:label_2)
summary.aov(fit.aov2.int)

# Additive model
fit.aov2.add <- aov(measure ~ label_1 + label_2)
summary.aov(fit.aov2.add)

# geological period is not influencing the length -> anova one way
# Additive model
fit.aov <- aov(measure ~ label_1)
summary.aov(fit.aov)


# Estimates for parameters
fit.aov$coefficients
sd(fit.aov$residuals) # sigma

n       <- dim(data)[1]      # total observations, LEVELS
ng      <- table(data$label_1)   # observations in each group
treat   <- levels(factor(data$label_1))  # levels of the treatment
g       <- length(treat) 

# Point estimate for mean and variance
{
  X_bar = mean(measure)
  #  Mean
  {
    
    tau1 = mean((measure[label_1 == treat[1]])) - X_bar
    tau2 = mean((measure[label_1 == treat[2]])) - X_bar
    
    m_1 <- X_bar + tau1
    m_2 <- X_bar + tau2 
    m_1
    m_2
  }
  
  # For variance is the same for every model, check DF and model used
  {
    W <- sum(fit.aov$residuals^2)
    var <- W/(fit.aov$df.residual)   # Replace gbn-b with DF of Residuals
    #(check that is the correct model used)
    var
  }
}


{
  k = g*(g-1)/2
  alpha = 0.05
  Mediag = tapply(measure, label_1, mean) # group-wise means
  SSres  = sum(residuals(fit.aov)^2)
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
  # Significant effect between carn and herbivorous, but not with omnivorous
  # Also effect between herbivorous and omnivorous
  
  # carn is influencing much more the lenght than the herbivorous
  # omnivorous is influencing much more the lenght than the herbivorous
  #no difference between omn and carn
  
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


