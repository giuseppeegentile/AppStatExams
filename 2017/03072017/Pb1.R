setwd("C:/Users/Giuseppe/Documents/.magistrale/secondo anno/AppStat/Exams/2017/20170703/20170703")
library(car)
library(MVN)
library(MVN)
library(car)
library(heplots)
data <- read.table('kimono.txt',header=T)
head(data)
dim(data)
summary(data)
# If data provided, rename accordingly
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
      shapiro.test(measure[which(label_1 == "Tokyo"   & label_2 == "hand-made")])$p,
      shapiro.test(measure[which(label_1 == "Tokyo"   & label_2 == "ready-to-use")])$p,
      shapiro.test(measure[which(label_1 == "Kyoto"   & label_2 == "hand-made")])$p,
      shapiro.test(measure[which(label_1 == "Kyoto" & label_2 == "ready-to-use")])$p
    )
    Ps
    
    # Check balanced experiment (not requested assumptions, but check it to say you's use lm)
    {
      length(measure[which(label_1 == "Tokyo"   & label_2 == "hand-made")])
      length(measure[which(label_1 == "Tokyo"   & label_2 == "ready-to-use")])
      length(measure[which(label_1 == "Kyoto"   & label_2 == "hand-made")])
      length(measure[which(label_1 == "Kyoto"   & label_2 == "ready-to-use")])
    }
    
    bartlett.test(measure, label_1)
    bartlett.test(measure, label_2)
    bartlett.test(measure, label_1_2)
  }
  
    # ok nroamlity and homogeneity of variances
  }
  
  
}


# Model with interaction (complete model): 
{
  #   X.ijk = mu + tau.i + beta.j + gamma.ij + eps.ijk; eps.ijk~N(0,sigma^2), 
  #         i=1,2 (effect station), j=1,2 (effect gasoline)
  fit.aov2.int <- aov(measure ~ label_1 + label_2 + label_1:label_2)
  summary.aov(fit.aov2.int)
  }
  
}

# Additive model: 
{
  ### X.ijk = mu + tau.i + beta.j + eps.ijk; eps.ijk~N(0,sigma^2), 
  #           i=1,2 (label_1), j=1,2 (label_2)
  fit.aov2.ad <- aov(measure ~ label_1 + label_2)
  summary.aov(fit.aov2.ad)
  
  # Remark: by removing the interaction, the residual degrees of freedom increase! 
}

#           i=1,2 (label_1), j=1,2 (label_2)
fit <- aov(measure ~ label_2)
summary.aov(fit)





n       <- dim(data)[1]      # total observations, LEVELS
ng      <- table(data$label_2)   # observations in each group
# treat   <- levels(data$label)  
treat   <- levels(factor(data$label_2)) # levels of the treatment
g       <- length(treat)     # number of levels (of groups)





{
  # if you come from reduced model, replace with the dimension of the
  # group of that label
  # in this case we considered g, but if the only variable influent is label_2
  # replace everything with b
  
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
  
}




