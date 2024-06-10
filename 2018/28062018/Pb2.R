setwd("C:/Users/Giuseppe/Documents/.magistrale/secondo anno/AppStat/Exams/2018/20180628/20180628")
library(MVN)
library(car)
library(heplots)
data <- read.table('Mexican.txt',header=T)
head(data)
dim(data)
# two way ANOVA

colnames(data) <- c("measure", "label_1", "label_2")
attach(data)
# Consider also the interactions
label_1_2  <- factor(paste(label_1, label_2))

# gbn
{
  #g <- length(levels(label_1))
  #b <- length(levels(label_2))
  # se sono a 0 fai cosi:
  g <- length(levels(as.factor(label_1))) #2 -> type food
  b <- length(levels(as.factor(label_2))) # 3 -> area
  
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
      shapiro.test(measure[which(label_1 == "Tacos"   & label_2 == "Guanajato")])$p,
      shapiro.test(measure[which(label_1 == "Tacos"   & label_2 == "Cancun")])$p,
      shapiro.test(measure[which(label_1 == "Tacos"   & label_2 == "MexicoCity")])$p,
      shapiro.test(measure[which(label_1 == "Fajitas" & label_2 == "Guanajato")])$p,
      shapiro.test(measure[which(label_1 == "Fajitas" & label_2 == "Cancun")])$p,
      shapiro.test(measure[which(label_1 == "Fajitas" & label_2 == "MexicoCity")])$p
    )
    Ps
    
    # ok normality
    
    # Check balanced experiment (not requested assumptions, but check it to say you'd use lm)
    {
      length(measure[which(label_1 == "Tacos"   & label_2 == "Guanajato")])
      length(measure[which(label_1 == "Tacos"   & label_2 == "Guanajato")])
      length(measure[which(label_1 == "Tacos"   & label_2 == "MexicoCity")])
      length(measure[which(label_1 == "Fajitas"   & label_2 == "Guanajato")])
      length(measure[which(label_1 == "Fajitas"   & label_2 == "Guanajato")])
      length(measure[which(label_1 == "Fajitas"   & label_2 == "MexicoCity")])
    }
    
    bartlett.test(measure, label_1)
    bartlett.test(measure, label_2)
    bartlett.test(measure, label_1_2)
     # also homogeneity of variacne
  }
  
 
  
}

# Model with interaction (complete model): 
{
  #   X.ijk = mu + tau.i + beta.j + gamma.ij + eps.ijk; eps.ijk~N(0,sigma^2), 
  #         i=1,2 (effect station), j=1,2 (effect gasoline)
  fit.aov2.int <- aov(measure ~ label_1 + label_2 + label_1:label_2)
  summary.aov(fit.aov2.int)
  
  # we can remove interaction and consier the additive model
}

fit.aov2.ad <- aov(measure ~ label_1 + label_2 )
summary.aov(fit.aov2.ad)


# only the area is influencing the prices of meals,  not the type of meal
fit <- aov(measure ~ label_2 )
summary.aov(fit)

n       <- dim(data)[1]      # total observations, LEVELS
ng      <- table(data$label_2)   # observations in each group
# treat   <- levels(data$label)  
treat   <- levels(factor(data$label_2)) # levels of the treatment
g       <- length(treat)     # number of levels (of groups)



{

  k = g*(g-1)/2 
  alpha = 0.01
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
# Cancun - Guanajato 	    [1] 269.2994 342.8620
# Cancun - MexicoCity 	  [1] 273.9844 347.5470
# Guanajato - MexicoCity 	[1] -32.09627  41.46627

# Prices in cancun are much more high than price in guanajato and mexico city too
# instead prices in guajanato are comparable to the ones in mexico city




