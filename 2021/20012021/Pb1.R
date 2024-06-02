setwd("C:/Users/Giuseppe/Documents/.magistrale/secondo anno/AppStat/Exams/2021/20210120/20210120")


library(MVN)
library(car)
library(heplots)
data <- read.table("wine.txt",header=T)


head(data)
# -> two way ANOVA
dim(data)

colnames(data) <- c("measure", "label_1", "label_2")
attach(data)

# label_1 = region
# label_2 = color

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
            M_label_1[3]+M_label_2[2]-M## add more of this if g>2 or b >2
  ), names.arg=levels(label_1_2), 
  col=rep(c('darkgreen','orange'),times=2), density=rep(10,4), angle=135, 
  main='Additive model label_1 + label_2: both (summed) affect measure')
  
  barplot(M_label_1_2, names.arg=levels(label_1_2),
          col=rainbow(5)[2:5], main='Model with Interaction label_1 & label_2 affect measure')
  # interaction means that the 95 in Esso is assumed different from the 95 in Shell
  plot(label_1_2, measure, col=rainbow(5)[2:5], xlab='')
}

# seems that only color influences alcohol

# Assumptions
{
  # If you have more than two data per group
  {
    # Check normality and variance homogeneity for the complete model
    Ps <- c(
      shapiro.test(measure[which(label_2 == "red"   & label_1== "Piemonte")])$p,
      shapiro.test(measure[which(label_2 == "red"   & label_1== "Toscana")])$p,
      shapiro.test(measure[which(label_2 == "red"   & label_1== "Veneto")])$p,
      shapiro.test(measure[which(label_2 == "white" & label_1 == "Piemonte")])$p,
      shapiro.test(measure[which(label_2 == "white" & label_1 == "Toscana")])$p,
      shapiro.test(measure[which(label_2 == "white" & label_1 == "Veneto")])$p
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
  
  # as expected from visual inspection, we can remove first the region factor
  # (even if is always better remove the interaction first, but is equivalent in this case)
  fit.aov2.add <- aov(measure ~ label_2 + label_1:label_2)
  summary.aov(fit.aov2.add)
  
  # and also the interaction now: we have an ANOVA one way
  fit.aov <- aov(measure ~ label_2)
  summary.aov(fit.aov)
}

n       <- dim(data)[1]      # total observations, LEVELS
ng      <- table(data$label_2)   # observations in each group
# treat   <- levels(data$label)  
treat   <- levels(factor(data$label_2)) # levels of the treatment
g       <- length(treat)     # number of levels (of groups)

ng # -> balanced


# Estimates for parameters
fit.aov$coefficients
# red influence positively alcohol, white negatively 
sd(fit.aov$residuals) # sigma



# Bonferroni confidence intervals for estimates of means and variance
{
  fit <- fit.aov
  DF <- fit$df # n - b
  Spooled <- sum(fit$res^2)/DF
  
  
  means <- as.vector(tapply(data$measure, data$label_2, mean))
  names(means) <- levels(factor(label_2))
  means
  
  
  
  ng <- c(length(which(label_2==levels(factor(label_2))[1])),
          length(which(label_2==levels(factor(label_2))[2])))
  
  # 99%
  alpha <- 0.01
  k     <- b + 1 # (b Conf Int for the means and 1 for the variance)
  BF    <- rbind(cbind(means - sqrt(Spooled / ng) * qt(1 - alpha / (2*k), DF), 
                       means + sqrt(Spooled / ng) * qt(1 - alpha / (2*k), DF)),
                 c(Spooled * DF / qchisq(1 - alpha/(2*k), DF), 
                   Spooled * DF / qchisq(alpha/(2*k), DF)))
  rownames(BF)[b + 1] <- 'Var.'
  BF
}





