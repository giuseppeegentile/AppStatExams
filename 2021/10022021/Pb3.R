setwd("C:/Users/Giuseppe/Documents/.magistrale/secondo anno/AppStat/Exams/2021/20210210/20210210")
data <- read.table("landslides.txt",header=T)
plot(data, pch=19)

n   <- dim(data)[[1]]
head(data)

# Target variable
y   <-  data$rate
fm <- lm(data$rate ~ .,data=data)
summary(fm) 
sqrt(sum(residuals(fm)^2)/fm$df)  # s estimate of sigma
coefficients(fm)

#(Intercept)         rain     hardness       coarse         fine 
#20.514647296  0.006115268  0.011423423  0.385216816  0.195448455



## Model diagnostic, Verify assumptions
{
  par(mfrow=c(2,2))
  plot(fm)
  
  # all fine
  
  
  shapiro.test(residuals(fm))
  

  par(mfrow=c(1,1))
}


# we can remove only hardness, is not influential
fm.orig <- lm(y ~ rain + coarse + fine,data=data)
summary(fm.orig) 

vif(fm.orig)
linearHypothesis(fm.orig, rbind(c(0,0,1,-2)), c(0))


fm1 <- lm(y ~ rain + I(1.5*coarse), data=data)
summary(fm1)

fm2 <- lm(y ~ rain + I(3*fine), data=data)
summary(fm2)

AIC(fm1) #143.1087
AIC(fm2) #176.202
# shouldn't they be equal?

# can't reject
# -> coarse = 2*fine -> posso togliere coarse, e considerare 3*fine (?)
fm <- lm(y ~ rain + coarse + coarse:fine, data=data)
summary(fm)

AIC(fm.orig) #128.0724
AIC(fm) #127.5045
#the only solution that led to a better model with all influential variable is the last one
# but why?

z0   <- data.frame(rain=700, coarse=10, fine=8)
predict(fm, z0)
alpha=0.01
# Conf. int. for the mean
Conf <- predict(fm, z0, interval='confidence', level=1-alpha)  
Conf
# point estimate is 30.28448






