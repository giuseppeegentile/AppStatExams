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





## Model diagnostic, Verify assumptions
{
  par(mfrow=c(2,2))
  plot(fm)
  
  # all fine
  
  
  shapiro.test(residuals(fm))
  

  par(mfrow=c(1,1))
}


# we can remove only hardness, is not influential
fm <- lm(y ~ rain + coarse + fine,data=data)
summary(fm) 


linearHypothesis(fm, rbind(c(0,0,1,-2)), c(0))
# can't reject
# -> coarse = 2*fine -> posso togliere coarse, e considerare 3*fine (?)
fm <- lm(y ~ rain + I(3*fine), data=data)
summary(fm)

z0   <- data.frame(rain=700, coarse=10, fine=8)
predict(fm, z0)
alpha=0.01
# Conf. int. for the mean
Conf <- predict(fm, z0, interval='confidence', level=1-alpha)  
Conf
# point estimate is 30.03895