setwd("C:/Users/Giuseppe/Documents/.magistrale/secondo anno/AppStat/Exams/2021/20210906/20210906")
data <- read.table("boats.txt",header=T)
data


head(data)
y = data$price
plot(data, pch=19)

n   <- dim(data)[[1]]




fm <- lm(y ~material+ length + power + draught + crew + year   ,data=data)
summary(fm) 

# Residual standard error: (estimated variance of residual )^/1/2
sqrt(sum(residuals(fm)^2)/fm$df)  # s estimate of sigma
coefficients(fm)

## Model diagnostic, Verify assumptions
{
  par(mfrow=c(2,2))
  plot(fm)
 #ok
  
  shapiro.test(residuals(fm))
  
  
  
  par(mfrow=c(1,1))
}
linearHypothesis(fm, rbind(c(0,0,1,0,0,0,0), 
                           c(0,0,0,1,0,0,0),
                           c(0,0,0,0,1,0,0)), c(0,0,0))
# yes is influential



linearHypothesis(fm, rbind(c(0,0,0,0,0,1,0),
                           c(0,0,0,0,0,0,1)), c(0,0))

# We can remove draught
fm <- lm(y ~material+ length + power  + crew + year   ,data=data)
summary(fm) 

# We can remove year 
fm <- lm(y ~material+ length + power  + crew   ,data=data)
summary(fm) 


fm.original <- lm(y ~material+ length + power + draught + crew + year   ,data=data)
summary(fm.original)
linearHypothesis(fm.original, rbind(c(0,0,0,0,0,0,1),
                                    c(0,0,0,0,1,0,0)), c(0,0))
# we can't reject at 1%, moreover, the fitted value is not changed a lot (still 98.5%), we prefer less parameters
# for a better generalization

# Final model
fm <- lm(y ~material+ length + power  + crew   ,data=data)
summary(fm) 



## Model diagnostic, Verify assumptions
{
  par(mfrow=c(2,2))
  plot(fm)
  #ok
  
  shapiro.test(residuals(fm))
  par(mfrow=c(1,1))
}

# Residual standard error: (estimated variance of residual )^/1/2
sqrt(sum(residuals(fm)^2)/fm$df)  # s estimate of sigma
coefficients(fm)

z0 <- data.frame(length = 10 , power = 1070
, draught = 1.5 ,  crew = 1, year = 2015,  material =
  "fiberglass")
# Pred. int. for a new obs
alpha = 0.05
Pred <- predict(fm, z0, interval='prediction', level=1-alpha)  
Pred



