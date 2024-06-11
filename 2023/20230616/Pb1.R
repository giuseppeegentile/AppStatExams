library(MVN)
library(car)
library(heplots)

data <- read.table("noise.txt", header=TRUE)
features <- data[,3]

names(data)
noise <- data$noise

fuel <- factor(data$fuel)
category<-factor(data$category)


fuelCategory <- factor(paste(fuel,category,sep=''))
# two way ANOVA
# complete model
fit <- aov(noise ~ fuel + category + fuel:category )
summary(fit)

#Verify assumptions
# normality
ps <- c(shapiro.test(features[fuelCategory == levels(fuelCategory)[1]])$p,
       shapiro.test(features[fuelCategory == levels(fuelCategory)[2]])$p,
       shapiro.test(features[fuelCategory == levels(fuelCategory)[3]])$p,
       shapiro.test(features[fuelCategory == levels(fuelCategory)[4]])$p,
       shapiro.test(features[fuelCategory == levels(fuelCategory)[5]])$p,
       shapiro.test(features[fuelCategory == levels(fuelCategory)[6]])$p)
ps
# normality assumption verified
bartlett.test(features, fuelCategory)

Ss <- c(var(features[fuelCategory == levels(fuelCategory)[1]]),
        var(features[fuelCategory == levels(fuelCategory)[2]]),
        var(features[fuelCategory == levels(fuelCategory)[3]]),
        var(features[fuelCategory == levels(fuelCategory)[4]]),
        var(features[fuelCategory == levels(fuelCategory)[5]]),
        var(features[fuelCategory == levels(fuelCategory)[6]]))
Ss
# i can assume also same covariances

# reduced model
summary(fit)
# interaction does not have significance
fit2 <- aov(noise ~ fuel + category)
summary(fit2)
# also category is not statistically significant
fit3 <- aov(noise ~ fuel)
summary(fit3)
# coefficients of the model
fit3$coefficients
(fit3$residuals %*% fit3$residuals)/fit3$df.residual

# Bonferroni
g <- 3
k <- g*(g-1)/2
alpha <- 0.05
n <- length(fuel)
ng <- table(fuel)
treat <-levels(fuel)

Mediag <- tapply(noise,fuel,mean)
SSres <- sum(residuals(fit3) ^ 2)
S <- SSres / (n - g)



ICrange=NULL
for(i in 1:(g-1)) {
  for(j in (i+1):g) {
    print(paste(treat[i],"-",treat[j]))        
    print(as.numeric(c(Mediag[i]-Mediag[j] - qt(1-alpha/(2*k), n-g) * sqrt(S * ( 1/ng[i] + 1/ng[j])),
                       Mediag[i]-Mediag[j] + qt(1-alpha/(2*k), n-g) * sqrt(S * ( 1/ng[i] + 1/ng[j])))))
    ICrange=rbind(ICrange,as.numeric(c(Mediag[i]-Mediag[j] - qt(1-alpha/(2*k), n-g) * sqrt(S * (1/ng[i] + 1/ng[j])),
                                       Mediag[i]-Mediag[j] + qt(1-alpha/(2*k), n-g) * sqrt(S * (1/ng[i] + 1/ng[j])))))
  }}

# i notice a difference only between diesel and ethanol and ethanol and gasoline
# i should consider only 2 groups (diesel and gasoline) vs ethanol
