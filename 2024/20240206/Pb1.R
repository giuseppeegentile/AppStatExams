###-------------------------------------------------------------------###
### Problem 1: Dietary habits and physical health outcomes (20240206) ###
###-------------------------------------------------------------------###


rm(list = ls())
graphics.off()

options(rgl.printRglwidget = TRUE)

library(MVN)
library(car)
library(heplots)

data <- read.table('diet.txt', header = T)
head(data)

Veg <- factor(data$vegetarian, labels=c('F','T')) # Treat.1
Vit <- factor(data$vitamin, labels=c('F','T')) # Treat.2

VegVit <- factor(paste(Veg, Vit, sep=''))
VegVit

data.new  <- data[, 1:2]


# a) ----------------------------------------------------------------------

fit <- manova(as.matrix(data.new) ~ Veg + Vit + Veg:Vit)
summary.manova(fit)

# Dietary habits have a significant effect on health indicators


# b) ----------------------------------------------------------------------

Ps <- c(mvn(data.new[VegVit==levels(VegVit)[1],],)$multivariateNormality$`p value`,
        mvn(data.new[VegVit==levels(VegVit)[2],],)$multivariateNormality$`p value`,
        mvn(data.new[VegVit==levels(VegVit)[3],],)$multivariateNormality$`p value`,
        mvn(data.new[VegVit==levels(VegVit)[4],],)$multivariateNormality$`p value`)
print("Gaussianity tests p-values's"); Ps

S1 <-  cov(data.new[VegVit==levels(VegVit)[1],])
S2 <-  cov(data.new[VegVit==levels(VegVit)[2],])
S3 <-  cov(data.new[VegVit==levels(VegVit)[3],])
S4 <-  cov(data.new[VegVit==levels(VegVit)[4],])

S <- c(S1, S2, S3, S4)
S <- array(S, dim = c(2, 2, 4))

for (i in 1:4) 
{
  cat("\n")
  print(paste("Cov.", levels(VegVit)[i]))
  print(S[, , i])
}

par(mfrow=c(1,4))
image(S1, col=heat.colors(100),main='Cov. S1', asp=1, axes = FALSE, breaks = quantile(rbind(S1,S2,S3,S4), (0:100)/100, na.rm=TRUE))
image(S2, col=heat.colors(100),main='Cov. S2', asp=1, axes = FALSE, breaks = quantile(rbind(S1,S2,S3,S4), (0:100)/100, na.rm=TRUE))
image(S3, col=heat.colors(100),main='Cov. S3', asp=1, axes = FALSE, breaks = quantile(rbind(S1,S2,S3,S4), (0:100)/100, na.rm=TRUE))
image(S4, col=heat.colors(100),main='Cov. S4', asp=1, axes = FALSE, breaks = quantile(rbind(S1,S2,S3,S4), (0:100)/100, na.rm=TRUE))

# Gaussianity is respected and covariances are similar


# c) ----------------------------------------------------------------------

# Both the diets have a significant effect on the mean (but not
# their interaction, that we could remove)

fit2 <- manova(as.matrix(data.new) ~ Veg + Vit)
summary.manova(fit2)


# d) ----------------------------------------------------------------------

summary.aov(fit2)

# Vitamin intake affects the pressure; 
# vegetarian diet affects the cholesterol

alpha <- 0.05
g <- 2
b <- 2
p <- 2
table(VegVit) # balanced design
n <- 50
N <- n*g*b 

W <- summary.manova(fit2)$SS$Residuals

k <- p*g*(g-1)/2 + p*b*(b-1)/2
k

qT <- qt(1 - alpha/(2*k), g*b*n-g-b+1)

mVegF  <- sapply(data.new[Veg=='F',],mean)
mVegT  <- sapply(data.new[Veg=='T',],mean)
diffMeanVeg <- mVegT-mVegF
infVeg <- diffMeanVeg - qT * sqrt(diag(W)/(g*b*n-g-b+1) * (1/(N/2)+1/(N/2)))
supVeg <- diffMeanVeg + qT * sqrt(diag(W)/(g*b*n-g-b+1) * (1/(N/2)+1/(N/2)))

mVitF  <- sapply(data.new[Vit=='F',],mean)
mVitT  <- sapply(data.new[Vit=='T',],mean)
diffMeanVit <- mVitT-mVitF
infVit <- diffMeanVit - qT * sqrt(diag(W)/(g*b*n-g-b+1) * (1/(N/2)+1/(N/2)))
supVit <- diffMeanVit + qT * sqrt(diag(W)/(g*b*n-g-b+1) * (1/(N/2)+1/(N/2)))

IC2   <- list(VegT_VegF=cbind(infVeg,diffMeanVeg, supVeg), VitT_VitF=cbind(infVit, diffMeanVit, supVit))
IC2

# Vitamin intake diminishes pressure;
# Vegetarian diet decreases cholesterol


# Extra -------------------------------------------------------------------

### Graphical exploration of the data
# effect of the treatments + their interaction on the first variable
layout(matrix(c(1,1,2,3), 2, byrow=T))
boxplot(data.new[,1] ~ VegVit, main='Model with Interac. Vegetarian+Vitamin (Pressure)',
        ylab='Pr', col='grey95')
boxplot(data.new[,1] ~ Veg, main='Only Factor Vegetarian', ylab='Pr', col=c('red','blue'))
boxplot(data.new[,1] ~ Vit, main='Only Factor Vitamin', ylab='Pr', col=c('forestgreen','gold'))

# effect of the treatments + their interaction on the second variable
layout(matrix(c(1,1,2,3), 2, byrow=T))
boxplot(data.new[,2] ~ VegVit, main='Model with Interac. Vegetarian+Vitamin (Cholesterol)',
        ylab='Ch', col='grey95')
boxplot(data.new[,2] ~ Veg, main='Only Factor Vegetarian', ylab='Ch', col=c('red','blue'))
boxplot(data.new[,2] ~ Vit, main='Only Factor Vitamin', ylab='Ch', col=c('forestgreen','gold'))

### Plots per treatment levels
pairs(data.new[VegVit==levels(VegVit)[1],])
pairs(data.new[VegVit==levels(VegVit)[2],])
pairs(data.new[VegVit==levels(VegVit)[3],])
pairs(data.new[VegVit==levels(VegVit)[4],])
