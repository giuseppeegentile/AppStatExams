library(MVN)
library(car)
library(heplots)

data <- read.table("diet.txt", header=TRUE)

attach(data)

features <- data[,1:2]
vitamin <- factor(data$vitamin, labels=c('FALSE', 'TRUE'))
vegetarian <- factor(data$vegetarian, labels=c('FALSE', 'TRUE'))

VitVeg <- factor(paste(vitamin,vegetarian, sep=' '))

dim(features[VitVeg==levels(VitVeg)[1],])
dim(features[VitVeg==levels(VitVeg)[2],])
dim(features[VitVeg==levels(VitVeg)[3],])
dim(features[VitVeg==levels(VitVeg)[4],])

# fit the complete model with interaction
fit1 <- manova(as.matrix(features) ~ vegetarian + vitamin + vegetarian:vitamin)
summary(fit1)

# the interaction is indicated as not significant -> reduced model with only interaction

fit2 <- manova(as.matrix(features) ~ vegetarian + vitamin)
summary(fit2)

# taken individually the two diets are significants

# Assumptions -> gaussian samples (4 tests, each combination), same covariance
# Graphical Assessment
par(mfrow=c(2,2))
pairs(features[VitVeg==levels(VitVeg)[1],])
pairs(features[VitVeg==levels(VitVeg)[2],])
pairs(features[VitVeg==levels(VitVeg)[3],])
pairs(features[VitVeg==levels(VitVeg)[4],])

ps <- c(mvn(features[VitVeg==levels(VitVeg)[1],])$multivariateNormality$`p value`,
        mvn(features[VitVeg==levels(VitVeg)[2],])$multivariateNormality$`p value`,
        mvn(features[VitVeg==levels(VitVeg)[3],])$multivariateNormality$`p value`,
        mvn(features[VitVeg==levels(VitVeg)[4],])$multivariateNormality$`p value`)
ps
# we can safely accept the gaussianity assumption

S1 <-  cov(features[VitVeg==levels(VitVeg)[1],])
S2 <-  cov(features[VitVeg==levels(VitVeg)[2],])
S3 <-  cov(features[VitVeg==levels(VitVeg)[3],])
S4 <-  cov(features[VitVeg==levels(VitVeg)[4],])

par(mfrow=c(2,2))
image(S1, col=heat.colors(100),main='Cov. S1', asp=1, axes = FALSE, breaks = quantile(rbind(S1,S2,S3,S4), (0:100)/100, na.rm=TRUE))
image(S2, col=heat.colors(100),main='Cov. S2', asp=1, axes = FALSE, breaks = quantile(rbind(S1,S2,S3,S4), (0:100)/100, na.rm=TRUE))
image(S3, col=heat.colors(100),main='Cov. S3', asp=1, axes = FALSE, breaks = quantile(rbind(S1,S2,S3,S4), (0:100)/100, na.rm=TRUE))
image(S4, col=heat.colors(100),main='Cov. S4', asp=1, axes = FALSE, breaks = quantile(rbind(S1,S2,S3,S4), (0:100)/100, na.rm=TRUE))

round(S1, digits=2)
round(S2, digits=2)
round(S3, digits=2)
round(S4, digits=2)

# we cannot be certain about the covariance assumption, however (M)ANOVA has been demonstrated
# to be robust with respect to its assumption, so we proceed with the analysis

summary(fit1)
summary(fit2)
# I would propose the reduced model due to to the fact that the interaction between the treatment
# has been modeled as not significant

summary.aov(fit2)

# Bongerroni CI
alpha <- 0.05
g <- 2
b <- 2
p <- 2
n <- 50
N <- n*g*b 
# number of comparisons
k <- p*g*(g-1)/2 + p*b*(b-1)/2
# t- quantile
qT <- qt(1 - alpha/(2*k), g*b*n-g-b+1)
# covariance within groups
W <- summary.manova(fit2)$SS$Residuals

mVitYES  <- sapply(features[vitamin=='TRUE',],mean)
mVitNO  <- sapply(features[vitamin=='FALSE',],mean)
infVit <- mVitYES - mVitNO -  qT * sqrt(diag(W)/(g*b*n-g-b+1) * (1/100+1/100))
supVit <- mVitYES - mVitNO + qT * sqrt(diag(W)/(g*b*n-g-b+1) * (1/100+1/100))

mVegYES  <- sapply(features[vegetarian=='TRUE',],mean)
mVegNO  <- sapply(features[vegetarian=='FALSE',],mean)
infVeg <- mVegYES - mVegNO - qT * sqrt(diag(W)/(g*b*n-g-b+1) * (1/100+1/100))
supVeg <- mVegYES - mVegNO + qT * sqrt(diag(W)/(g*b*n-g-b+1) * (1/100+1/100))

IC2   <- list(VitYes_VitNo=cbind(infVit, supVit), VegYes_VegNo=cbind(infVeg, supVeg))
IC2

# Vitamins have an effect on pressure ->
# Vegetarian diet has an effect on cholesterol ->

