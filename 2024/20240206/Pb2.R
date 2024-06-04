###----------------------------------------------------------###
### Problem 2: Quality control in food production (20240206) ###
###----------------------------------------------------------###


rm(list = ls())
graphics.off()

options(rgl.printRglwidget = TRUE)

library(MVN)
library(MASS)

data <- read.table('food.txt', header = T)
head(data)

hl <- factor(data$result)
col.lab <- ifelse(hl %in% c("high"), 'red', 'blue')

plot(data[,1:4], col = col.lab, pch=20)

high <- data[which(data$result == "high"), 1:4]
low <- data[which(data$result == "low"), 1:4]


# a) ----------------------------------------------------------------------

mvn(high)$multivariateNormality
mvn(low)$multivariateNormality

# Normality is not respected for the "low" group

var.test(high[, 1], low[, 1])
var.test(high[, 2], low[, 2])
var.test(high[, 3], low[, 3])
var.test(high[, 4], low[, 4])

# On the diagonal, we have no problems

hCov <- cov(high)
lCov <- cov(low)

list(S1 = hCov, S2 = lCov)

# Using the rule of thumb, the homogeneity assumption seems to hold

# -> LDA


# b) ----------------------------------------------------------------------

c.hl <- 100000
c.lh <- 500

pl <- 0.001
ph <- 1-0.001
prior = c(ph, pl)
prior

prior.c <- c(ph*c.lh/(c.hl*pl+c.lh*ph), pl*c.hl/(c.hl*pl+c.lh*ph))
prior.c

lda <- lda(data[, 1:4], hl, prior = prior.c)
lda

lda.pred <- predict(lda, data[, 1:4])

G <- 2
misc <- table(class.true = hl, class.assigned = lda.pred$class)
APER <- 0
for(g in 1:G)
  APER <- APER + sum(misc[g,-g])/sum(misc[g,]) * prior.c[g]

print(paste("APER:", APER))

errors_CV <- 0
miscCV <- cbind(0, 0, 0, 0)
colnames(miscCV) <- c("TN", "FN", "FP", "TP")
for(i in 1:60)
{
  ldaCV.i <- lda(data[-i, 1:4], hl[-i], prior = prior.c)
  errors_CV <- errors_CV + as.numeric(predict(ldaCV.i, data[i, 1:4])$class != hl[i])
  for(j in 1:2)
    for(k in 1:2)
      if (predict(ldaCV.i, data[i, 1:4])$class == levels(hl)[j] && hl[i] == levels(hl)[k]) 
        miscCV[j*k+as.numeric(j == 2 && k == 1)] <- miscCV[j*k+as.numeric(j == 2 && k == 1)] + 1
}

AERCV <- sum(errors_CV)/length(hl)
print(paste("AERCV:", AERCV))

# APER is 0.1445, while AERCV is 0.3
# We don't have enough observations, therefore the estimates of our parameters are a bit rough
# and our classifier performs badly on unseen data


# c) ----------------------------------------------------------------------

total_products <- 1000
TN <- miscCV[which(colnames(miscCV) == "TN")]
FN <- miscCV[which(colnames(miscCV) == "FN")]
FP <- miscCV[which(colnames(miscCV) == "FP")]
TP <- miscCV[which(colnames(miscCV) == "TP")]

rbind("Budget for the laboratory test", ((FP / (TN + FP)) * total_products * ph + (TP / (TP + FN)) * total_products * pl) * c.lh)

# I intended as: "how much money do we need to have in order to perform the lab tests?"
# Instead, if the meaning was: "what will be the average economic loss?", I would have removed the TP part


# d) ----------------------------------------------------------------------

prev_strategy_cost <- total_products * c.lh
cur_strategy_cost <- (FP / (TN + FP)) * total_products * ph * c.lh + (FN / (TP + FN)) * total_products * pl * c.hl

rbind("Savings", prev_strategy_cost - cur_strategy_cost)


# Extra -------------------------------------------------------------------

# Equivalent code
errors_CV <- 0
TN <- 0
FN <- 0
FP <- 0
TP <- 0
for(i in 1:60)
{
  ldaCV.i <- lda(data[-i, 1:4], hl[-i], prior = prior.c)
  errors_CV <- errors_CV + as.numeric(predict(ldaCV.i, data[i, 1:4])$class != hl[i])
  if (predict(ldaCV.i, data[i, 1:4])$class == "high" && hl[i] == "high") 
    TN <- TN + 1
  if (predict(ldaCV.i, data[i, 1:4])$class == "high" && hl[i] == "low") 
    FN <- FN + 1
  if (predict(ldaCV.i, data[i, 1:4])$class == "low" && hl[i] == "high") 
    FP <- FP + 1
  if (predict(ldaCV.i, data[i, 1:4])$class == "low" && hl[i] == "low") 
    TP <- TP + 1
}