###-------------------------------------------------------------###
### Problem 2:  Doping in professional cyclist races (20230904) ###
###-------------------------------------------------------------###

rm(list = ls())
graphics.off()

library(MVN)
library(MASS)

data <- read.table('doping.txt', header = T)
head(data)

cd <- factor(data$result)
col.lab <- ifelse(cd %in% c("clean"), 'green', 'red')

plot(data[, 1:4], col = col.lab, pch = 20)

clean <- data[which(data$result == "clean"), 1:4]
doped <- data[which(data$result == "doped"), 1:4]


# a) ----------------------------------------------------------------------

mvn(clean)$multivariateNormality
mvn(doped)$multivariateNormality

# Normality is respected by both the groups

var.test(clean[, 1], doped[, 1])
var.test(clean[, 2], doped[, 2])
var.test(clean[, 3], doped[, 3])
var.test(clean[, 4], doped[, 4])

# We may have problems with the variances of pH and rdensity (not equal)

cCov <- cov(clean)
dCov <- cov(doped)

list(S1 = cCov, S2 = dCov, ratio = abs(cCov/dCov))

# Using the rule of thumb, the homogeneity assumption seems to hold.
# However, as I'm not so sure, I decide to go with QDA


# b) ----------------------------------------------------------------------

c.cd <- 50000
c.dc <- 1000

pd <- 0.01
pc <- 1-0.01
prior = c(pc, pd)
prior

prior.c <- c(pc*c.dc/(c.cd*pd+c.dc*pc), pd*c.cd/(c.cd*pd+c.dc*pc))
prior.c

qdaCV <- qda(data[, 1:4], cd, CV = TRUE, prior = prior.c)

G <- 2
miscCV <- table(class.true = cd, class.assigned = qdaCV$class)
AERCV <- 0
for(g in 1:G)
  AERCV <- AERCV + sum(miscCV[g,-g])/sum(miscCV[g,]) * prior[g] # prior NON adjusted! p(true)p(miscl.|true) + p(false)p(miscl.|false)

print(paste("AERCV:", AERCV))


# c) ----------------------------------------------------------------------

TN <- miscCV[1, 1]
FN <- miscCV[2, 1]
FP <- miscCV[1, 2]
TP <- miscCV[2, 2]

total_cyclists <- 200

rbind("Budget for the blood-based tests", ((FP / (TN + FP)) * total_cyclists * pc + (TP / (TP + FN)) * total_cyclists * pd) * c.dc)


# d) ----------------------------------------------------------------------

prev_strategy_cost <- total_cyclists * c.dc
cur_strategy_cost <- (FP / (TN + FP)) * total_cyclists * pc * c.dc + (FN / (TP + FN)) * total_cyclists * pd * c.cd

rbind("Savings", prev_strategy_cost - cur_strategy_cost)


# Extra -------------------------------------------------------------------

## APER w/ QDA -------------------------------------------------------------

qda <- qda(data[, 1:4], cd, prior = prior.c)
qda

qda.pred <- predict(qda, data[, 1:4])

G <- 2
misc <- table(class.true = cd, class.assigned = qda.pred$class)
APER <- 0
for(g in 1:G)
  APER <- APER + sum(misc[g,-g])/sum(misc[g,]) * prior[g] # prior NON adjusted! p(true)p(miscl.|true) + p(false)p(miscl.|false)

print(paste("APER:", APER))


## LDA ---------------------------------------------------------------------

lda <- lda(data[, 1:4], cd, prior = prior.c)
lda

lda.pred <- predict(lda, data[, 1:4])

G <- 2
misc.LDA <- table(class.true = cd, class.assigned = lda.pred$class)
APER.LDA <- 0
for(g in 1:G)
  APER.LDA <- APER.LDA + sum(misc.LDA[g,-g])/sum(misc.LDA[g,]) * prior[g] # prior NON adjusted! p(true)p(miscl.|true) + p(false)p(miscl.|false)

print(paste("APER:", APER.LDA))

ldaCV <- lda(data[, 1:4], cd, CV = TRUE, prior = prior.c)

G <- 2
miscCV.LDA <- table(class.true = cd, class.assigned = ldaCV$class)
AERCV.LDA <- 0
for(g in 1:G)
  AERCV.LDA <- AERCV.LDA + sum(miscCV.LDA[g,-g])/sum(miscCV.LDA[g,]) * prior[g] # prior NON adjusted! p(true)p(miscl.|true) + p(false)p(miscl.|false)

print(paste("AERCV:", AERCV.LDA))

TN <- miscCV.LDA[1, 1]
FN <- miscCV.LDA[2, 1]
FP <- miscCV.LDA[1, 2]
TP <- miscCV.LDA[2, 2]

total_cyclists <- 200

rbind("Budget for the blood-based tests", ((FP / (TN + FP)) * total_cyclists * pc + (TP / (TP + FN)) * total_cyclists * pd) * c.dc)

prev_strategy_cost <- total_cyclists * c.dc
cur_strategy_cost <- (FP / (TN + FP)) * total_cyclists * pc * c.dc + (FN / (TP + FN)) * total_cyclists * pd * c.cd

rbind("Savings", prev_strategy_cost - cur_strategy_cost)


## QDA vs. LDA -------------------------------------------------------------

list(CM.QDA = misc, CM.QDA.CV = miscCV, CM.LDA = misc.LDA, CM.LDA.CV = miscCV.LDA)
list(APER.QDA = APER, AERCV.QDA = AERCV, APER.LDA = APER.LDA, AERCV.LDA = AERCV.LDA)
