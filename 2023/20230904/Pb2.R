library(MASS)
library(MVN)
library(heplots)
data <- read.table("doping.txt", header=TRUE)
head(data)


result <- factor(data$result)

clean <- which(result=="clean")
doped <- which(result=="doped")
feat <- data[,1:4]

p.d <- 0.01
p.nd <- 1 - p.d

# what i pay if i put a doped in the non doped
c.dnd <- 50000
# what i pay if i put the non doped in the doped
c.ndd <- 1000

priors.orig <- c(p.nd, p.d)

p.d.new <- (p.d * c.dnd) / (p.d*c.dnd + p.nd*c.ndd)
p.nd.new <- (p.nd * c.ndd) / (p.d*c.dnd + p.nd*c.ndd)

priors <- c(p.nd.new, p.d.new)
priors

plot(feat,col=ifelse(result=="clean","green","red"),pch=19)
#LDA vs QDA

# gaussianity assumption
mvn(feat[clean,])$multivariateNormality
mvn(feat[doped,])$multivariateNormality
# gaussianity assumption confirmed

# same covariance between groups
S1 <- cov(feat[clean,])
S2 <- cov(feat[doped,])

boxM(feat, result)

# we can assume same covariance structure

# LDA
fit <- manova(as.matrix(feat) ~ result)
summary(fit)
# we do not expect a good result from the classifier

lda.cv <- lda(feat, result, prior=priors, CV=TRUE)

table(class.true=result, class.assignedCV=lda.cv$class)

G <- 2
misc <- table(class.true=result, class.assigned=lda.cv$class)
APER.CV <- 0
for(g in 1:G)
   APER.CV <- APER.CV + sum(misc[g,-g])/sum(misc[g,]) * priors.orig[g]

APER.CV

lda.normal <- lda(feat, result, prior=priors)

table(class.true=result, class.assigned=(lda.normal)$class)


# new 2000 observation.
new.obs <- 2000

budget <- APER.CV * p.d*new.obs * c.dnd + APER.CV *p.nd*new.obs*c.ndd
budget

oldbudget <- 1000*new.obs
oldbudget

oldbudget - budget


