setwd("C:/Users/Giuseppe/Documents/.magistrale/secondo anno/AppStat/Exams/2021/20210618/20210618")
library(mvtnorm) # to deal with multivariate normal distributions
library(car) # "Companion to Applied Regression" for regression analysis


data <- read.table('beans.txt', header=T)

head(data)
dim(data)

n <- dim(data)[1]
p <- dim(data)[2]

# if there are labels (two in this case), skip otherwise
{
  # store them and remove from data
  data.label <- data[, dim(data)[2]]
  data       <- data[, -dim(data)[2]]
}


# see if the variances are different
{
  par(mfrow = c(1,1))
  boxplot(data, las = 2, col = 'gold')
  ## centred boxplot
  boxplot(scale(x = data, center = T, scale = F), las = 2, col = 'gold')
  # Area and covex area variability are masking all the others features
  # due to unit of measure of surfaces (r^2 increase much more the uncertainty wrt to r)
}

{
  data.sd <- scale(data)
  data.sd <- data.frame(data.sd)
  head(data.sd)
  # Boxplot
  par(mfrow = c(1, 1))
  boxplot(data.sd, las = 2, col = 'gold')
  # now every box has similar height
  # you can see in the next plot how every feature has variance 1
  barplot(sapply(data.sd, sd)^2, las = 2, main = 'Standardized Variables', ylim = c(0, 7),
          ylab = 'Variances')
}

# PCA
{ 
  pc.data <- princomp(data.sd, scores = T)
  pc.data
  summary(pc.data)
}
# with only two components we explain 89% of the variability

# Loadings
{
  load.data <- pc.data$loadings
  
  
  # Graphical representation of loadings of the first "chosen_pc" PC
  chosen_pc = 2
  par(mar = c(2,2,2,1), mfrow=c(chosen_pc,1))
  for(i in 1:chosen_pc)barplot(load.data[,i], ylim = c(-1, 1), main=paste('Loadings PC ',i,sep=''))
  
  # interpretation: say that the second component has high loadings for 4/5 stars
  #                                                and negative loading for 1/2 stars
  #                 -> an high second component indicates high flow(night spent) for expensive solutions 
  #                 -> a low second component indicates high flow(night spent) for cheap solutions
  # go to CASE 1 for further interpretations
  
  # most of the variability is captured by a weighted average of Area, perimeter, axislengths and diameter
  # eccentricity and roundness are not considered as important as others components for the first PC
  # -> is favoring big beans, regardless of their shape (eccentricity and roundness)
  
  # second component is already capturing what wasn't captured  by the first component
  # can be seen as a contrast between "more edge shapes" and softer characteristic of the bean
  # , penalizing the roundness and minor axis
  # we can imaging a bean with high second component as very tall but thin (extremely high maj_ax / min_ax)
  # that looks like a conic shape
  # a bean with low second PC is a complete spherical bean
  
}

# Project the dataset onto the "k" principal components
{
  # This is the projection
  scores.data <- pc.data$scores
  
  # This is the plot of the projection keeping only the first 2 components
  # Projection of the data on the first two principal components. 
  # (x for PC1 and y for PC2)
  par(mfrow = c(1, 1))
  plot(scores.data[, 1:2], pch=19)
  abline(h=0, v=0, col='black')
}


biplot(pc.data)




# CASE 2: only a label provided
{
  value_scores = data.frame(scores.data)
  # Assign a color to each possible label
  colors <- c("red","green","black")
  all_possible_labels <- c(unique(data.label))
  data.label <- factor(data.label, levels = all_possible_labels)
  
  col.lab1 <- rep(NA, n)
  for(i in 1:n)
    col.lab1[i] <- colors[which(data.label[i] == levels(data.label))]
  
  plot(value_scores[, 1:2], col = col.lab1, pch = 19)
  abline(h = min(value_scores[,2]), v = min(value_scores[,1]), col = 1)
  points(value_scores[, 1], rep(min(value_scores[,2]), n), col = col.lab1, pch = 19)
  points(rep(min(value_scores[,1]), n), value_scores[, 2], col = col.lab1, pch = 19)
  abline(h = 0, v = 0, lty = 2, col = 'grey')
  legend('topright', levels(data.label), fill = colors, bty = 'n')
}


# adzuki are the biggest species of beans, cannellini the smallest and black-eyed a compromise
# adzuki have more edgy aspect, black-eyed more spherical, while cannellini seems both.



# Project the dataset onto the 2 principal components
{
  # This is the projection
  scores.data <- pc.data$scores
}
scores.data <- scores.data[, 1:2]
cannellini <- scores.data[which(data.label == "cannellini"), ]
head(cannellini)

D <- cannellini
n <- dim(D)[1]
p <- dim(D)[2]
library(MVN)
# Assumption: normality for the new dataset (not for the old one!)
result <- mvn(D)
result$multivariateNormality
# we don't have normality
# T2 Hotelling Test H0:  delta.0 = (0,0)
{
  D.mean   <- colMeans(D) 
  D.cov    <- cov(D)
  D.invcov <- solve(D.cov)
  
  alpha   <- .05
  delta.0 <- c(0, 0)
  
  D.T2 <- n * (D.mean - delta.0) %*% D.invcov %*% (D.mean - delta.0)
  D.T2
  cfr.fisher <- ((n - 1) * p / (n - p)) * qf(1 - alpha, p, n - p)
  cfr.fisher
  
  D.T2 < cfr.fisher # FALSE: we reject H0 at level alpha -> the two labs measure differently
  
  P <- 1 - pf(D.T2 * (n - p) / (p * (n - 1)), p, n - p)
  P
  # low -> reject
}



# Confidence region for true mean difference with confidence level 95%
{
  alpha   <- .05
  cfr.fisher <- ((n - 1) * p / (n - p)) * qf(1 - alpha, p, n - p)
  plot(D, asp=1, pch=19, main='Dataset of the Differences')
  ellipse(center=D.mean, shape=D.cov/n, radius=sqrt(cfr.fisher), lwd=2)
}

# RR for the mean (ellipsoidal region) 
# { m \in R^p t.c. n * (m-x_bar)' %*% (x.cov)^-1 %*% (m-x_bar) > cfr.fisher }
# Center:
D.mean

# Directions of the principal axes:
eigen(D.cov/n)$vectors

# Length of the semi-axes of the ellipse:
r <- sqrt(cfr.fisher)
r*sqrt(eigen(D.cov/n)$values)
