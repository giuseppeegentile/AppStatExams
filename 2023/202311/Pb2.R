setwd("C:/Users/Giuseppe/Documents/.magistrale/secondo anno/AppStat/Exams/2023/20231107/20231107")
options(rgl.printRglwidget = TRUE)


data <- read.table('gemstones.txt', header=T)

head(data)
dim(data)

n <- dim(data)[1]
p <- dim(data)[2]-1

# if there are labels (two in this case), skip otherwise
{
  # case of single label as last column
  data.label <- data[, dim(data)[2]]
  data       <- data[, -dim(data)[2]]
  
}

# see if the variances are different
{
  par(mfrow = c(1,1))
  boxplot(data, las = 2, col = 'gold')
  ## centred boxplot
  boxplot(scale(x = data, center = T, scale = F), las = 2, col = 'gold')
}
# for sure standardize

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

# Loadings
{
  load.data <- pc.data$loadings
  
  
  # Graphical representation of loadings of the first 3 PC
  par(mar = c(2,2,2,1), mfrow=c(2,1))
  for(i in 1:2)barplot(load.data[,i], ylim = c(-1, 1), main=paste('Loadings PC ',i,sep=''))
  
  # already done in another tde
}
par(mfrow=c(1,1))
scores.data <- pc.data$scores
biplot(pc.data)


value_scores = data.frame(scores.data)
# Assign a color to each possible label
colors <- c("green","blue","red")
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
# sapphire smaller, ruby in the middle, emerald bigger
# same interpretations of the bean exercise




# Confidence region for PC dataset of a subset given by label
# Project the dataset onto the 2 principal components
{
  library(MVN)
  # Project
  scores.data <- pc.data$scores
  scores.data <- scores.data[, 1:2]
  subset_asked <- scores.data[which(data.label == "ruby"), ]
  head(subset_asked)
  
  D <- data.frame(subset_asked)
}


# Confidence region for true mean difference with confidence level 95%
{
  D.mean   <- sapply(D, mean) 
  D.cov    <- cov(D)
  D.invcov <- solve(D.cov)
  
  alpha   <- .05
  delta.0 <- c(0, 0)
  
  
  alpha   <- .05
  cfr.fisher <- ((n - 1) * p / (n - p)) * qf(1 - alpha, p, n - p)
  plot(D, asp=1, pch=19, main='Dataset')
  ellipse(center=D.mean, shape=D.cov/n, radius=sqrt(cfr.fisher), lwd=2)
  
  points(delta.0[1], delta.0[2], pch=16, col='grey35', cex=1.5)
  abline(h=delta.0[1], v=delta.0[2], col='grey35')
  
}

mvn(D) # no normality





