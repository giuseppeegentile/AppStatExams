
setwd("C:/Users/Giuseppe/Documents/.magistrale/secondo anno/AppStat/Exams/2024/20240117/20240117")
data <- read.table("asteroids.txt",header=T)
plot(data,pch=19)

head(data)
dim(data)

type <- data$Type
data <- data[, 1:dim(data)[2]-1]
boxplot(scale(x = data, center = T, scale = F), las = 2, col = 'gold')

# we must scale variables, since surfarea and convvol have higher variances than
# other features
# due to different scale of the unit of measure










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


pc.data <- princomp(data.sd, scores = T)
pc.data
summary(pc.data)
# to explain the 88% of the variance we can take only two components

# Loadings
load.data <- pc.data$loadings
load.data

# Graphical representation of loadings of the first 3 PC
par(mar = c(2,2,2,1), mfrow=c(2,1))
for(i in 1:2)barplot(load.data[,i], ylim = c(-1, 1), main=paste('Loadings PC ',i,sep=''))

# first loading is a weighted average for all the features except for Eccentricity 
# and roundness. We can say that is representing the geometrical properties of the 
# asteroid in terms of tangible quantities.
# For component 2 is a contrast between how elongated and eccentric (more ellipsoidal like )is 
# and how round is (more spherical like, also penalizing the min axes, so if lower, the higher 
# the loading).



scores.data <- pc.data$scores
par(mfrow = c(1, 1))
plot(scores.data[, 1:2], pch=19, col=type)

abline(h=0, v=0, col='black')



n <- dim(data)[1]

colors <- c("red", "green", "blue")
all_possible_labels <- c(unique(type))
data.label <- factor(type, levels = all_possible_labels)

col.lab1 <- rep(NA, n)
for(i in 1:n)
  col.lab1[i] <- colors[which(type[i] == levels(as.factor(type)))]

plot(scores.data[, 1:2], col = col.lab1, pch = 19)
abline(h = min(scores.data[,2]), v = min(scores.data[,1]), col = 1)
points(scores.data[, 1], rep(min(scores.data[,2]), n), col = col.lab1, pch = 19)
points(rep(min(scores.data[,1]), n), scores.data[, 2], col = col.lab1, pch = 19)
abline(h = 0, v = 0, lty = 2, col = 'grey')
legend('topright', levels(data.label), fill = colors, bty = 'n')

# We can clearly see that carbonaceous have low PC1, silicate moderate and metallic very high

#c)
D <- scores.data[which(type=="metallic"), 1:2]

result =mvn(D <- scores.data[which(type=="metallic"), 1:2])
result$multivariateNormality
# We get a low pvalu -> can reject at 5% but not at 1%
# -> can't reject normality

n <- dim(D)[1]
p <- dim(D)[2]

# Confidence region for true mean difference with confidence level 95%
{
  D.mean = colMeans(D)
  D.cov  = cov(D)
  alpha   <- .05
  cfr.fisher <- ((n - 1) * p / (n - p)) * qf(1 - alpha, p, n - p)
  plot(D, asp=1, pch=19)
  ellipse(center=D.mean, shape=D.cov/n, radius=sqrt(cfr.fisher), lwd=2)
  
}

# Confidence region characterization
{
  # Center:
  D.mean
  
  # Directions of the principal axes:
  eigen(D.cov/n)$vectors
  
  # Length of the semi-axes of the ellipse:
  r <- sqrt(cfr.fisher)
  r*sqrt(eigen(D.cov/n)$values)
}





