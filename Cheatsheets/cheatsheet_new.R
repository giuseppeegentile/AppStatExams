###----------------###
### LINEAR ALGEBRA ###----------------------------------------------------------
###----------------###

#### Vectors ----

v <- c(2, 3, 5, 4)
v <- seq(2, 5, len = 4)
v <- seq(2, 5, by = 1)
v <- 2:5
v

z <- rep(c(1, 2, 3), times = 2, each = 3)
z <- rep(c(1, 2, 3), length.out = 18, each = 3)
z

#### Matrices ----

W <- rbind(c(11, 13, 15), c(12, 14, 16))
W <- cbind(c(11, 12), c(13, 14), c(15, 16))
W <- matrix(data = c(11, 12, 13, 14, 15, 16), nrow = 2, ncol = 3, byrow = F)
W <- matrix(c(11, 12, 13, 14, 15, 16), 2, 3)
W

#### Operations ----

v^2 
exp(v)
sum(v) 
prod(v)
sqrt(v)
abs(v)
min(v)
max(v)
sin(v)
cos(v)
tan(v)

mean(v)
var(v)

W + W
W * W

t(W)
W %*% t(W)
t(W) %*% W

v %*% v
t(v) %*% v
rbind(v) %*% v
rbind(v) %*% cbind(v)

W + 2:4 # RECYCLING!

# Inverse of a matrix (square and invertible)
A <- matrix(c(11, 13, 12, 14), ncol = 2, nrow = 2, byrow = TRUE)
det(A)
solve(A)


###--------------------###
### DATA PREPROCESSING ###------------------------------------------------------
###--------------------###

rm(list = ls())

data <- read.table('data.txt', header = T)
head(data)
dim(data)

n <- dim(data)[1]
p <- dim(data)[2]

colnames(data)
length(unique(data$feature))
typeof(data$feature)
is(data$feature)

table(data$feature)
range(data$feature)

data$feature <- ifelse(data$feature == "Yes", "Sì", "No")
data$feature <- factor(data$feature, labels = c('F','T'))
# (If you want to specify the "labels" argument, pay attention to write the labels
# in the alphabetical order of the original ones in order to get the correct mapping)

F1 <- factor(data$feature1, labels=c('F','T'))
F2 <- factor(data$feature2, labels=c('F','T'))
F1F2 <- factor(paste(F1, F2, sep=''))

data.label <- data[, "feature_name"]
data <- data[, !colnames(data) %in% "feature_name"]

factors <- c("feature1_name", "feature2_name")
data.label <- data[, colnames(data) %in% factors]
data <- data[, !colnames(data) %in% factors]

data.label <- data[, 7]
data <- data[, -7]
data <- data[, 1:6]

data <- data[, -c(6, 9)]
data <- data[, c(1:5, 7:8, 10)]


###--------------------###
### DATA VISUALIZATION ###------------------------------------------------------
###--------------------###

x11()
dev.off()

graphics.off()
par(mfrow=c(1,1))


#### Multivariate Data ----

options(rgl.printRglwidget = TRUE) # library(rgl) is useful for 3D plots

plot(data)
plot(data[, "feature1_name"], data[, "feature2_name"])
points(x = 100, y = 80, pch = 19, cex = 2, col = 'red')

abline(h = 80, lty = 2, col = 'grey')
abline(v = 100, lty = 2, col = 'grey')
abline(a = 20, b = (80-20) / 100, col = 'blue')
lines(c(95, 95), c(60, 100), col='blue', pch=16, type='b')
# Note: for "lines", the first argument are all the x-coords, while the 2nd all the y-coords
# (Do not confuse the first argument with the first point and the 2nd w/ the 2nd)
segments(105, 60, 105, 100, lwd = 3, col = 'green')
# Here, with "segments", we specify, in order: x1, y1, x2, y2

ellipse(center = c(100, 80), shape = cbind(c(9, 3), c(3, 3)), radius = 8, lwd = 2) # !library(car)
dataEllipse(data[, 1], data[, 2], levels = 0.9, add = TRUE)

legend('topleft', c('line', 'point', 'ellipse'),
       col = c('blue','red','blue'), lty = c(1, 1, 1), lwd = c(1, 2, 2))

plot(data[, 1], data[, 2], type = 'n')
text(data[, 1], data[, 2], dimnames(data)[[1]], cex = 0.7)

pairs(data)

boxplot(data, col = 'gold')
boxplot(scale(x = data, center = T, scale = T), las = 2, col = 'gold')
boxplot(data[, "feature"] ~ data.label, col = c('black', 'white'))

matplot(t(data), type = 'l')

color.lab <- ifelse(data.label == 'T', 'red', 'blue')
color.lab <- rainbow(length(levels(data.label)))

layout(cbind(c(1, 1), c(2, 3)), widths = c(2, 1), heights = c(1, 1))
plot(data[, "feature1_name"], data[, "feature2_name"], 
     xlab = "x1", ylab = "x2", 
     xlim = c(-5, 5), ylim = c(-10, 120),
     col = color.lab,
     asp = 1, pch = 16, cex = 1.5)
# If "asp" (aspect ratio) is specified (e.g. asp = 1), then "xlim" (or "ylim") is ignored

hist(data[, "feature1_name"], 
     main = "Histogram of 'feature1'",
     prob = T)
hist(data[, "feature2_name"])

par(mfrow=c(2,2))
# Equivalent to: layout(cbind(c(1, 3), c(2, 4)), widths = c(1, 1), heights = c(1, 1))


#### Categorical Data ----

pie(table(data.label), col = color.lab)

barplot(table(data.label) / length(data.label))


#### Save Plots ----

##### Single PDF File -----

pdf(file = "myplots.pdf", onefile = T)
plot(data[, 3], col = 'red', pch = 8, cex = 1.5)
plot(data[, 3], col = 'green', pch = 19, asp = 10)
plot(data[, 3], col = 'blue', pch = 11, cex = 3)
dev.off()


##### Single PNG File -----

plot_configs <- list(
  list(col = 'red', pch = 8, cex = 1.5),
  list(col = 'green', pch = 19, asp = 10),
  list(col = 'blue', pch = 11, cex = 3)
)

for (i in seq_along(plot_configs)) {
  png(file = paste0("plot", i, ".png"))
  do.call(plot, c(list(data[, 3]), plot_configs[[i]]))
  dev.off()
}

library(magick)

concatenate_images_vertically <- function(image_paths) {
  images <- lapply(image_paths, image_read)
  combined_image <- image_append(image = do.call(c, images), stack = TRUE)
  return(combined_image)
}

image_paths <- c('plot1.png', 'plot2.png', 'plot3.png')
combined_image <- concatenate_images_vertically(image_paths)

image_write(combined_image, path = 'myplots.png')


###-------------------------------###
### ANALYSIS OF QUANTITATIVE DATA ###-------------------------------------------
###-------------------------------###

colMeans(data)

sapply(data, mean)
sapply(data, sd)
sapply(data, var)

tapply(data[, 1], F1F2, mean)
tapply(data[, 2], F1F2, mean)

cov(data)
cor(data)

#### Various Aspects of Variance ----

covariance <- matrix(c(0, 0, 0, 0), nrow = 2, ncol = 2)
SS <- 0

for(i in 1:dim(data)[1])
{
  covariance <- covariance + as.numeric(data[i, ] - mean) %*% t(as.numeric(data[i, ] - mean))
  SS <- SS + t(as.numeric(data[i, ] - mean)) %*% as.numeric(data[i, ] - mean)
}

covariance <- covariance / (dim(data)[1] - 1)

data_centered <- data
data_centered[, 1] <- data[, 1] - mean[1]
data_centered[, 2] <- data[, 2] - mean[2]

SS_variant <- sum(data_centered^2)

covariance
cov(data)

SS
SS_variant

sapply(data, var)


###-----------------------###
### GAUSSIAN DISTRIBUTION ###-------------------------------------------
###-----------------------###

#### Univariate Case ----

##### Probability Density Function (pdf) -----

dnorm(0)                      # density function at 0 for a distribution N(0,1)
dnorm(0, mean = 1, sd = 2)    # density function at 0 for a distribution N(1,4)


##### Cumulative Distribution Function (cdf) -----

pnorm(0)       # P(Z<0), with Z ~ N(0,1)
pnorm(0, 1, 2) # P(X<0), with X ~ N(1,4)


##### Quantiles (inverse of cdf) -----

qnorm(0.95)        #  = z s.t.  P(Z<z)=0.95, with Z ~ N(0,1)
qnorm(0.95, 1, 2)  #  = z s.t.  P(Z<z)=0.95, with Z ~ N(1,4)


##### Random Generation -----

set.seed(8321)
rnorm(10)       # X_1,..,X_10 ~ N(0,1) i.i.d.
rnorm(10, 1, 2) # X_1,..,X_10 ~ N(1,4) i.i.d.


##### Visualization -----

par(mfrow=c(3,1))

s <- seq(-2, 2, by = 0.01)
plot(s, dnorm(s, 0, 1), main = 'Gaussian N(0,1)', type = 'l', ylim = c(0, 1))
plot(s, pnorm(s, 0, 1), main = 'Gaussian N(0,1)', type = 'l', ylim = c(0, 1))

w <- seq(0, 1, by = 0.01)
plot(w, qnorm(w, 0, 1), main = 'Gaussian N(0,1)', type = 'l')

par(mfrow=c(1,2))

set.seed(8321)
z <- rnorm(n = 1000, 0, 1)
plot(z, main = 'Gaussian N(0,1)')
hist(z, main = '', col = 'grey', xlab = 'x', prob = T, ylim = c(0,.45))
lines(seq(-4, 4, length = 100), dnorm(seq(-4, 4, length = 100)), col = 'blue', lty = 2, lwd = 2)
box()


##### QQPlot -----

par(mfrow=c(1,1))

qqplot(qnorm((1:1000/1000-1/2000)), z, xlab = 'Theoretical quantile N(0,1)',
       ylab = 'Sample quantile', asp =1, ylim = c(-5,5)*2, main  ='N(0,1)')
abline(0, 1)
qqline(z, col = 'red')
# If the data are Gaussian, the slope of the qqline is an estimate of 
# the standard deviation, the intercept is an estimate of the mean


##### Tests of Gaussianity -----
# H0: X ~ N     vs    H1=H0^c

shapiro.test(z)


#### Multivariate Case ----

##### Random Generation -----

set.seed(20230320)
X <- rmvnorm(n = 150, mean = c(1, 2), sigma = matrix(c(1, 1, 1, 2), 2)) # !library(mvtnorm)


##### Tests of Gaussianity -----

mvn(X)$multivariateNormality # !library(MVN)
mvn(X)$multivariateNormality$'p value'

mvn(data = X, mvnTest = "royston")$multivariateNormality
# Royston’s test is a multivariate extension of the Shapiro-Wilk test

mvn(X, mvnTest = "hz", multivariatePlot = "qq")$multivariateNormality
# With Q-Q plot of the squared Mahalanobis distance over chi-square


##### Identify Outliers -----

d2 <- matrix(mahalanobis(data, colMeans(data), cov(data)))

hist(d2, prob = TRUE, main = " Histogram of the Mahalanobis Distance", ylab = "density")
lines(0:2000/100, dchisq(0:2000/100, 2), col = 'blue', lty = 2, lwd = 2)
# M. Dist, in presence of normal data, follows a chi-squared distribution (df = #features or #positive_eigenvalues)

threshold <- 12
data_wo_outliers <- data[which(d2 <= threshold), ]

plot(d2)
abline(h = threshold, col = 'grey', lty = 2, lwd = 1)
points(d2, col = ifelse(d2 <= threshold, 'black', 'red'), pch = 19)


##### Box-Cox Transformations -----

box_cox <- function(x, lambda = 0) {
  if (lambda != 0)
    return((x ^ lambda - 1) / lambda)
  return(log(x))
}
# Remember: these transformations can be applied only to positive data
# For lambda<1: observations <1 are "spread", observations >1 are "shrinked"
# For lambda>1: observations <1 are "shrinked", observations >1 are "spread"

x <- seq(0.01, 25, by = 0.01)

hist(x)
hist(box_cox(x))

lambda.x <- powerTransform(x) # !library(car)
bc.x <- bcPower(x, lambda.x$lambda[1]) # !library(car)

hist(bc.x)

lambda.data <- powerTransform(data)
BC1 <- bcPower(data[, "feature1_name"], lambda.data$lambda[1])
BC2 <- bcPower(data[, "feature2_name"], lambda.data$lambda[2])

mvn(cbind(BC1, BC2))$multivariateNormality
# Note: the higher the dimensionality, the more difficult to recover the normality


##### Tests and Confidence Regions for the Mean -----

n <- dim(data)[1]
p <- dim(data)[2]

M <- sapply(data, mean)
S <- cov(data)
S.inv <- solve(S)

mvn(data)$multivariateNormality

alpha <- 0.10
mu0 <- c(95, 75)

###### Inference Relying on Normality ------

T2 <- n * (M - mu0) %*% S.inv %*% (M - mu0)
T2

cfr.fisher <- ((n - 1) * p / (n - p)) * qf(1 - alpha, p, n - p)
cfr.fisher

T2 < cfr.fisher

P <- 1 - pf(T2 * (n - p) / ((n - 1) * p), p, n - p)
P

plot(data, asp = 1)
ellipse(M, S/n, sqrt(cfr.fisher), col = 'red', lty = 2, lwd = 2, center.cex = 1)
points(mu0[1], mu0[2], pch = 16, col ='blue', cex = 1.5)


###### Inference Relying on Asymptotics ------

T2A <- n * (M - mu0) %*%  S.inv  %*% (M - mu0)
cfr.chisq <- qchisq(1 - alpha, p)

T2A < cfr.chisq

PA <- 1 - pchisq(T2A, p)
PA


##### Confidence Intervals ------

###### Simultaneous T2 Confidence Intervals -------

T2.I <- cbind(inf = M - sqrt(cfr.fisher * diag(S)/n), 
              center = M, 
              sup = M + sqrt(cfr.fisher * diag(S)/n))
T2.I

# or

T2.I.F1 <- c(M[1] - sqrt(cfr.fisher * S[1, 1] / n),
             M[1],
             M[1] + sqrt(cfr.fisher * S[1, 1] / n))

T2.I.F2 <- c(M[2] - sqrt(cfr.fisher * S[2, 2] / n),
             M[2],
             M[2] + sqrt(cfr.fisher * S[2, 2] / n))

T2.I <- rbind(T2.I.F1, T2.I.F2)
dimnames(T2.I)[[2]] <- c('inf', 'center', 'sup')
dimnames(T2.I)[[1]] <- c(dimnames(data)[[2]][1], dimnames(data)[[2]][2])

T2.I

rect(T2.I[1,1], T2.I[2,1], T2.I[1,3], T2.I[2,3], border = 'red', lwd = 2)

matplot(1:p, 1:p, pch = '', ylim = range(data), xlab = 'Variables', ylab = 'T2 for a component', 
        main =' Simultaneous T2 conf. int. for the components')
for(i in 1:p) segments(i, T2.I[i, 1], i, T2.I[i, 3], lwd = 3, col = i)
points(1:p, T2.I[, 2], pch = 16, col = 1:p)
points(1:p, mu0, lwd = 3, col = 'orange')


###### Bonferroni Confidence Intervals -------

k <- p 
cfr.t <- qt(1 - alpha/(2*k), n-1)

BF.I <- cbind(inf = M - cfr.t * sqrt(diag(S)/n),
              center = M, 
              sup = M + cfr.t * sqrt(diag(S)/n))
BF.I

# or

BF.I.F1 <- c(M[1] - cfr.t * sqrt(S[1, 1] / n),
             M[1],
             M[1] + cfr.t * sqrt(S[1, 1] / n))

BF.I.F2 <- c(M[2] - cfr.t * sqrt(S[2, 2] / n),
             M[2],
             M[2] + cfr.t * sqrt(S[2, 2] / n))

BF.I <- rbind(BF.I.F1, BF.I.F2)
dimnames(BF.I)[[2]] <- c('inf', 'center', 'sup')
dimnames(BF.I)[[1]] <- c(dimnames(data)[[2]][1], dimnames(data)[[2]][2])

BF.I

rect(BF.I[1,1], BF.I[2,1], BF.I[1,3], BF.I[2,3], border = 'orange', lwd = 2)

matplot(1:p, 1:p, pch = '', ylim = range(data), xlab = 'Variables', ylab = 'Bonferroni for a component', 
        main =' Bonferroni conf. int. for the components')
for(i in 1:p) segments(i, BF.I[i, 1], i, BF.I[i, 3], lwd = 3, col = i)
points(1:p, BF.I[, 2], pch = 16, col = 1:p)
points(1:p, mu0, lwd = 3, col = 'orange')


##### Project Confidence Region ------

###### Along Cartesian Axes ------

plot(data, asp = 1, pch = 1)
ellipse(center = M, shape = S/n, radius = sqrt(cfr.fisher), lwd = 2, col = 'blue')
abline(v = T2.I[1,1], col = 'red', lwd = 1, lty = 2)
abline(v = T2.I[1,3], col = 'red', lwd = 1, lty = 2)
abline(h = T2.I[2,1], col = 'red', lwd = 1, lty = 2)
abline(h = T2.I[2,3], col  ='red', lwd = 1, lty = 2)

points(mu0[1], mu0[2], pch = 16, col = 'grey35', cex = 1.5)
abline(v = mu0[1], h = mu0[2], col = 'grey35')

segments(T2.I[1, 1], mu0[2], T2.I[1, 3], mu0[2], lty = 1, lwd = 2, col = 'red')
segments(mu0[1], T2.I[2, 1], mu0[1], T2.I[2, 3], lty = 1, lwd = 2, col = 'red')

abline(v = BF.I[1,1], col = 'orange', lwd = 1, lty = 2)
abline(v = BF.I[1,3], col = 'orange', lwd = 1, lty = 2)
abline(h = BF.I[2,1], col = 'orange', lwd = 1, lty = 2)
abline(h = BF.I[2,3], col  ='orange', lwd = 1, lty = 2)

points(mu0[1], mu0[2], pch = 16, col = 'grey35', cex = 1.5)
abline(v = mu0[1], h = mu0[2], col = 'grey35')

segments(BF.I[1, 1], mu0[2], BF.I[1, 3], mu0[2], lty = 1, lwd = 2, col = 'orange')
segments(mu0[1], BF.I[2, 1], mu0[1], BF.I[2, 3], lty = 1, lwd = 2, col = 'orange')


###### Along Worst Direction ------

worst <- S.inv %*% (M - mu0)
worst <- worst / sqrt(sum(worst ^ 2))
worst

T2
n * (t(worst) %*% (M - mu0))^2 / (t(worst) %*% S %*% worst)
(mean(as.matrix(data) %*% worst) - (mu0 %*% worst))^2 / (var(as.matrix(data) %*% worst) / n)

CI.worst <- c(M %*% worst - sqrt(cfr.fisher*(t(worst) %*% S %*% worst) / n),
              M %*% worst,
              M %*% worst + sqrt(cfr.fisher*(t(worst) %*% S %*% worst) / n))
CI.worst
mu0 %*% worst
(CI.worst[1] < mu0 %*% worst) & (mu0 %*% worst < CI.worst[3])   

x.min <- CI.worst[1] * worst # (x,y) coords of the lower bound of the interval
x.max <- CI.worst[3] * worst # (x,y) coords of the upper bound of the interval
m1.ort <- - worst[1] / worst[2] # Slope of the line orthogonal to worst
q.min.ort <- x.min[2] - m1.ort * x.min[1] # Intercept of line of slope m1.ort passing by x.min
q.max.ort <- x.max[2] - m1.ort * x.max[1] # Intercept of line of slope m1.ort passing by x.max
abline(q.min.ort, m1.ort, col = 'forestgreen', lty = 2,lwd = 1)
abline(q.max.ort, m1.ort, col = 'forestgreen', lty = 2,lwd = 1)

m1 = worst[2] / worst[1] # worst direction
q1 <- mu0[2] - m1 * mu0[1]
abline(q1, m1, col = 'grey35')

x.min.plot <- c((q1 - q.min.ort) / (m1.ort - m1), m1 * (q1 - q.min.ort) / (m1.ort - m1) + q1)
x.max.plot <- c((q1 - q.max.ort) / (m1.ort - m1), m1 * (q1 - q.max.ort) / (m1.ort - m1) + q1)

segments(x.min.plot[1], x.min.plot[2], x.max.plot[1], x.max.plot[2], lty = 1, lwd = 2, col = 'forestgreen')


##### Confidence Region Characterization ------

# Center:
M

# Directions of the principal axes:
eigen(S/n)$vectors
eigen(S/n)$vectors[, 1]
eigen(S/n)$vectors[, 2]

# Length of the semi-axes of the ellipse:
r <- sqrt(cfr.fisher)
r * sqrt(eigen(S/n)$values)

plot(data, asp = 1)
abline(a = M[2] - eigen(S)$vectors[2, 1] / eigen(S)$vectors[1, 1] * M[1], 
       b = eigen(S)$vectors[2, 1] / eigen(S)$vectors[1, 1], 
       lty = 2, col = 'dark red', lwd = 2)
abline(a = M[2] - eigen(S)$vectors[2, 2] / eigen(S)$vectors[1, 2] * M[1], 
       b = eigen(S)$vectors[2, 2] / eigen(S)$vectors[1, 2], 
       lty = 2, col = 'red', lwd = 2)


###------------------------------###
### PRINCIPAL COMPONENT ANALYSIS ###-------------------------------------------
###------------------------------###

boxplot(data, las = 2, col = 'gold')
boxplot(scale(x = data, center = T, scale = F), las = 2, col = 'gold')
boxplot(scale(x = data, center = T, scale = T), las = 2, col = 'gold')


#### PCA on Original Data -----

pc.data <- princomp(data, scores = T)
pc.data
summary(pc.data)

layout(matrix(c(2, 3, 1, 3), 2, byrow = TRUE))

plot(pc.data, las = 2, main = 'Principal Components', ylim = c(0, 7))
abline(h = 1, col = 'blue')

barplot(sapply(data, sd)^2, las = 2, main = 'Original Variables', ylim = c(0, 7),
        ylab = 'Variances')

plot(cumsum(pc.data$sde^2) / sum(pc.data$sde^2), type = 'b', axes = FALSE, 
     xlab = 'Number of components', ylab = 'Contribution to the total variance', ylim = c(0, 1))
abline(h = 1, col = 'blue')
abline(h = 0.8, lty = 2, col = 'blue')
box()
axis(2, at = 0:10/10, labels = 0:10/10)
axis(1, at = 1:ncol(data), labels = 1:ncol(data), las = 2)


#### PCA on Standardized Variables -----

data.sd <- scale(data)
data.sd <- data.frame(data.sd)

boxplot(data.sd, las = 2, col = 'gold')

pc.data <- princomp(data.sd, scores = T)
pc.data
summary(pc.data)

layout(matrix(c(2, 3, 1, 3), 2, byrow = TRUE))

plot(pc.data, las = 2, main = 'Principal Components', ylim = c(0, 7))
abline(h = 1, col = 'blue')

barplot(sapply(data.sd, sd)^2, las = 2, main = 'Original Variables', ylim = c(0, 7),
        ylab = 'Variances')

plot(cumsum(pc.data$sde^2) / sum(pc.data$sde^2), type = 'b', axes = FALSE, 
     xlab = 'Number of components', ylab = 'Contribution to the total variance', ylim = c(0, 1))
abline(h = 1, col = 'blue')
abline(h = 0.8, lty = 2, col = 'blue')
box()
axis(2, at = 0:10/10, labels = 0:10/10)
axis(1, at = 1:ncol(data.sd), labels = 1:ncol(data.sd), las = 2)


#### Variances ----

pc.data$sdev^2

# Proportion of variance explained by component 1
var(as.matrix(data.sd) %*% load.data[, 1]) / sum(sapply(data.sd, var))


#### Loadings ----

load.data <- pc.data$loadings
load.data

nPCs <- 3
par(mar = c(2,2,2,1), mfrow = c(nPCs, 1))
for(i in 1:nPCs) barplot(load.data[, i], ylim = c(-1, 1), main = paste('Loadings PC ', i, sep = ''))

# The first PC represents...
# The second PC contains the information...

# High PC1: big...
# Low PC1: small...
# High PC2: high..., low...
# Low PC2: small..., high...


#### Scores ----

scores.data <- pc.data$scores

col.lab <- ifelse(data.label[] %in% c("feature2_name", "feature3_name"), 
                  ifelse(data.label[] %in% c("feature3_name"), 
                         'blue', 'green'), 
                  'red')

par(mfrow = c(1, 1))
plot(scores.data[, 1:2], col = col.lab, pch = 19, xlim = c(-5, 5), ylim = c(-5, 5))
abline(h = -5, v = -5, col = 1)
points(scores.data[, 1], rep(-5, n), col = col.lab, pch = 19)
points(rep(-5, n), scores.data[, 2], col = col.lab, pch = 19)
abline(h = 0, v = 0, lty = 2, col = 'grey')
legend('topright', levels(factor(data.label[])), fill = c('red', 'green', 'blue'), bty = 'n')

biplot(pc.data)


#### Projections on PC Space ----

##### PCA Being Performed on Original Data -----

# Projection on the space generated by the k-th principal component
par(mfrow = c(2, 5))
matplot(t(data), type = 'l', main = 'Data', ylim = range(data))
meanA <- colMeans(data)
matplot(meanA, type = 'l', main = '0 PC', lwd = 2, ylim = range(data))
for(i in 1:8)
{
  projection <- matrix(meanA, dim(data)[[1]], dim(data)[[2]], byrow = T) + scores.data[, i] %*% t(load.data[, i])
  matplot(t(projection), type='l', main = paste(i, 'PC'), ylim = range(data))
  matplot(meanA, type='l', lwd = 2, add = T)
}

# Projection on the space generated by the first k principal components
par(mfrow = c(2, 5))
matplot(t(data), type = 'l', main = 'Data', ylim = range(data))
meanF <- colMeans(data)
matplot(meanF, type = 'l', main = 'First 0 PCs', lwd = 2, ylim = range(data))
projection <- matrix(meanF, dim(data)[[1]], dim(data)[[2]], byrow = T)
for (i in 1:8) {
  projection <- projection + scores.data[, i] %*% t(load.data[, i])
  matplot(t(projection), type = 'l', main = paste('First', i, 'PCs'), ylim = range(data))
  matplot(meanF, type = 'l', lwd = 2, add = T)
}


##### PCA Being Performed on Standardized Variables -----

# Projection on the space generated by the k-th principal component
par(mfrow = c(2, 5))
matplot(t(data.sd), type = 'l', main = 'Data', ylim = range(data.sd))
meanA <- colMeans(data.sd)
matplot(meanA, type = 'l', main = '0 PC', lwd = 2, ylim = range(data.sd))
for(i in 1:8)
{
  projection <- matrix(meanA, dim(data.sd)[[1]], dim(data.sd)[[2]], byrow = T) + scores.data[, i] %*% t(load.data[, i])
  matplot(t(projection), type='l', main = paste(i, 'PC'), ylim = range(data.sd))
  matplot(meanA, type='l', lwd = 2, add = T)
}

# Projection on the space generated by the first k principal components
par(mfrow = c(2, 5))
matplot(t(data.sd), type = 'l', main = 'Data', ylim = range(data.sd))
meanF <- colMeans(data.sd)
matplot(meanF, type = 'l', main = 'First 0 PCs', lwd = 2, ylim = range(data.sd))
projection <- matrix(meanF, dim(data.sd)[[1]], dim(data.sd)[[2]], byrow = T)
for (i in 1:8) {
  projection <- projection + scores.data[, i] %*% t(load.data[, i])
  matplot(t(projection), type = 'l', main = paste('First', i, 'PCs'), ylim = range(data.sd))
  matplot(meanF, type = 'l', lwd = 2, add = T)
}


##### Projection of a New Datum -----

new.datum <- c(37548.50, 754.42, 318.34, 154.25, 0.88, 37506.40, 219.63, 0.80)

###### PCA Being Performed on Original Data ------

new.datum.score <- t(load.data) %*% (new.datum - colMeans(data))
new.datum.score

plot(pc.data$scores[, 1:2], pch = 19, las = 2)
points(new.datum.score[1], new.datum.score[2], col = 'red', pch = 19)


###### PCA Being Performed on Standardized Variables ------

new.datum.std <- (new.datum - colMeans(data)) / sapply(data, sd)

new.datum.std.score <- t(load.data) %*% (new.datum.std - colMeans(data.sd))
new.datum.std.score

plot(pc.data$scores[, 1:2], pch = 19, las = 2)
points(new.datum.std.score[1], new.datum.std.score[2], col = 'red', pch = 19)


###--------------------------###
### INFERENCE ABOUT THE MEAN ###-------------------------------------------
###--------------------------###

#### Paired Data ----

D <- data.frame(DF1 = data$feature1_obs1 - data$feature1_obs2,
                DF2 = data$feature2_obs1 - data$feature2_obs2) 
D

mvn(data = D)$multivariateNormality

plot(D)

n <- dim(D)[1]
p <- dim(D)[2]

D.mean <- sapply(D, mean)
D.cov <- cov(D)
D.invcov <- solve(D.cov)

alpha <- 0.05
delta.0 <- c(0, 0)

D.T2 <- n * (D.mean - delta.0) %*% D.invcov %*% (D.mean - delta.0)
D.T2

cfr.fisher <- ((n - 1) * p / (n - p)) * qf(1 - alpha, p, n - p)
cfr.fisher

D.T2 < cfr.fisher

P <- 1 - pf(D.T2 * (n - p) / (p * (n - 1)), p, n - p)
P


##### Confidence Intervals -----

T2.I.DF1 <- c(D.mean[1] - sqrt(cfr.fisher * D.cov[1, 1] / n),
              D.mean[1],
              D.mean[1] + sqrt(cfr.fisher * D.cov[1, 1] / n))

T2.I.DF2 <- c(D.mean[2] - sqrt(cfr.fisher * D.cov[2, 2] / n),
              D.mean[2],
              D.mean[2] + sqrt(cfr.fisher * D.cov[2, 2] / n))

T2.I <- rbind(T2.I.DF1, T2.I.DF2)
dimnames(T2.I)[[2]] <- c('inf', 'center', 'sup')
T2.I

BF.I.DF1 <- c(D.mean[1] - cfr.t * sqrt(D.cov[1, 1] / n),
              D.mean[1],
              D.mean[1] + cfr.t * sqrt(D.cov[1, 1] / n))

BF.I.DF2 <- c(D.mean[2] - cfr.t * sqrt(D.cov[2, 2] / n),
              D.mean[2],
              D.mean[2] + cfr.t * sqrt(D.cov[2, 2] / n))

BF.I <- rbind(BF.I.DF1, BF.I.DF2)
dimnames(BF.I)[[2]] <- c('inf', 'center', 'sup')
BF.I


##### Plot Intervals -----

plot(D, asp = 1, pch = 1, main = 'Dataset of the Differences')
ellipse(center = D.mean, shape = D.cov/n, radius = sqrt(cfr.fisher), lwd = 2, col = 'grey')
abline(v = T2.I[1,1], col = 'red', lwd = 1, lty = 2)
abline(v = T2.I[1,3], col = 'red', lwd = 1, lty = 2)
abline(h = T2.I[2,1], col = 'red', lwd = 1, lty = 2)
abline(h = T2.I[2,3], col  ='red', lwd = 1, lty = 2)

points(delta.0[1], delta.0[2], pch = 16, col = 'grey35', cex = 1.5)
abline(h = delta.0[1], v = delta.0[2], col = 'grey35')

segments(T2.I.DF1[1], 0, T2.I.DF1[3], 0, lty = 1, lwd = 2, col = 'red')
segments(0, T2.I.DF2[1], 0, T2.I.DF2[3], lty = 1, lwd = 2, col = 'red')

abline(v = BF.I[1,1], col = 'orange', lwd = 1, lty = 2)
abline(v = BF.I[1,3], col = 'orange', lwd = 1, lty = 2)
abline(h = BF.I[2,1], col = 'orange', lwd = 1, lty = 2)
abline(h = BF.I[2,3], col  ='orange', lwd = 1, lty = 2)

points(delta.0[1], delta.0[2], pch = 16, col = 'grey35', cex = 1.5)
abline(h = delta.0[1], v = delta.0[2], col = 'grey35')

segments(BF.I.DF1[1], 0, BF.I.DF1[3], 0, lty = 1, lwd = 2, col = 'orange')
segments(0, BF.I.DF2[1], 0, BF.I.DF2[3], lty = 1, lwd = 2, col = 'orange')

worst <- D.invcov %*% (D.mean - delta.0)
worst <- worst / sqrt(sum(worst ^ 2))
worst

CI.worst <- c(D.mean %*% worst - sqrt(cfr.fisher*(t(worst) %*% D.cov %*% worst) / n),
              D.mean %*% worst,
              D.mean %*% worst + sqrt(cfr.fisher*(t(worst) %*% D.cov %*% worst) / n))
CI.worst
delta.0 %*% worst
(IC.worst[1] < delta.0 %*% worst) & (delta.0 %*% worst < IC.worst[3])   

x.min <- CI.worst[1] * worst # (x,y) coords of the lower bound of the interval
x.max <- CI.worst[3] * worst # (x,y) coords of the upper bound of the interval
m1.ort <- - worst[1] / worst[2] # Slope of the line orthogonal to worst
q.min.ort <- x.min[2] - m1.ort * x.min[1] # Intercept of line of slope m1.ort passing by x.min
q.max.ort <- x.max[2] - m1.ort * x.max[1] # Intercept of line of slope m1.ort passing by x.max
abline(q.min.ort, m1.ort, col = 'forestgreen', lty = 2, lwd = 1)
abline(q.max.ort, m1.ort, col = 'forestgreen', lty = 2, lwd = 1)

m1 = worst[2] / worst[1] # worst direction
abline(0, m1, col='grey35') # Intercept 0 because the line has to pass by delta.0 which is (0, 0)
segments(x.min[1], x.min[2], x.max[1], x.max[2], lty = 1, lwd = 2, col = 'forestgreen')


#### Repeated Measures ----

mvn(data)$multivariateNormality

matplot(t(data), type = 'l', lty = 1)

n <- dim(data)[1]
q <- dim(data)[2]

M <- sapply(data, mean) 
S <- cov(data) 

C <- matrix(c(-1, 1, 0, 0,
              -1, 0, 1, 0,
              -1, 0, 0, 1), q-1, q, byrow = T)
C

alpha <- .05
delta.0 <- c(0, 0, 0)

Md <- C %*% M 
Sd <- C %*% S %*% t(C) 
Sdinv <- solve(Sd)

T2 <- n * t(Md - delta.0) %*% Sdinv %*% (Md - delta.0)
T2

cfr.fisher <- ((q - 1) * (n - 1) / (n - (q - 1))) * qf(1 - alpha, (q - 1), n - (q - 1)) 
cfr.fisher

T2 < cfr.fisher

P <- 1 - pf(T2 * (n - (q - 1)) / ((q - 1) * (n - 1)), (q - 1), n - (q - 1))
P


##### Confidence Intervals -----

# Simultaneous T2 intervals in the direction of the contrasts

T2.I <- cbind(Md - sqrt(cfr.fisher * diag(Sd)/n),
              Md,
              Md + sqrt(cfr.fisher * diag(Sd)/n))
T2.I

# Bonferroni intervals

k <- q-1   # number of increments (i.e., dim(C)[1])
cfr.t <- qt(1 - alpha/(2*k), n-1)

BF.I <- cbind(Md - cfr.t * sqrt(diag(Sd)/n),
              Md,
              Md + cfr.t * sqrt(diag(Sd)/n))
BF.I


##### Plotting Intervals -----

matplot(t(matrix(1:(q-1), 3, 3)), t(BF.I), type = 'b', pch = '', xlim = c(0, 4), xlab = '', ylab = '', main = 'Confidence Intervals')

segments(matrix(1:(q-1), 3, 1), BF.I[, 1], matrix(1:3, 3, 1), BF.I[, 3],
         col = 'orange', lwd = 2)
points(1:(q-1), BF.I[, 2], col = 'orange', pch = 16)

points(1:(q-1)+.05, delta.0, col='black', pch = 16)

segments(matrix(1:(q-1)+.1, 3, 1), T2.I[, 1], matrix(1:(q-1)+.1, 3, 1), T2.I[, 3], col = 'blue', lwd = 2)
points(1:(q-1)+.1, T2.I[, 2], col = 'blue', pch = 16)

legend('topright', c('Bonf. IC', 'Sim-T2 IC'), col = c('orange', 'blue'), lty = 1, lwd = 2)


##### Equivalent Contrast Matrices -----

C <- matrix(c(-1, 1, 0, 0,
              -1, 0, 1, 0,
              -1, 0, 0, 1), q-1, q, byrow = T)

delta.0 <- c(-2, -2, 0)

# or

C <- matrix(c(-1, 1, 0, 0,
              0, -1, 1, 0,
              0, 0, -1, 1), q-1, q, byrow = T)

delta.0 <- c(-2, 0, 2)

# and

C <- matrix(c(-1, 1, 0, 0,
              -1, 0, 1, 0,
              -1, 0, 0, 1), q-1, q, byrow = T)

delta.0 <- c(3, -2, 0)

# or

C <- matrix(c(-1, 1, 0, 0,
              0, -1, 1, 0,
              0, 0, -1, 1), q-1, q, byrow = T)

delta.0 <- c(3, -5, 2)


#### Two Independent Gaussian Populations ----

data1 <- read.table('data_pop1.txt', header = T)
data2 <- read.table('data_pop2.txt', header = T)

n1 <- dim(data1)[1] 
n2 <- dim(data2)[1] 
p  <- dim(data1)[2] 

mvn(data1)$multivariateNormality
mvn(data2)$multivariateNormality

data1.mean <- sapply(data1, mean)
data2.mean <- sapply(data2, mean)
data1.cov <-  cov(data1)
data2.cov <-  cov(data2)
Sp <- ((n1 - 1) * data1.cov + (n2 - 1) * data2.cov) / (n1 + n2 - 2)

list(S1 = data1.cov, S2 = data2.cov, Spooled = Sp)

alpha   <- .01
delta.0 <- c(0, 0)
Spinv   <- solve(Sp)

T2 <- n1 * n2 / (n1 + n2) * (data1.mean - data2.mean - delta.0) %*% Spinv %*% (data1.mean - data2.mean - delta.0)
T2

cfr.fisher <- (p * (n1 + n2 - 2) / (n1 + n2 - 1 - p)) * qf(1 - alpha, p, n1 + n2 - 1 - p)
cfr.fisher

T2 < cfr.fisher

P <- 1 - pf(T2 / (p * (n1 + n2 - 2) / (n1 + n2 - 1 - p)), p, n1 + n2 - 1 - p)
P


##### Confidence Intervals -----

# Simultaneous T2 intervals

T2.I.F1 <- c(data1.mean[1] - data2.mean[1] - sqrt(cfr.fisher * Sp[1, 1] * (1 / n1 + 1 / n2)),
             data1.mean[1] - data2.mean[1],
             data1.mean[1] - data2.mean[1] + sqrt(cfr.fisher * Sp[1, 1] * (1 / n1 + 1 / n2)))
T2.I.F2 <- c(data1.mean[2] - data2.mean[2] - sqrt(cfr.fisher * Sp[2, 2] * (1 / n1 + 1 / n2)),
             data1.mean[2] - data2.mean[2],
             data1.mean[2] - data2.mean[2] + sqrt(cfr.fisher * Sp[2, 2] * (1 / n1 + 1 / n2)))

T2.I <- rbind(T2.I.F1, T2.I.F2)
dimnames(T2.I)[[2]] <- c('inf', 'center', 'sup')
T2.I

# Bonferroni intervals

k <- p
cfr.t <- qt(1 - alpha/(2*k), n-1)

BF.I.F1 <- c(data1.mean[1] - data2.mean[1] - cfr.t * sqrt(Sp[1, 1] * (1 / n1 + 1 / n2)),
             data1.mean[1] - data2.mean[1],
             data1.mean[1] - data2.mean[1] + cfr.t * sqrt(Sp[1, 1] * (1 / n1 + 1 / n2)))
BF.I.F2 <- c(data1.mean[2] - data2.mean[2] - cfr.t * sqrt(Sp[2, 2] * (1 / n1 + 1 / n2)),
             data1.mean[2] - data2.mean[2],
             data1.mean[2] - data2.mean[2] + cfr.t * sqrt(Sp[2, 2] * (1 / n1 + 1 / n2)))

BF.I <- rbind(BF.I.F1, BF.I.F2)
dimnames(BF.I)[[2]] <- c('inf', 'center', 'sup')
BF.I


##### Confidence Region -----

plot(data1, asp = 1, pch = 19, col = 'red', xlim = c(4, 10), ylim = c(4,10))
for (i in 1:n1)
{
  points(data2[i, 1], data2[i, 2], col = 'blue', pch = 19)
}
points(data1.mean[1], data1.mean[2], col = 'red', pch = 11, cex = 2)
points(data2.mean[1], data2.mean[2], col = 'blue', pch = 11, cex = 2)

plot(data1.mean[1] - data2.mean[1], data1.mean[2] - data2.mean[2], asp = 1, pch = 19, xlim = c(-1, 3))
ellipse(center = (data1.mean - data2.mean), shape=Sp*(n1+n2)/(n1*n2), radius = sqrt(cfr.fisher), lwd = 2, col = 'grey', center.cex = 1.25)
points(delta.0[1], delta.0[2], col = 'red', pch = 9, cex = 1)

abline(v = BF.I[1,1], col = 'blue', lwd = 1, lty = 2)
abline(v = BF.I[1,3], col = 'blue', lwd = 1, lty = 2)
abline(h = BF.I[2,1], col = 'blue', lwd = 1, lty = 2)
abline(h = BF.I[2,3], col = 'blue', lwd = 1, lty = 2)
segments(BF.I.F1[1], BF.I.F2[1], BF.I.F1[3], BF.I.F2[1], lty = 1, lwd = 2, col = 'blue')
segments(BF.I.F1[1], BF.I.F2[1], BF.I.F1[1], BF.I.F2[3], lty = 1, lwd = 2, col = 'blue')

abline(v = T2.I[1,1], col = 'purple', lwd = 1, lty = 2)
abline(v = T2.I[1,3], col = 'purple', lwd = 1, lty = 2)
abline(h = T2.I[2,1], col = 'purple', lwd = 1, lty = 2)
abline(h = T2.I[2,3], col = 'purple', lwd = 1, lty = 2)
segments(T2.I.F1[1], T2.I.F2[1], T2.I.F1[3], T2.I.F2[1], lty = 1, lwd = 2, col = 'purple')
segments(T2.I.F1[1], T2.I.F2[1], T2.I.F1[1], T2.I.F2[3], lty = 1, lwd = 2, col = 'purple')

legend('topright', c('Bonf. IC', 'Sim-T2 IC'), col = c('blue', 'purple'), lty = 1, lwd = 2)


#### Large Scale Hypothesis Testing ----

allergy <- read.table('hatingalmonds.txt')
head(allergy)
dim(allergy)

noallergy <- read.table('lovingalmonds.txt')
head(noallergy)
dim(noallergy)

n1 <- dim(allergy)[1]
n2 <- dim(noallergy)[1]
p <- dim(noallergy)[2]

x.mean1 <- sapply(allergy, mean)
x.mean2 <- sapply(noallergy, mean)

p.hat <- (x.mean1 * n1 + x.mean2 * n2) / (n1 + n2) # pooled estimator of the proportion
x.var <- (p.hat * (1 - p.hat)) # (I trust it)

# Test: H0.i: mu.i1 == mu.i2  vs H1.i: mu.i1 != mu.i2
# Asymptotic Z-test for the comparison of proportions (univariate and n=100 -> OK)

z.i <- (x.mean1 - x.mean2) / sqrt(x.var * (1 / n1 + 1 / n2))
p.i <- ifelse(z.i < 0, 2 * pnorm(z.i), 2 * (1 - pnorm(z.i)))

which(p.i<.01)

# Bonferroni (control the family-wise error rate)
k <- 520

which(p.i*k<.01)

# or
p.Bf <- p.adjust(p.i, method='bonferroni')

which(p.Bf<.01)  

# Benjamini-Hockberg (control the false discovery rate)  
p.BH <- p.adjust(p.i, method='BH')

which(p.BH<.01)


par(mfrow=c(1,3))
plot(p.i, main='Univariate')
abline(h=.01, lwd=2, col='red')

plot(p.Bf, main='Corrected - Bonferroni')
abline(h=.01, lwd=2, col='red')

plot(p.BH, main='Corrected - BH')
abline(h=.01, lwd=2, col='red')