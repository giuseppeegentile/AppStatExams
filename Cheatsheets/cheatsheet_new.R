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
lines(c(95, 95), c(60, 100), col = 'blue', pch = 16, type = 'b')
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


###-----------------------------------###
### MULTIVARIATE ANALYSIS OF VARIANCE ###-------------------------------------------
###-----------------------------------###

#### One-way ANOVA ----

target <- data$target
treatment <- factor(data$treatment)

n <- length(treatment)      
ng <- table(treatment)       
treat <- levels(treatment)      
g <- length(levels(treatment))


##### Data Visualization -----

boxplot(target ~ treatment, xlab = 'Treat Level', ylab = 'Target Variable', col = 'grey85')

par(mfrow=c(1,2))
barplot(rep(mean(target), g), names.arg = treat, ylim = c(0, max(target)),
        las = 2, col = 'grey85', main='Model under H0')
barplot(tapply(target, treatment, mean), names.arg = treat, ylim = c(0, max(target)),
        las = 2, col = rainbow(g), main='Model under a special case of H1')
par(mfrow=c(1,1))


##### Verify Assumptions -----

# 1) normality (univariate) in each group

Ps <- NULL
for(i in 1:g)
{
  Ps <- rbind(Ps, shapiro.test(target[treatment == treat[i]])$p)
}
dimnames(Ps)[[1]] <- treat
dimnames(Ps)[[2]] <- c("p-value")
Ps

# 2) same covariance structure (= same sigma^2)

Var <- NULL
for(i in 1:g)
{
  Var <- rbind(Var, var(target[treatment == treat[i]]))
}
dimnames(Var)[[1]] <- treat
dimnames(Var)[[2]] <- c("sigma^2")
Var

Pvt <- NULL
rownames <- NULL
for(i in 1:(g-1))
{
  for(j in (i+1):g)
  {
    Pvt <- rbind(Pvt, var.test(target[treatment == treat[i]], target[treatment == treat[j]])$p.value)
    rownames <- cbind(rownames, paste(treat[i], "v", treat[j]))
  }
}
dimnames(Pvt)[[1]] <- rownames
dimnames(Pvt)[[2]] <- c("p-value")
Pvt

bartlett.test(target, treatment)


##### Fit -----

fit <- aov(target ~ treatment)
summary(fit)
### How to read the summary:
#              Df   Sum Sq      Mean Sq      F value     Pr(>F)    
# treatment  (g-1) SStreat  SStreat/(g-1)  Fstatistic  p-value [H0: tau.i=0 for every i]
# Residuals  (n-g) SSres     SSres/(n-g)


##### Confidence Intervals -----

# Which treatment level is responsible for this?

k <- g * (g - 1) / 2
alpha <- 0.05

Mediag <- tapply(target, treatment, mean)
SSres <- sum(residuals(fit) ^ 2)
S <- SSres / (n - g)

CI.range <- NULL
for(i in 1:(g-1)) 
{
  for(j in (i+1):g) 
  {
    print(paste(treat[i], "-", treat[j]))        
    print(as.numeric(c(Mediag[i] - Mediag[j] - qt(1 - alpha/(2*k), n-g) * sqrt(S * (1/ng[i] + 1/ng[j])),
                       Mediag[i] - Mediag[j] + qt(1 - alpha/(2*k), n-g) * sqrt(S * (1/ng[i] + 1/ng[j])))))
    CI.range <- rbind(CI.range, as.numeric(c(Mediag[i]-Mediag[j] - qt(1 - alpha/(2*k), n-g) * sqrt(S * (1/ng[i] + 1/ng[j])),
                                             Mediag[i]-Mediag[j] + qt(1 - alpha/(2*k), n-g) * sqrt(S * (1/ng[i] + 1/ng[j])))))
  }
}

h <- 1
plot(c(1, g*(g-1)/2), range(CI.range), pch = '', xlab = 'Pairs Treat', ylab = 'Conf. Int. tau Target')
for(i in 1:(g-1)) 
{
  for(j in (i+1):g) 
  {
    lines (c(h, h), c(CI.range[h, 1], CI.range[h, 2]), col = 'grey55'); 
    points(h, Mediag[i] - Mediag[j], pch = 16, col = 'grey55'); 
    points(h, CI.range[h, 1], col = rainbow(g)[j], pch = 16); 
    points(h, CI.range[h, 2], col = rainbow(g)[i], pch = 16); 
    h <- h + 1
  }
}
abline(h = 0)

# One-at-a-time

Auni <- matrix(0, g, g)
for(i in 1:g) {
  for(j in i:g) {
    Auni[i, j] <- Mediag[i] - Mediag[j] + qt(1 - alpha/2, n-g) * sqrt(S * (1/ng[i] + 1/ng[j]))}
  for(j in 1:i) {
    Auni[i, j] <- Mediag[j] - Mediag[i] - qt(1 - alpha/2, n-g) * sqrt(S * (1/ng[i] + 1/ng[j]))}
  Auni[i, i] <- 0
}

h <- 1
plot(c(1, g*(g-1)/2), range(Auni), pch = '', xlab = 'pairs treat', ylab = 'CI delta weight', main = 'Univariate Conf. Int.', col = 'grey55')
for(i in 1:(g-1)) 
{
  for(j in (i+1):g) 
  {
    lines (c(h, h), c(Auni[i, j], Auni[j, i])); 
    points(h, Mediag[i] - Mediag[j], pch = 16, col = 'grey55'); 
    points(h, Auni[i, j], col = rainbow(g)[i], pch = 16); 
    points(h, Auni[j, i], col = rainbow(g)[j], pch = 16); 
    h <- h + 1
  }
}
abline(h = 0)

# p-values

P <- matrix(0, g, g)
for(i in 1:g) {
  for(j in i:g) {
    P[i, j] <- (1 - pt(abs((Mediag[i] - Mediag[j]) / sqrt(S * (1/ng[i] + 1/ng[j]))), n-g))*2}
  for(j in 1:i) {
    P[i, j] <- (1 - pt(abs((Mediag[i] - Mediag[j]) / sqrt(S * (1/ng[i] + 1/ng[j]))), n-g))*2}
  P[i, i] <- 0
}
P

p <- NULL
for(i in 1:(g-2))
{
  p <- c(p, P[i, (i+1):g])
}
p <- c(p, P[g-1, g])
p

plot(1:k, p, ylim = c(0, 1), type = 'b', pch = 16, col = 'grey55', xlab = 'Pairs Treat', main='p-values')
abline(h = alpha, lty = 2)

## Bonferroni correction
p.bonf <- p.adjust(p, 'bonf') 
lines(1:k, p.bonf, col = 'blue', pch = 16, type = 'b')

## Correction according to the false discovery rate (Benjamini-Hockberg)
p.fdr <- p.adjust(p, 'fdr')
lines(1:k, p.fdr, col = 'red', pch = 16, type = 'b')

legend('topleft', c('Not corr.', 'Bonf.', 'BH'), col = c('grey55', 'blue', 'red'), pch = 16)

which(p.bonf < alpha)
which(p.fdr < alpha)


#### One-way MANOVA ----

treatment <- factor(data$treatment)
target <- data[, 1:2] # variables

n <- length(treatment)
ng <- table(treatment)
treat <- levels(treatment)
g <- length(levels(treatment))
p <- dim(target)[2]

idx <- NULL
for(i in 1:g)
{
  idx <- rbind(idx, which(treatment == treat[i]))
}
dimnames(idx)[[1]] <- treat
idx


##### Data Visualization -----

par(mfrow=c(1,g))
for(i in 1:g)
{
  boxplot(target[idx[i, ], ], main = treat[i], ylim = range(target), col = rainbow(p))
}

par(mfrow=c(1,p))
for(i in 1:p)
{
  boxplot(target[, i] ~ treatment, main = colnames(target)[i], ylim = range(target), col = rainbow(g))
}

par(mfrow=c(1,1))


##### Verify Assumptions -----

# 1)  normality (multivariate) in each group
Ps <- NULL
for(i in 1:g) {
  mvn.test <- mvn(data = target[idx[i, ], ])
  Ps <- rbind(Ps, mvn.test$multivariateNormality$`p value`)
}
dimnames(Ps)[[1]] <- treat
dimnames(Ps)[[2]] <- c("p-value")
Ps

# 2) same covariance structure (= same covariance matrix Sigma)

covs <- list()
covs[["S"]] <- cov(target)
for(i in 1:g)
{
  cov <- cov(target[idx[i, ],])
  covs[[paste("S", i, sep = '')]] <- cov
}
covs

# Box's M test for homogeneity of covariance matrices
# Should be done only if n_i > ~20 for all i, p < 5 and g < 5
# WARNING: Very sensitive to departure from normality and too restrictive for MANOVA
# especially if we have a high number of samples

summary(boxM(target, treatment)) # !library(heplots)
boxM(target, treatment)$p.value

# Heatmap on the covariance matrices

par(mfrow=c(1,g))
quant <- NULL
for(i in 2:(g+1))
{
  quant <- rbind(quant, covs[[i]])
}
for(i in 1:g)
{
  Si <- paste("S", i, sep = '')
  image(covs[[Si]], col = heat.colors(100), main = paste('Cov.', Si), asp = 1, axes = FALSE, breaks = quantile(quant, (0:100)/100, na.rm = TRUE))
}
par(mfrow=c(1,1))

# Note: We can verify the assumptions a posteriori on the residuals of the estimated model 


##### Fit -----

fit <- manova(as.matrix(target) ~ treatment)
summary.manova(fit)
# Reject the test, i.e., we have statistical evidence to state that
# the treatment has an effect on the mean features 
# of the flowers.
# Who's the responsible for this?

summary.aov(fit)
# Note: this analysis does NOT say: 
#       a) which group differ
#       b) with respect to which variables the groups in (a) differ


##### Confidence Intervals -----

alpha <- 0.05
k <- p * g * (g - 1) / 2
qT <- qt(1 - alpha/(2 * k), n - g)

W <- summary.manova(fit)$SS$Residuals
m <- sapply(target, mean)

means <- NULL
for(i in 1:g) {
  means <- rbind(means, sapply(target[idx[i, ], ], mean)) 
}
dimnames(means)[[1]] <- treat
means

inf <- list()
sup <- list()
CI <- list()
pairs <- combn(g, 2)

for (i in 1:ncol(pairs)) {
  idx1 <- pairs[1, i]
  idx2 <- pairs[2, i]
  
  m1 <- means[idx1, ]
  m2 <- means[idx2, ]
  n1 <- ng[idx1]
  n2 <- ng[idx2]
  
  inf_val <- m1 - m2 - qT * sqrt(diag(W) / (n - g) * (1 / n1 + 1 / n2))
  sup_val <- m1 - m2 + qT * sqrt(diag(W) / (n - g) * (1 / n1 + 1 / n2))
  
  inf[[paste("inf", idx1, idx2, sep = '')]] <- inf_val
  sup[[paste("sup", idx1, idx2, sep = '')]] <- sup_val
  
  CI[[paste(treat[idx1], "_", treat[idx2], sep = '')]] <- cbind(inf_val, sup_val)
}

CI

par(mfrow=c(2,p))
for(i in 1:p)
{
  boxplot(target[, i] ~ treatment, main = colnames(target)[i], ylim = range(target), col = rainbow(g))
}

for(k in 1:p)
{
  plot(c(1, g*(g - 1)/2), pch = '', 
       xlim = c(1, g*(g - 1)/2), ylim = c(range(CI)[1] - 3, range(CI)[2] + 3),
       xlab = 'Pairs Treat', ylab = paste('CI tau', k), 
       main = paste('CI tau', colnames(target)[k]))
  for(i in 1:(g*(g - 1)/2))
  {
    j <- pairs[1, i]
    l <- pairs[2, i]
    lines(c(i, i), c(CI[[i]][k, 1], CI[[i]][k, 2]))
    points(i, means[j, k] - means[l, k], pch = 16); 
    points(i, CI[[i]][k, 1], col = rainbow(g)[l], pch = 16); 
    points(i, CI[[i]][k, 2], col = rainbow(g)[j], pch = 16); 
  }
  abline(h = 0)
}

par(mfrow=c(1,1))


#### Two-ways ANOVA ----

target <- data$target

treatment1 <- factor(data$treatment1) 
treatment2 <- factor(data$treatment2) 

treatments <- factor(paste(treatment1, treatment2, sep=''))
treatments

g <- length(levels(treatment1)) # number of factor1 levels
b <- length(levels(treatment2)) # number of factor2 levels
n <- dim(target)[1]/(g*b) # group sizes
treats <- levels(treatments)

M <- mean(target) # overall mean
M.treat1 <- tapply(target, treatment1, mean) # mean per treat 1
M.treat2 <- tapply(target, treatment2, mean) # mean per treat 2
M.treats <- tapply(target, treatments, mean) # mean per treat 1 x treat 2

M.add <- NULL
for(i in 1:g)
{
  for(j in 1:b)
  {
    M.add <- cbind(M.add, M.treat1[i] + M.treat2[j])
  }
}
dimnames(M.add)[[1]] <- c("M.add")
dimnames(M.add)[[2]] <- treats
M.add


##### Visualize Data -----

par(mfrow=c(2,3), las = 2)
barplot(rep(M,(g*b)), names.arg = treats, ylim = c(0, range(target)[2] + 3), main = 'No Treatments')
barplot(rep(M.treat1, each = b), names.arg = treats, ylim = c(0, range(target)[2] + 3), 
        col = rep(rainbow(g), each = b), main = 'Only Treat. 1')
barplot(rep(M.treat2, times = g), names.arg = treats, ylim = c(0,range(target)[2] + 3),
        col = rep(rainbow(b), times = g), main = 'Only Treat. 2')
barplot(M.add - M, names.arg = treats, ylim = c(0, range(target)[2] + 3), 
        col = rep(rainbow(b), times = g), density = rep(10, (g*b)), angle = 135, 
        main = 'Additive Model T1+T2')
barplot(M.add - M, names.arg = treats, ylim = c(0,range(target)[2] + 3), 
        col = rep(rainbow(g), each = b), density = rep(10, (g*b)), add = T)
barplot(M.treats, names.arg = treats, ylim = c(0,range(target)[2] + 3), 
        col = rainbow(g*b), main = 'Model with Interact. T1+T2')
plot(treatments, target, col = rainbow(g*b), ylim =c (0, range(target)[2] + 3), xlab='')
par(mfrow=c(1,1))


##### Verify Assumptions -----

# 1) normality (univariate) in each group

Ps <- NULL
for(i in 1:(g*b))
{
  Ps <- rbind(Ps, shapiro.test(target[treatments == treats[i]])$p)
}
dimnames(Ps)[[1]] <- treats
dimnames(Ps)[[2]] <- c("p-value")
Ps

# 2) same covariance structure (= same sigma^2)

Var <- NULL
for(i in 1:(g*b))
{
  Var <- rbind(Var, var(target[treatments == treats[i]]))
}
dimnames(Var)[[1]] <- treats
dimnames(Var)[[2]] <- c("sigma^2")
Var

Pvt <- NULL
rownames <- NULL
for(i in 1:((g*b)-1))
{
  for(j in (i+1):(g*b))
  {
    Pvt <- rbind(Pvt, var.test(target[treatments == treats[i]], target[treatments == treats[j]])$p.value)
    rownames <- cbind(rownames, paste(treats[i], "v", treats[j]))
  }
}
dimnames(Pvt)[[1]] <- rownames
dimnames(Pvt)[[2]] <- c("p-value")
Pvt

bartlett.test(target, treatments)


##### Fit -----

fit.aov2.int <- aov(target ~ treatment1 + treatment2 + treatment1:treatment2)
summary.aov(fit.aov2.int)

fit.aov2.ad <- aov(target ~ treatment1 + treatment2)
summary.aov(fit.aov2.ad)

# Note: These aren't the only tests we can do!
# Example: global test for the joint significance of the two treatments 
#          (model without interaction)
SS.treat1 <- sum(n * b * (M.treat1 - M) ^ 2)              
SS.treat2 <- sum(n * g * (M.treat2 - M) ^ 2)             
SSres <- sum((target - M) ^ 2) - (SS.treat1 + SS.treat2)   

Ftot <- ((SS.treat1 + SS.treat2) / (g - 1 + b - 1)) / (SSres / (n * g * b - g - b + 1))
Ptot <- 1 - pf(Ftot, (g - 1) + (b - 1), n * g * b - g - b + 1) 
Ptot


##### Confidence Intervals -----

alpha <- 0.10
k <- (g*b)*(g*b-1)/2 # Complete Model
k <- (g*b)*(g*b-1)/2 - (g+b)*(g*b-g-b+1)/2 # Additive Model
dofs <- g*b*(n - 1) # Complete Model
dofs <- g*b*n - g-b+1 # Additive Model

qT <- qt(1 - alpha/(2*k), dofs)

Mediag <- tapply(target, treatments, mean)
SSres <- sum(residuals(fit) ^ 2)
S <- SSres / dofs

CI.range <- NULL
for(i in 1:((g*b)-1)) 
{
  for(j in (i+1):(g*b)) 
  {
    print(paste(treats[i], "-", treats[j]))        
    print(as.numeric(c(Mediag[i] - Mediag[j] - qT * sqrt(S * (1/n + 1/n)),
                       Mediag[i] - Mediag[j] + qT * sqrt(S * (1/n + 1/n)))))
    CI.range <- rbind(CI.range, as.numeric(c(Mediag[i]-Mediag[j] - qT * sqrt(S * (1/n + 1/n)),
                                             Mediag[i]-Mediag[j] + qT * sqrt(S * (1/n + 1/n)))))
  }
}

h <- 1
plot(c(1, (g*b)*(g*b-1)/2), range(CI.range), pch = '', xlab = 'Pairs Treat', ylab = 'Conf. Int. tau Target')
for(i in 1:((g*b)-1)) 
{
  for(j in (i+1):(g*b)) 
  {
    lines (c(h, h), c(CI.range[h, 1], CI.range[h, 2]), col = 'grey55'); 
    points(h, Mediag[i] - Mediag[j], pch = 16, col = 'grey55'); 
    points(h, CI.range[h, 1], col = rainbow(g*b)[j], pch = 16); 
    points(h, CI.range[h, 2], col = rainbow(g*b)[i], pch = 16); 
    h <- h + 1
  }
}
abline(h = 0)


##### Reduced additive model (ANOVA one-way, b=2) -----

# X.jk = mu + beta.j + eps.jk; eps.jk~N(0,sigma^2), 
#     j=1,2 (effect treatment2)
fit.aov1 <- aov(target ~ treatment2)
summary.aov(fit.aov1)

SSres <- sum(residuals(fit.aov1)^2)

# Interval at 90% for the differences (reduced additive model)
# [b=2, thus one interval only]
IC <- c(diff(M.treat2) - qt(0.95, (n*g-1)*b) * sqrt(SSres/((n*g-1)*b) *(1/(n*g) + 1/(n*g))), 
        diff(M.treat2) + qt(0.95, (n*g-1)*b) * sqrt(SSres/((n*g-1)*b) *(1/(n*g) + 1/(n*g))))
names(IC) <- c('Inf', 'Sup')
IC # IC for mu(j=1)-mu(j=2)


#### Two-ways MANOVA ----

treatment1 <- factor(data$treatment1, labels = c('F', 'T')) 
treatment2 <- factor(data$treatment2, labels = c('F', 'T')) 

treatments <- factor(paste(treatment1, treatment2, sep=''))
treatments

target <- data[, 1:2] # variables

g <- length(levels(treatment1))
b <- length(levels(treatment2))
p <- dim(target)[2]
n <- dim(target)[1]/(g*b) # group sizes
N <- n*g*b
treats <- levels(treatments)

idx <- NULL
for(i in 1:(g*b))
{
  idx <- rbind(idx, which(treatments == treats[i]))
}
dimnames(idx)[[1]] <- treats
idx


##### Visualize Data -----

for(i in 1:p)
{
  layout(matrix(c(1, 1, 2, 3), 2, byrow = T))
  boxplot(target[, i] ~ treatments, main = paste('Model with Interac. T1+T2 (', colnames(target)[i], ')', sep = ''),
          ylab = colnames(target)[i], col = 'grey95')
  boxplot(target[, i] ~ treatment1, main = 'Only Treatment 1', ylab = colnames(target)[i], col = c('red','blue'))
  boxplot(target[, i] ~ treatment2, main = 'Only Treatment 2', ylab = colnames(target)[i], col = c('forestgreen','gold'))
}

par(mfrow=c(1,1))


##### Verify Assumptions -----

# 1)  normality (multivariate) in each group
Ps <- NULL
for(i in 1:(g*b)) {
  mvn.test <- mvn(data = target[idx[i, ], ])
  Ps <- rbind(Ps, mvn.test$multivariateNormality$`p value`)
}
dimnames(Ps)[[1]] <- treats
dimnames(Ps)[[2]] <- c("p-value")
Ps

# 2) same covariance structure (= same covariance matrix Sigma)

covs <- list()
covs[["S"]] <- cov(target)
for(i in 1:(g*b))
{
  cov <- cov(target[idx[i, ],])
  covs[[paste("S", i, sep = '')]] <- cov
}
covs

# Box's M test for homogeneity of covariance matrices
# Should be done only if n_i > ~20 for all i, p < 5 and g < 5
# WARNING: Very sensitive to departure from normality and too restrictive for MANOVA
# especially if we have a high number of samples

summary(boxM(target, treatments)) # !library(heplots)
boxM(target, treatments)$p.value

# Heatmap on the covariance matrices

par(mfrow=c(1,(g*b)))
quant <- NULL
for(i in 2:((g*b)+1))
{
  quant <- rbind(quant, covs[[i]])
}
for(i in 1:(g*b))
{
  Si <- paste("S", i, sep = '')
  image(covs[[Si]], col = heat.colors(100), main = paste('Cov.', Si), asp = 1, axes = FALSE, breaks = quantile(quant, (0:100)/100, na.rm = TRUE))
}
par(mfrow=c(1,1))


##### Fit -----

fit <- manova(as.matrix(target) ~ treatment1 + treatment2 + treatment1:treatment2)
summary.manova(fit)

fit2 <- manova(as.matrix(target) ~ treatment1 + treatment2)
summary.manova(fit2)

summary.aov(fit2)


##### Confidence Intervals -----

alpha <- 0.10
k <- p*(g*b)*(g*b-1)/2 # Complete Model
k <- p*((g*b)*(g*b-1)/2 - (g+b)*(g*b-g-b+1)/2) # Additive Model
dofs <- g*b*(n - 1) # Complete Model
dofs <- g*b*n - g-b+1 # Additive Model

W <- summary.manova(fit)$SS$Residuals # Complete Model
W <- summary.manova(fit2)$SS$Residuals # Additive Model
qT <- qt(1 - alpha/(2*k), dofs)

means <- NULL
for(i in 1:(g*b)) {
  means <- rbind(means, sapply(target[idx[i, ], ], mean)) 
}
dimnames(means)[[1]] <- treats
means

inf <- list()
sup <- list()
CI <- list()
pairs <- combn((g*b), 2)

for (i in 1:ncol(pairs)) {
  idx1 <- pairs[1, i]
  idx2 <- pairs[2, i]
  
  m1 <- means[idx1, ]
  m2 <- means[idx2, ]
  
  inf_val <- m1 - m2 - qT * sqrt(diag(W) / dofs * (1/n + 1/n))
  sup_val <- m1 - m2 + qT * sqrt(diag(W) / dofs * (1/n + 1/n))
  
  inf[[paste("inf", idx1, idx2, sep = '')]] <- inf_val
  sup[[paste("sup", idx1, idx2, sep = '')]] <- sup_val
  
  CI[[paste(treats[idx1], "_", treats[idx2], sep = '')]] <- cbind(inf_val, sup_val)
}

CI

par(mfrow=c(2,p))
for(i in 1:p)
{
  boxplot(target[, i] ~ treatments, main = colnames(target)[i], ylim = range(target), col = rainbow(g))
}

for(k in 1:p)
{
  plot(c(1, (g*b)*((g*b) - 1)/2), pch = '', 
       xlim = c(1, (g*b)*((g*b) - 1)/2), ylim = c(range(CI)[1] - 3, range(CI)[2] + 3),
       xlab = 'Pairs Treat', ylab = paste('CI tau', k), 
       main = paste('CI tau', colnames(target)[k]))
  for(i in 1:((g*b)*((g*b) - 1)/2))
  {
    j <- pairs[1, i]
    l <- pairs[2, i]
    lines(c(i, i), c(CI[[i]][k, 1], CI[[i]][k, 2]))
    points(i, means[j, k] - means[l, k], pch = 16); 
    points(i, CI[[i]][k, 1], col = rainbow(g*b)[l], pch = 16); 
    points(i, CI[[i]][k, 2], col = rainbow(g*b)[j], pch = 16); 
  }
  abline(h = 0)
}

par(mfrow=c(1,1))


###---------------------------###
### SUPERVISED CLASSIFICATION ###----------------------------------------------------------
###---------------------------###

groups <- factor(data$class, labels = c('N','P'))
col.lab <- ifelse(groups %in% c("P"), 'red', 'blue')

neg <- which(groups == "N")
pos <- which(groups == "P")

n <- dim(data)[1]

#### LDA & QDA ----

##### Univariate Case -----

target <- data$target

plot(target, rep(0, n), col = col.lab, pch = 20)


###### Verify Assumptions ------

# 1) normality (univariate) within the groups (although LDA is robust to it)
shapiro.test(target[neg])
shapiro.test(target[pos])

# 2) equal variance (univariate)
var.test(target[neg], target[pos])
# If OK -> LDA, else -> QDA


###### Perform Classification ------

c.np <- 100000
c.pn <- 500

p.pos <- 0.001
p.neg <- 1-0.001
prior = c(p.neg, p.pos)
prior

prior.c <- c(p.neg*c.pn/(c.np*p.pos+c.pn*p.neg), p.pos*c.np/(c.np*p.pos+c.pn*p.neg))
prior.c

x <- data.frame(target = seq(range(target)[1] - diff(range(target))/5, range(target)[2] + diff(range(target))/5, diff(range(target))/100))

# Without priors

lda <- lda(data.frame(target), groups) #!library(MASS)
lda
# Note: if we don't specify the prior probabilities, they are estimated from the sample

# Posterior probability for a grid of x's

head(predict(lda, x)$posterior)
LDA.F <- predict(lda, x)$posterior[, 1]
LDA.T <- predict(lda, x)$posterior[, 2]

plot(target[neg], rep(0, length(neg)), pch = 16, col = 'blue', xlim = range(x), ylim = c(0, 1),
     xlab = 'x', ylab = 'estimated posterior', main = "LDA")
points(target[pos], rep(0, length(pos)), pch = 16, col = 'red')
abline(v = mean(range(target)), col = 'grey')
points(c(mean(range(target)), mean(range(target))), c(predict(lda, data.frame(target = mean(range(target))))$posterior),
       col = c('blue', 'red'), pch = '*', cex = 2.5)

predict(lda, x)$class
predict(lda, mean(range(target)))$class

lines(x[, 1], LDA.F, type = 'l', col = 'blue',
      xlab = 'x', ylab = 'estimated posterior', main = "LDA")
lines(x[, 1], LDA.T, type = 'l', col = 'red')
abline(h = 0.5)
legend(range(target)[1], 1, legend = c('P(NEG|X=x)', 'P(POS|X=x)'), fill = c('blue', 'red'), cex = 0.7)

# Set prior probabilities

lda.p <- lda(data.frame(target), groups, prior = prior.c)
lda.p

LDA.F.p <- predict(lda.p, x)$posterior[,1]
LDA.T.p <- predict(lda.p, x)$posterior[,2]

plot(x[, 1], LDA.F.p, type = 'l', col = 'blue', xlim = range(x), ylim = c(0,1),
     xlab = 'x', ylab = 'estimated posterior', main = "LDA")
points(x[ ,1], LDA.T.p, type = 'l', col = 'red')
abline(h = 0.5)
points(target[neg], rep(0, length(neg)), pch = 16, col = 'blue')
points(target[pos], rep(0, length(pos)), pch = 16, col = 'red')
abline(v = mean(range(target)), col = 'grey')
points(c(mean(range(target)), mean(range(target))), c(predict(lda.p, data.frame(target = mean(range(target))))$posterior),
       col = c('blue', 'red'), pch = '*', cex = 2.5)

points(x[, 1], LDA.F, type = 'l', col = 'grey')
points(x[, 1], LDA.T, type = 'l', col = 'grey')
legend(range(target)[1], 1, legend = c('P(NEG|X=x)', 'P(POS|X=x)'), fill = c('blue', 'red'), cex = 0.7)


##### Multivariate Case -----

target <- data[, 1:4]
p <- dim(target)[2]

plot(target, col = col.lab, pch = 20)


###### Verify Assumptions ------

mvn(target[neg, ])$multivariateNormality
mvn(target[pos, ])$multivariateNormality

# Normality is not respected for the "low" group

Pvt <- NULL
Pbart <- NULL
rownames <- NULL
for(i in 1:p)
{
    Pvt <- rbind(Pvt, var.test(target[neg, i], target[pos, i])$p.value)
    Pbart <- rbind(Pbart, bartlett.test(target[, i], groups)$p.value)
    rownames <- cbind(rownames, paste(colnames(target)[i], "_F vs ", colnames(target)[i], "_T", sep = ''))
}
dimnames(Pvt)[[1]] <- rownames
dimnames(Pbart)[[1]] <- rownames
dimnames(Pvt)[[2]] <- c("p-value")
dimnames(Pbart)[[2]] <- c("p-value")
Pvt
Pbart
# Note: when groups are 2, var.test is basically the same as bartlett.test!

summary(boxM(target, groups)) # !library(heplots)
boxM(target, groups)$p.value

list(SF = cov(target[neg, ]), ST = cov(target[pos, ]))
abs(cov(target[neg, ])/cov(target[pos, ]))


###### Perform Classification ------

lda <- lda(target, groups, prior = prior.c)
lda


###### Evaluation ------

lda.pred <- predict(lda, target)

G <- 2

misc <- table(class.true = groups, class.assigned = lda.pred$class)
APER <- 0
for(g in 1:G)
  APER <- APER + sum(misc[g,-g])/sum(misc[g,]) * prior[g] # priors NON adjusted! p(pos)p(miscl.|pos) + p(neg)p(miscl.|neg)

print(paste("APER:", APER))

ldaCV <- lda(target, groups, CV = TRUE, prior = prior.c)

miscCV <- table(class.true = groups, class.assigned = ldaCV$class)
AERCV <- 0
for(g in 1:G)
  AERCV <- AERCV + sum(miscCV[g,-g])/sum(miscCV[g,]) * prior[g] # priors NON adjusted! p(pos)p(miscl.|pos) + p(neg)p(miscl.|neg)

print(paste("AERCV:", AERCV))


###### Plot Classification Regions ------

# (Second variable against the first)

plot(target[, 1:2], xlab = colnames(target)[1], ylab = colnames(target)[2], pch = '')
points(target[neg, 1:2], col = 'blue', pch = 20)
points(target[pos, 1:2], col = 'red', pch = 20)
legend('topright', legend = levels(groups), fill = c('blue', 'red'), cex = .7)

points(lda$means[, 1:2], pch = 4, col = c('blue', 'red') , lwd = 2, cex = 1.5)

x <- seq(min(target[, 1]), max(target[, 1]), length = 200)
y <- seq(min(target[, 2]), max(target[, 2]), length = 200)
xy <- expand.grid(V1 = x, V2 = y)

xy$V3 <- mean(target[, 3]) # !!! Do it only if p > 2 !!! #
xy$V4 <- mean(target[, 4]) # !!! Do it only if p > 2 !!! #
# Fix reasonably the coordinates of variables you don't want to plot 

colnames(xy) <- colnames(target)

post <- predict(lda, xy)$post  
post1 <- post[, 1] - post[, 2] 
post2 <- post[, 2] - post[, 1]  

contour(x, y, matrix(post1, 200), levels = 0, drawlabels = F, add = T)  
contour(x, y, matrix(post2, 200), levels = 0, drawlabels = F, add = T)


##### Estimates -----

TN <- miscCV[1, 1]
FN <- miscCV[2, 1]
FP <- miscCV[1, 2]
TP <- miscCV[2, 2]

total <- 200

rbind("Budget", ((FP / (TN + FP)) * total * p.neg + (TP / (TP + FN)) * total * p.pos) * c.pn)

prev_strategy_cost <- total * c.pn
cur_strategy_cost <- (FP / (TN + FP)) * total * p.neg * c.pn + (FN / (TP + FN)) * total * p.pos * c.np

rbind("Savings", prev_strategy_cost - cur_strategy_cost)


#### Support Vector Machines (SVM) ----

##### Linear Case -----

target <- data[, c(1, 4)]

# The classes are not separable
plot(target, col = col.lab, pch = 19, asp = 1)

# Fit the Support Vector Classifier (kernel = "linear")
dat <- data.frame(x = target, y = groups)
svmfit <- svm(y~., data = dat, kernel = 'linear', cost = 10, scale = FALSE) #!library(e1071)
summary(svmfit)

par(mfrow=c(1,2))
plot(svmfit, dat, col = c('salmon', 'light blue'), pch = 19)
par(mfrow=c(1,1))

# Support vectors are indicated with crosses.
# They are:
svmfit$index

n.g <- 100

xgrid <- expand.grid(x.1 = seq(from = range(dat[, 1])[1], to = range(dat[, 1])[2],length = n.g),
                     x.2 = seq(from = range(dat[, 2])[1], to = range(dat[, 2])[2],length = n.g))
colnames(xgrid) <- colnames(dat)[1:2]
ygrid <- predict(svmfit, xgrid)
ygrid <- factor(ygrid, labels = c(0, 1)) # not necessary afterwards

plot(xgrid, col = c("blue", "red")[as.numeric(ygrid)], pch = 20, cex = .2)
points(target, col = col.lab, pch = 19)
points(target[svmfit$index, ], pch = 5, cex = 2)

plot(target, col = col.lab ,pch = 19)
contour(seq(from = range(dat[, 1])[1], to = range(dat[, 1])[2], length = n.g),
        seq(from = range(dat[, 2])[1], to = range(dat[, 2])[2], length = n.g),
        matrix(as.numeric(ygrid), n.g, n.g), level = 1.5, add = TRUE, drawlabels = F)

# Prediction for a new observation (command predict())

testdat <- data.frame(x = cbind(6.75, 6) , y = as.factor("N"))
colnames(testdat)[1:2] <- colnames(dat)[1:2]

ypred <- predict(svmfit, testdat)
table(true.label = testdat$y, assigned.label = ypred)

points(testdat[, 1:2], col = c("blue", "red")[as.numeric(testdat$y)], pch = 4, lwd = 2, cex = 1.5)

# To set the parameter C we can use the function tune(),
# which is based on cross-validation (10-fold)
set.seed(1)
tune.out <- tune(svm, y~., data = dat, kernel = 'linear',
                 ranges = list(cost = c(0.001 , 0.01, 0.1, 1, 5, 10, 100)))
summary(tune.out)

# Extract the best model from the result of tune
bestmod <- tune.out$best.model
summary(bestmod)

# If the classes are separable, setting a high value for the cost function
# leads to the maximal margin classifier (i.e., it returns the classification
# provided by the best separating hyperplane)


##### Non-linear Case -----

target <- data[, c(2, 4)]

plot(target, col = col.lab, pch = 19, asp = 1)

# Fit the Support Vector Classifier (kernel = "radial") given a cost C
dat <- data.frame(x = target, y = groups)
svmfit <- svm(y~., data = dat, kernel = 'radial', gamma = 1, cost = 10)
summary(svmfit)

par(mfrow=c(1,2))
plot(svmfit, dat, col = c('salmon', 'light blue'), pch = 19)
par(mfrow=c(1,1))


###------------###
### CLUSTERING ###----------------------------------------------------------
###------------###

n <- dim(data)[1]


#### Hierarchical Clustering ----

# Dissimilarity Matrix (available metrics: euclidean, manhattan, canberra)

dist <- dist(data, method = 'euclidean')
image(1:n, 1:n, as.matrix(dist), asp = 1, xlab = 'i', ylab = 'j')

# Dissimilarity Between Clusters (available methods: single, complete, average, ward.D2)

HC <- hclust(dist, method = 'ward.D2')

names(HC)
HC$merge  # order of aggregation of statistical units / clusters
HC$height # distance at which we have aggregations
HC$order  # ordering that allows to avoid intersections in the dendrogram

# Dendrogram

plot(HC, main = paste(HC$dist.method, "-" , HC$method, sep = ''), hang = -0.1, sub = '', labels = F, xlab = '')

k <- 2 # number of clusters
rect.hclust(HC, k = k)

# Clustering

cluster <- cutree(HC, k = k)
table(cluster)

plot(data, col = cluster + 1, pch = 16, lwd = 2)

means <- NULL
for(i in 1:length(unique(cluster)))
{
  means = rbind(means, sapply(data[cluster == i,], mean))
}

means
points(means, col = sort(unique(cluster)) + 1, pch = '*', cex = 4)

# Cophenetic Coefficient

coph <- cophenetic(HC)
cor(dist, coph)

par(mfrow=c(1,2))
image(as.matrix(dist), main = HC$dist.method)
image(as.matrix(coph), main = HC$method)
par(mfrow=c(1,1))


##### Choice of k (W(k)/SS_tot Plot) -----

w <- NULL
w_i <- NULL
mean <- colMeans(data)

for(k in 1:10)
{
  cluster <- cutree(HC, k = k)
  w_i <- NULL
  means <- NULL
  for(i in 1:length(unique(cluster)))
  {
    means <-  rbind(means, sapply(data[cluster == i, ], mean))
  }
  mean_bar <- colMeans(means)
  for(i in 1:length(unique(cluster)))
  {
    cluster_i_centered_rel <- data[cluster == i, ]
    cluster_i_centered_rel[, 1] <- data[cluster == i, 1] - means[i, 1]
    cluster_i_centered_rel[, 2] <- data[cluster == i, 2] - means[i, 2]
    w_i <- c(w_i, sum(cluster_i_centered_rel^2))
  }
  w <- c(w, sum(w_i))
}

data_centered <- data
data_centered[, 1] <- data[, 1] - mean[1]
data_centered[, 2] <- data[, 2] - mean[2]

t <- sum(data_centered^2)

matplot(1:10, w/t, pch = '', xlab = 'clusters', ylab = 'within/tot', main = 'Choice of k', ylim = c(0, 1))
lines(1:10, w/t, type = 'b', lwd = 2)


#### K-Means ----

k <- 3
result.k <- kmeans(data, centers = k) # Centers: fixed number of clusters

names(result.k)

result.k$cluster      # labels of clusters
result.k$centers      # centers of the clusters
result.k$totss        # tot. sum of squares
result.k$withinss     # sum of squares within clusters
result.k$tot.withinss # sum(sum of squares within cluster)
result.k$betweenss    # sum of squares between clusters
result.k$size         # dimension of the clusters

plot(data, col = result.k$cluster + 1, pch = 16, lwd = 2)
points(result.k$centers, col = sort(unique(result.k$cluster)) + 1, pch = '*', cex = 4)


##### Choice of k (W(k)/SS_tot Plot) -----

b <- NULL
w <- NULL
for(k in 1:10)
{
  result.k <- kmeans(data, k)
  w <- c(w, sum(result.k$wit))
  b <- c(b, result.k$bet)
}

matplot(1:10, w/(w+b), pch = '', xlab = 'clusters', ylab = 'within/tot', main = 'Choice of k', ylim = c(0, 1))
lines(1:10, w/(w+b), type = 'b', lwd = 2)


#### DBSCAN ----

# Choice of hyperparameters for DBSCAN
# Rule of thumb, minPts should be at least p + 1
# Can be eventually increased
minPts <- 3

# How to choose eps from minPts?
# Plot of the distances to the minPts-1 nearest neighbor
kNNdistplot(data, minPts = minPts) #!library(dbscan)
# Set a good threshold
abline(h = 2.75, col = "red", lty = 2)
threshold <- 2.75

# Run the dbscan
dbs <- dbscan(data, eps = threshold, minPts = minPts)
dbs

# Plot of the resulting clustering
plot(data, col = dbs$cluster + 1, pch = 16, lwd = 2)


##### Silhouette scores -----

# How to tune the algorithm and find the "best" eps and minPts?
# Silhouette score (from the package "cluster")
help(silhouette) #!library(cluster)

# Let's compute the silhouette score on the clustering performed before
# WARNING (specific to DBSCAN): We need to remove the noise points as they do
# not belong to a cluster, before computing the silhouette score
clustered_index <- which(dbs$cluster != 0) # Index of non noise points
clustered_points <- data[clustered_index, ] # only clustered points
clustered_labels <- dbs$cluster[clustered_index] # corresponding labels

# Silhouette coefficient is calculated for each unit i in the following way:
# sil(i) = (b(i)-a(i)) / max{a(i),b(i)} [it ranges from -1 to 1]
# where: - a(i) is the average distance of i to a point of the SAME cluster
#        - b(i) is the average distance of i to a point of the NEAREST (different) cluster
sil <- silhouette(clustered_labels, dist(clustered_points))
summary(sil)

sil_score <- function(labels, dist) {
  # Compute the average of the silhouette widths
  sil <- silhouette(labels, dist)
  sil_widths <- sil[, "sil_width"]
  mean(sil_widths)
}

sil_score(clustered_labels, dist(clustered_points))

# Grid Search
minPts_grid <- 1:20
eps_grid <- seq(threshold/5, threshold*3, length = 20)

max_share_noise <- 0.2

dbscan_perf <- function(minPts, eps) {
  # Compute the silhouette score resulting from dbscan clustering
  dbs <- dbscan(data, eps, minPts) # Run dbscan
  
  clustered_index <- which(dbs$cluster != 0) # Index of non noise points
  clustered_points <- data[clustered_index, ] # only clustered points
  clustered_labels <- dbs$cluster[clustered_index] # corresponding labels
  nb_clusters <- length(unique(clustered_labels))
  
  if ((nb_clusters > 1 & nb_clusters < n) & (length(which(dbs$cluster == 0))/n < max_share_noise)) { 
    # Silhouette score is defined only if 2 <= nb_clusters <= n-1
    sil_score(clustered_labels, dist(clustered_points))
  }
  
  else {
    # otherwise we return 0 which would be the approx. value of the silhouette
    # score if the clusters were completely overlapping
    0
  }
}

# We compute the silhouette score for all combinations of minPts and eps
perf_grid <- outer(minPts_grid, eps_grid, FUN = Vectorize(dbscan_perf))
dimnames(perf_grid) <- list(minPts_grid, eps_grid)

# Histogram of the Silhouette scores
hist(perf_grid, breaks = 20, xlab = "Silhouette score", xlim = c(-1, 1), main = NULL)

max_score <- max(perf_grid)
min_score <- min(perf_grid)
max_abs <- max(abs(max_score), abs(min_score))

image.plot(x = eps_grid, y = minPts_grid, z = perf_grid, xlab = "eps", ylab = "minPts",
           main = 'Silhouette score', col = hcl.colors(64, palette = 'Blue-Red'),
           breaks = c(seq(-max_abs, 0, length = 33)[-33], seq(0, max_abs, length = 33))) #!library(fields)
par(mfrow=c(1,1))

# Retrieve best parameter values
max_score <- max(perf_grid)
argmax_score <- which(perf_grid == max_score, arr.ind = TRUE)
best_eps <- eps_grid[argmax_score[2]]
best_minPts <- minPts_grid[argmax_score[1]]
best_eps
best_minPts
max_score

# Run the dbscan
dbs <- dbscan(data, best_eps, best_minPts)
dbs

plot(data, col = dbs$cluster + 1, pch = 16, lwd = 2)


#### Multidimensional Scaling ----

# Given the distances (dissimilarities) among n statistical units, look for 
# the k-dimensional representation (k small) of the n statistical units
# such that the distances (dissimilarities) among the representations
# of the n units are as close as possible to the original distances
# (dissimilarities) among the n units.

help(eurodist)
dist <- eurodist

mds <- cmdscale(dist, k = 2)
mds

plot(mds[, 1], mds[, 2], type = 'n', asp = 1, axes = FALSE, main = "MDS", xlab = '', ylab = '')
text(mds[, 1], mds[, 2], labels = colnames(as.matrix(dist)), cex = 0.75, pos = 3)

# Compare the original matrix d_ij = d(x_i,x_j) and delta_ij = d(y_i,y_j) 
plot(dist, dist(mds))

# Visualize the most different distances
par(cex = 0.75, mar = c(10, 10, 2, 2))
image(1:21, 1:21, asp = 1, abs(as.matrix(dist(mds)) - as.matrix(dist)), axes = F, xlab = '', ylab ='')
axis(1, at = 1:21, labels = colnames(as.matrix(dist)), las = 2, cex = 0.75)
axis(2, at = 1:21, labels = colnames(as.matrix(dist)), las = 1, cex = 0.75)
box()

# Rome-Athens
as.matrix(dist)[19,1]
as.matrix(dist(mds))[19,1]

# Cologne-Geneve
as.matrix(dist)[6,8]
as.matrix(dist(mds))[6,8]


# Compute the "stress": the higher it is, the worse
# the matching between original distances and their
# geometrical representation through MDS
Stressk <- NULL
for(k in 1:4)
{
  mds.k <- cmdscale(dist, k)
  Stress <- (sum((as.vector(dist) - as.vector(dist(mds.k)))^2) / sum(as.vector(mds.k)^2))^(1/2)
  Stressk <- c(Stressk, Stress) 
}

plot(1:4, Stressk, xlab = 'k', ylab = 'Stress', lwd = 2)
lines(1:4, Stressk, type = 'b', lwd = 2)
