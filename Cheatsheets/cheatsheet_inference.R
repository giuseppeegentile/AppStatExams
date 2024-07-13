###--------------------------###
### INFERENCE ABOUT THE MEAN ###-------------------------------------------
###--------------------------###

#### Paired Data ----

### Try It Out! ###
rm(list = ls())
graphics.off()
par(mfrow=c(1,1))
cat("\014")
data <- read.table('datasets/data_paired.txt', header = T)
### Try It Out! ###

D <- data.frame(DF1 = data$feature1_obs1 - data$feature1_obs2,
                DF2 = data$feature2_obs1 - data$feature2_obs2) # may be useful to substitute DF1 w/ feature1's name
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

# Simultaneous T2 intervals

T2.I <- cbind(D.mean - sqrt(cfr.fisher * diag(D.cov)/n),
              D.mean,
              D.mean + sqrt(cfr.fisher * diag(D.cov)/n))
dimnames(T2.I)[[2]] <- c('inf', 'center', 'sup')
T2.I

# or

T2.I <- NULL
for(i in 1:p)
{
  T2.I.DFi <- cbind(D.mean[i] - sqrt(cfr.fisher * D.cov[i, i] / n),
                    D.mean[i],
                    D.mean[i] + sqrt(cfr.fisher * D.cov[i, i] / n))
  T2.I <- rbind(T2.I, T2.I.DFi)
}
dimnames(T2.I)[[2]] <- c('inf', 'center', 'sup')
T2.I

# Bonferroni intervals

k <- p
cfr.t <- qt(1 - alpha/(2*k), n-1)

BF.I <- cbind(D.mean - cfr.t * sqrt(diag(D.cov)/n),
              D.mean,
              D.mean + cfr.t * sqrt(diag(D.cov)/n))
dimnames(BF.I)[[2]] <- c('inf', 'center', 'sup')
BF.I

# or

BF.I <- NULL
for(i in 1:p)
{
  BF.I.DFi <- cbind(D.mean[i] - cfr.t * sqrt(D.cov[i, i] / n),
                    D.mean[i],
                    D.mean[i] + cfr.t * sqrt(D.cov[i, i] / n))
  BF.I <- rbind(BF.I, BF.I.DFi)
}
dimnames(BF.I)[[2]] <- c('inf', 'center', 'sup')
BF.I

# Confidence Interval for the Variance

k <- 1 # eventual Bonferroni correction

Var.I <- cbind((n-1) * diag(D.cov) / qchisq(1 - (alpha)/(2*k), n-1),
               diag(D.cov),
               (n-1) * diag(D.cov) / qchisq(alpha/(2*k), n-1))
dimnames(Var.I)[[2]] <- c('inf', 'center', 'sup')
Var.I


##### Confidence Region -----

plot(D, asp = 1, pch = 1, 
     xlim = c(min(D[, 1]) - 2, max(D[, 1]) + 2),
     main = 'Dataset of the Differences')

abline(h = delta.0[1], v = delta.0[2], col = 'grey35', lty = 2)
points(delta.0[1], delta.0[2], col = 'red', pch = 9, cex = 1)

ellipse(center = D.mean, shape = D.cov/n, radius = sqrt(cfr.fisher), lwd = 2, lty = 2, col = 'blue')

abline(v = T2.I[1, 1], col = 'orange', lwd = 1, lty = 2)
abline(v = T2.I[1, 3], col = 'orange', lwd = 1, lty = 2)
abline(h = T2.I[2, 1], col = 'orange', lwd = 1, lty = 2)
abline(h = T2.I[2, 3], col = 'orange', lwd = 1, lty = 2)

segments(T2.I[1, 1], 0, T2.I[1, 3], 0, lty = 1, lwd = 2, col = 'orange')
segments(0, T2.I[2, 1], 0, T2.I[2, 3], lty = 1, lwd = 2, col = 'orange')

abline(v = BF.I[1, 1], col = 'purple', lwd = 1, lty = 2)
abline(v = BF.I[1, 3], col = 'purple', lwd = 1, lty = 2)
abline(h = BF.I[2, 1], col = 'purple', lwd = 1, lty = 2)
abline(h = BF.I[2, 3], col = 'purple', lwd = 1, lty = 2)

segments(BF.I[1, 1], 0, BF.I[1, 3], 0, lty = 1, lwd = 2, col = 'purple')
segments(0, BF.I[2, 1], 0, BF.I[2, 3], lty = 1, lwd = 2, col = 'purple')

legend('topright', c('Bonf. CI', 'Sim-T2 CI'), col = c('purple', 'orange'), lty = 1, lwd = 2)

worst <- D.invcov %*% (D.mean - delta.0)
worst <- worst / sqrt(sum(worst ^ 2))
worst

CI.worst <- c(D.mean %*% worst - sqrt(cfr.fisher * (t(worst) %*% D.cov %*% worst) / n),
              D.mean %*% worst,
              D.mean %*% worst + sqrt(cfr.fisher * (t(worst) %*% D.cov %*% worst) / n))
CI.worst
delta.0 %*% worst
(CI.worst[1] < delta.0 %*% worst) & (delta.0 %*% worst < CI.worst[3])   

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

### Try It Out! ###
rm(list = ls())
graphics.off()
par(mfrow=c(1,1))
cat("\014")
data <- read.table('datasets/data_repeated.txt', header = T)
### Try It Out! ###

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

mvn(t(C %*% t(data)))$multivariateNormality$'p value'

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
dimnames(T2.I)[[2]] <- c('inf', 'center', 'sup')
T2.I

# Bonferroni intervals

k <- q-1   # number of increments (i.e., dim(C)[1])
cfr.t <- qt(1 - alpha/(2*k), n-1)

BF.I <- cbind(Md - cfr.t * sqrt(diag(Sd)/n),
              Md,
              Md + cfr.t * sqrt(diag(Sd)/n))
dimnames(BF.I)[[2]] <- c('inf', 'center', 'sup')
BF.I

# Confidence Interval for the Variance

k <- 1 # eventual Bonferroni correction

Var.I <- cbind((n-1) * diag(Sd) / qchisq(1 - (alpha)/(2*k), n-1),
               diag(Sd),
               (n-1) * diag(Sd) / qchisq(alpha/(2*k), n-1))
dimnames(Var.I)[[2]] <- c('inf', 'center', 'sup')
Var.I


##### Plotting Intervals -----

matplot(t(matrix(1:(q-1), 3, 3)), t(BF.I), type = 'b', pch = '', xlim = c(0, 4), xlab = '', ylab = '', main = 'Confidence Intervals')

segments(matrix(1:(q-1), 3, 1), BF.I[, 1], matrix(1:3, 3, 1), BF.I[, 3],
         col = 'purple', lwd = 2)
points(1:(q-1), BF.I[, 2], col = 'purple', pch = 16)

points(1:(q-1)+.05, delta.0, col='black', pch = 16)

segments(matrix(1:(q-1)+.1, 3, 1), T2.I[, 1], matrix(1:(q-1)+.1, 3, 1), T2.I[, 3], col = 'orange', lwd = 2)
points(1:(q-1)+.1, T2.I[, 2], col = 'orange', pch = 16)

abline(h = 0, col = 'grey', lty = 2)

legend('topright', c('Bonf. CI', 'Sim-T2 CI'), col = c('purple', 'orange'), lty = 1, lwd = 2)


##### Confidence Region (for Differences) -----

C <- matrix(c(1, -1, 0, 0, 
              1, 0, -1, 0), q-2, q, byrow = T)
C

alpha <- .05
delta.0 <- c(0, 0)

Md <- C %*% M 
Sd <- C %*% S %*% t(C) 
Sdinv <- solve(Sd)

cfr.fisher <- ((q - 1) * (n - 1) / (n - (q - 1))) * qf(1 - alpha, (q - 1), n - (q - 1)) 

diff <- cbind(data[, 1] - data[, 2], data[, 1] - data[, 3])

plot(diff[, 1], diff[, 2], asp = 1, pch = 1, 
     xlim = c(min(diff[, 1]) - 2, max(diff[, 1]) + 2),
     main = 'Dataset of the Differences t1-t3 vs t1-t2')

abline(h = delta.0[1], v = delta.0[2], col = 'grey35', lty = 2)
points(delta.0[1], delta.0[2], col = 'red', pch = 9, cex = 1)

ellipse(center = c(Md), shape = Sd/n, radius = sqrt(cfr.fisher), lwd = 2, lty = 2, col = 'blue')

T2.I <- cbind(Md - sqrt(cfr.fisher * diag(Sd)/n),
              Md,
              Md + sqrt(cfr.fisher * diag(Sd)/n))
dimnames(T2.I)[[2]] <- c('inf', 'center', 'sup')
T2.I

k <- q-2
cfr.t <- qt(1 - alpha/(2*k), n-1)

BF.I <- cbind(Md - cfr.t * sqrt(diag(Sd)/n),
              Md,
              Md + cfr.t * sqrt(diag(Sd)/n))
dimnames(BF.I)[[2]] <- c('inf', 'center', 'sup')
BF.I

abline(v = T2.I[1, 1], col = 'orange', lwd = 1, lty = 2)
abline(v = T2.I[1, 3], col = 'orange', lwd = 1, lty = 2)
abline(h = T2.I[2, 1], col = 'orange', lwd = 1, lty = 2)
abline(h = T2.I[2, 3], col = 'orange', lwd = 1, lty = 2)

segments(T2.I[1, 1], 0, T2.I[1, 3], 0, lty = 1, lwd = 2, col = 'orange')
segments(0, T2.I[2, 1], 0, T2.I[2, 3], lty = 1, lwd = 2, col = 'orange')

abline(v = BF.I[1, 1], col = 'purple', lwd = 1, lty = 2)
abline(v = BF.I[1, 3], col = 'purple', lwd = 1, lty = 2)
abline(h = BF.I[2, 1], col = 'purple', lwd = 1, lty = 2)
abline(h = BF.I[2, 3], col = 'purple', lwd = 1, lty = 2)

segments(BF.I[1, 1], 0, BF.I[1, 3], 0, lty = 1, lwd = 2, col = 'purple')
segments(0, BF.I[2, 1], 0, BF.I[2, 3], lty = 1, lwd = 2, col = 'purple')

legend('topright', c('Bonf. CI', 'Sim-T2 CI'), col = c('purple', 'orange'), lty = 1, lwd = 2)

worst <- Sdinv %*% (Md - delta.0)
worst <- worst / sqrt(sum(worst ^ 2))
worst

CI.worst <- c(t(Md) %*% worst - sqrt(cfr.fisher * (t(worst) %*% Sd %*% worst) / n),
              t(Md) %*% worst,
              t(Md) %*% worst + sqrt(cfr.fisher * (t(worst) %*% Sd %*% worst) / n))
CI.worst
delta.0 %*% worst
(CI.worst[1] < delta.0 %*% worst) & (delta.0 %*% worst < CI.worst[3])   

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

### Try It Out! ###
rm(list = ls())
graphics.off()
par(mfrow=c(1,1))
cat("\014")
data1 <- read.table('datasets/data_pop1.txt', header = T)
data2 <- read.table('datasets/data_pop2.txt', header = T)
### Try It Out! ###

n1 <- dim(data1)[1] 
n2 <- dim(data2)[1] 
p  <- dim(data1)[2] 

mvn(data1)$multivariateNormality
mvn(data2)$multivariateNormality

list(S1 = cov(data1), S2 = cov(data2))
abs(cov(data1)/cov(data2))
# We compare the matrices -> here, using rule of thumb:
# we don't reject equality of covariance matrices if s1_ii and s2_ii differ from
# less than a factor ~4 (see J-W p.291)

# Box's M test for homogeneity of covariance matrices
# Should be done only if n_i > ~20 for all i, p < 5 and g < 5
# WARNING: Very sensitive to departure from normality and too restrictive for MANOVA
# especially if we have a high number of samples

target <- rbind(as.matrix(data1), as.matrix(data2))
groups <- factor(rbind(cbind(rep(1, n1)), cbind(rep(2, n2))))

summary(boxM(target, groups)) #!library(heplots)
boxM(target, groups)$p.value

data1.mean <- sapply(data1, mean)
data2.mean <- sapply(data2, mean)
data1.cov <-  cov(data1)
data2.cov <-  cov(data2)
Sp <- ((n1 - 1) * data1.cov + (n2 - 1) * data2.cov) / (n1 + n2 - 2)

plot(data1, asp = 1, pch = 19, col = 'red', 
     xlim = c(min(min(data1[, 1]), min(data2[, 1])) - 1, max(max(data1[, 1]), max(data2[, 1])) + 1), 
     ylim = c(min(min(data1[, 2]), min(data2[, 2])) - 1, max(max(data1[, 2]), max(data2[, 2])) + 1))
for (i in 1:n2)
{
  points(data2[i, 1], data2[i, 2], col = 'blue', pch = 19)
}
points(data1.mean[1], data1.mean[2], col = 'red', pch = 11, cex = 2)
points(data2.mean[1], data2.mean[2], col = 'blue', pch = 11, cex = 2)

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

T2.I <- cbind(data1.mean - data2.mean - sqrt(cfr.fisher * diag(Sp) * (1 / n1 + 1 / n2)),
              data1.mean - data2.mean,
              data1.mean - data2.mean + sqrt(cfr.fisher * diag(Sp) * (1 / n1 + 1 / n2)))
dimnames(T2.I)[[2]] <- c('inf', 'center', 'sup')
T2.I

# or 

T2.I <- NULL
for(i in 1:p)
{
  T2.I.Fi <- cbind(data1.mean[i] - data2.mean[i] - sqrt(cfr.fisher * Sp[i, i] * (1 / n1 + 1 / n2)),
                   data1.mean[i] - data2.mean[i],
                   data1.mean[i] - data2.mean[i] + sqrt(cfr.fisher * Sp[i, i] * (1 / n1 + 1 / n2)))
  T2.I <- rbind(T2.I, T2.I.Fi)
}
dimnames(T2.I)[[2]] <- c('inf', 'center', 'sup')
T2.I

# Bonferroni intervals

k <- p
cfr.t <- qt(1 - alpha/(2*k), n1 + n2 - 2)

BF.I <- cbind(data1.mean - data2.mean - cfr.t * sqrt(diag(Sp) * (1 / n1 + 1 / n2)),
              data1.mean - data2.mean,
              data1.mean - data2.mean + cfr.t * sqrt(diag(Sp) * (1 / n1 + 1 / n2)))
dimnames(BF.I)[[2]] <- c('inf', 'center', 'sup')
BF.I

# or

BF.I <- NULL
for(i in 1:p)
{
  BF.I.Fi <- cbind(data1.mean[i] - data2.mean[i] - cfr.t * sqrt(Sp[i, i] * (1 / n1 + 1 / n2)),
                   data1.mean[i] - data2.mean[i],
                   data1.mean[i] - data2.mean[i] + cfr.t * sqrt(Sp[i, i] * (1 / n1 + 1 / n2)))
  BF.I <- rbind(BF.I, BF.I.Fi)
}
dimnames(BF.I)[[2]] <- c('inf', 'center', 'sup')
BF.I

# Confidence Interval for the Variance

k <- 1 # eventual Bonferroni correction

Var.I <- cbind((n1 + n2 - 2) * diag(Sp) / qchisq(1 - (alpha)/(2*k), n1 + n2 - 2),
               diag(Sp),
               (n1 + n2 - 2) * diag(Sp) / qchisq(alpha/(2*k), n1 + n2 - 2))
dimnames(Var.I)[[2]] <- c('inf', 'center', 'sup')
Var.I


##### Confidence Region -----

plot(data1.mean[1] - data2.mean[1], data1.mean[2] - data2.mean[2], asp = 1, pch = 19, 
     xlim = c(data1.mean[1] - data2.mean[1] - 2, data1.mean[1] - data2.mean[1] + 2))

abline(h = delta.0[1], v = delta.0[2], col = 'grey35', lty = 2)
points(delta.0[1], delta.0[2], col = 'red', pch = 9, cex = 1)

ellipse(center = (data1.mean - data2.mean), shape = Sp * (n1+n2)/(n1*n2), radius = sqrt(cfr.fisher), lwd = 2, lty = 2, col = 'blue')

abline(v = T2.I[1, 1], col = 'orange', lwd = 1, lty = 2)
abline(v = T2.I[1, 3], col = 'orange', lwd = 1, lty = 2)
abline(h = T2.I[2, 1], col = 'orange', lwd = 1, lty = 2)
abline(h = T2.I[2, 3], col = 'orange', lwd = 1, lty = 2)

segments(T2.I[1, 1], 0, T2.I[1, 3], 0, lty = 1, lwd = 2, col = 'orange')
segments(0, T2.I[2, 1], 0, T2.I[2, 3], lty = 1, lwd = 2, col = 'orange')

abline(v = BF.I[1, 1], col = 'purple', lwd = 1, lty = 2)
abline(v = BF.I[1, 3], col = 'purple', lwd = 1, lty = 2)
abline(h = BF.I[2, 1], col = 'purple', lwd = 1, lty = 2)
abline(h = BF.I[2, 3], col = 'purple', lwd = 1, lty = 2)

segments(BF.I[1, 1], 0, BF.I[1, 3], 0, lty = 1, lwd = 2, col = 'purple')
segments(0, BF.I[2, 1], 0, BF.I[2, 3], lty = 1, lwd = 2, col = 'purple')

legend('topright', c('Bonf. CI', 'Sim-T2 CI'), col = c('purple', 'orange'), lty = 1, lwd = 2)

worst <- Spinv %*% (data1.mean - data2.mean - delta.0)
worst <- worst / sqrt(sum(worst^2))
worst

CI.worst <- c((data1.mean - data2.mean) %*% worst - sqrt(cfr.fisher * (t(worst) %*% Sp %*% worst) * (1 / n1 + 1 / n2)),
              (data1.mean - data2.mean) %*% worst,
              (data1.mean - data2.mean) %*% worst + sqrt(cfr.fisher * (t(worst) %*% Sp %*% worst) * (1 / n1 + 1 / n2)))
CI.worst
delta.0 %*% worst
(CI.worst[1] < delta.0 %*% worst) & (delta.0 %*% worst < CI.worst[3])   

x.min <- CI.worst[1] * worst # (x,y) coords of the lower bound of the interval
x.max <- CI.worst[3] * worst # (x,y) coords of the upper bound of the interval
m1.ort <- - worst[1] / worst[2] # Slope of the line orthogonal to worst
q.min.ort <- x.min[2] - m1.ort * x.min[1] # Intercept of line of slope m1.ort passing by x.min
q.max.ort <- x.max[2] - m1.ort * x.max[1] # Intercept of line of slope m1.ort passing by x.max
abline(q.min.ort, m1.ort, col = 'forestgreen', lty = 2, lwd = 1)
abline(q.max.ort, m1.ort, col = 'forestgreen', lty = 2, lwd = 1)

m1 = worst[2] / worst[1] # worst direction
abline(0, m1, col = 'grey35') # Intercept 0 because the line has to pass by delta.0 which is (0, 0)
segments(x.min[1], x.min[2], x.max[1], x.max[2], lty = 1, lwd = 2, col = 'forestgreen')


#### Large Scale Hypothesis Testing ----

allergy <- read.table('hatingalmonds.txt')
head(allergy)
# M1 M2 M3 M4 M5 M6 M7
#  0  0  0  0  1  1  0 -> mutation "i" YES or NO
dim(allergy)
# 100 520

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