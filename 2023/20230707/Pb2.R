###------------------------------------------------------------------###
### Problem 2: Electric consumption of Italian households (20230707) ###
###------------------------------------------------------------------###

rm(list = ls())
graphics.off()

data <- read.table('consumption.txt', header = T)
head(data)


# a) ----------------------------------------------------------------------

matplot(t(data), type = 'l', lty = 1)

n <- dim(data)[1]
q <- dim(data)[2]

# There's a clear pattern indicating that during the first half of the day
# the electricity consumption is generally low wrt the second half.
# This is probably due to the fact that people tend to sleep after midnight
# and go to work in the morning, leaving house;
# during night-time energy consumption is increased because people are at home 
# and massively use electronic devices and illumination


# b) ----------------------------------------------------------------------

M <- sapply(data, mean) 
S <- cov(data) 

# Contrast matrix to support electricity consumption variations during the day
C <- matrix(c(-1, 1, 0, 0, 0, 0,
              0, -1, 1, 0, 0, 0,
              0, 0, -1, 1, 0, 0,
              0, 0, 0, -1, 1, 0,
              0, 0, 0, 0, -1, 1), q-1, q, byrow = T)
C

alpha <- .05
delta.0 <- rep(0, q-1)

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
# 0 -> variations during the day

# Contrast matrix to support periodicity
C.bis <- matrix(c(-1, 0, 1, 0, 0, 0,
                  -1, 0, 0, 0, 1, 0,
                  0, -1, 0, 1, 0, 0,
                  0, -1, 0, 0, 0, 1), q-2, q, byrow = T)
C.bis

alpha <- .05
delta.0.bis <- rep(0, q-2)

Md.bis <- C.bis %*% M 
Sd.bis <- C.bis %*% S %*% t(C.bis) 
Sdinv.bis <- solve(Sd.bis)

T2.bis <- n * t(Md.bis - delta.0) %*% Sdinv.bis %*% (Md.bis - delta.0)
T2.bis

cfr.fisher.bis <- ((q - 2) * (n - 1) / (n - (q - 2))) * qf(1 - alpha, (q - 2), n - (q - 2)) 
cfr.fisher.bis

T2.bis < cfr.fisher.bis

P.bis <- 1 - pf(T2 * (n - (q - 2)) / ((q - 2) * (n - 1)), (q - 2), n - (q - 2))
P.bis
# 0.3820554 -> periodicity


# c) ----------------------------------------------------------------------

daily_consumption <- colSums(t(data))/3

shapiro.test(daily_consumption)

daily_consumption <- as.data.frame(daily_consumption)

n <- dim(daily_consumption)[1]
p <- dim(daily_consumption)[2]

M <- sapply(daily_consumption, mean)
S <- cov(daily_consumption)
S.inv <- solve(S)

alpha <- 0.05

cfr.fisher <- ((n - 1) * p / (n - p)) * qf(1 - alpha, p, n - p)
cfr.fisher

T2.I <- cbind(inf = M - sqrt(cfr.fisher * diag(S)/n), 
              center = M, 
              sup = M + sqrt(cfr.fisher * diag(S)/n))
T2.I
