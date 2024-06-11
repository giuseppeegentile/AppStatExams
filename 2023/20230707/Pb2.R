library(MVN)

data <- read.table("consumption.txt", header=TRUE)
plot(data)

boxplot(data)
# yeah from the boxplot it's clear that the consumption of energy is higher in the evening than in the
# mornign per each measured year

# i have to use a contrast matrix to confront am and pm
mvn(data)$multivariateNormality


C <- matrix (0, nrow=5, ncol=6)
for(i in 1:5)
{
  C[i,c(i,i+1)]<-c(-1,1)
}
C # thanks beps

mean <- colMeans(data)
mean

delta0 <-c(0,0,0,0,0,0)
alpha <- 0.5
n <- dim(data)[1]
q <- 6

fisher <- (((n-1)*(q-1))/(n-q+1)) *qf(1-alpha,q-1,n-q+1)

Cx <- C %*% mean
Cmu <- C %*% delta0
CS <- C %*% cov(data) %*% t(C)
CSinv <- solve(CS)

T2 <- n*t(Cx - Cmu)%*%CSinv%*%(Cx-Cmu)

T2 < fisher
# FALSE meaning evidence of repeating pattern in time
pvalue <- 1 - pf(T2*(n - (q - 1)) / ((q - 1) * (n - 1)), q-1,n-q+1)
pvalue

avg_perday <- ((mean[1]+mean[2])/2 + (mean[3]+mean[4])/2 + (mean[5]+mean[6])/2)/3
avg_perday # should i do an univariate confidence interval?



