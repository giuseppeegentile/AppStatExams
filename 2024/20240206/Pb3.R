###------------------------------------------------------------###
### Problem 3: Asthma prevalence in Italian regions (20240206) ###
###------------------------------------------------------------###


rm(list = ls())
graphics.off()

library(MASS)
library(car)
library(rgl)

options(rgl.printRglwidget = TRUE)

data <- read.table("asthma.txt", header = T)
# head(data)


# a) ----------------------------------------------------------------------

m0 <- lm(asthma ~ urban + age + pollution + sunny + income + education, data = data)
# summary(m0)

rbind("Sigma", sqrt(sum((m0$residuals)^2)/m0$df))
print("Coefficients"); m0$coefficients

par(mfrow=c(2,2))
plot(m0)

# Going towards high fitted values, the residuals tend to decrease

shapiro.test(residuals(m0))

# Normality verified


# Extra -------------------------------------------------------------------

# Exploring residuals vs. regressors
par(mfrow=c(2,3))

for(i in 1:10)
{
  if (i != 1 && i != 2 && i != 7 && i != 10) # 1: province_id; 2: region_id; 7: urban; 10: asthma
  {
    plot(as.numeric(unlist(data[i])), residuals(m0), xlab = colnames(data)[i], pch = 19)
    abline(h=0)
  }
}