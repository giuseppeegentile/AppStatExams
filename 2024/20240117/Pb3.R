###------------------------------------------------------------###
### Problem 3: Maintenance costs of storage centers (20240117) ###
###------------------------------------------------------------###


rm(list = ls())
graphics.off()

library(MASS)
library(car)
library(rgl)

options(rgl.printRglwidget = TRUE)

library(nlmeU) ## --> for the dataset
library(nlme)  ## --> for models implementation

library(corrplot)
library(lattice)
library(plot.matrix)

data <- read.table("StorageCentres.txt", header = T)
# head(data)


# a) -------------------------

m0 <- lm(costs ~ time + costs0 + growth:time + rad_less_15_city + size, data = data)
# summary(m0)

rbind("Sigma", sqrt(sum((m0$residuals)^2)/m0$df))
print("Coefficients"); m0$coefficients
rbind("AIC", AIC(m0))


# b) ----------------------------------------------------------------------

par(mfrow=c(1,1))
plot(fitted(m0), rstandard(m0))
abline(h = 0, col = 'red')

par(mfrow=c(1,2))

boxplot(rstandard(m0) ~ data$id_storage_centre, col = rainbow(length(unique(data$id_storage_centre))),
        xlab='Centres', ylab='Std. Residuals', main ='Distribution of std. residuals across centres')

boxplot(rstandard(m0) ~ data$time, col = rainbow(length(unique(data$time))),
        xlab='Semesters', ylab='Std. Residuals', main ='Distribution of std. residuals across semesters') 
## -> the variance of the observations increases in time


# c) ----------------------------------------------------------------------

m1 <- gls(costs ~ time + costs0 + growth:time + rad_less_15_city + size,  # the same as before
             weights = varPower(form = ~time), # Var. function; <delta, stratum>-group
             data = data)
# summary(m1)

m1$modelStruct$varStruct
rbind("AIC", AIC(m0))


# d) ----------------------------------------------------------------------

m2 <- update(m1, 
             correlation = corAR1(form = ~time|id_storage_centre),
             data = data)
# summary(m2)

intervals(m2, which = "var-cov")$corStruct
anova(m1, m2) # m1 is better


# Extra -------------------------------------------------------------------

## Visualize some of the trends

library(dplyr)

new_rows <- data %>%
  group_by(id_storage_centre) %>%
  summarise(time = 0, costs = first(costs0), rad_less_15_city = first(rad_less_15_city))

visual_data <- bind_rows(data, new_rows)
visual_data <- visual_data %>% arrange(id_storage_centre, time)
row.names(visual_data) <- NULL
visual_data <- select(visual_data, -costs0)

rad_less_15_city_verbose = ifelse(visual_data$rad_less_15_city == 1, "closer", "farther")

xy1 <- xyplot(costs ~ time | rad_less_15_city_verbose,   
              groups = id_storage_centre,
              data = visual_data,
              type = "l", lty = 1)
update(xy1, xlab = "Time (in semesters)",
       ylab = "Cost for maintenance (in k€)",
       grid = "h")

## Closer centres have on average higher costs of maintenance

rad_less_15_city_verbose = ifelse(data$rad_less_15_city == 1, "closer", "farther")

## sample means across time and closure
flst <- list(data$time, rad_less_15_city_verbose)
tMn <- tapply(data$costs, flst, FUN = mean)
tMn

## We confirm what we observe in the plot

## let's color the residuals relative to different time instants
par(mfrow=c(1,1))

colori = rainbow(length(unique(data$time)))
num_ctr = table(data$time)
colori2 = rep(colori, num_ctr)
plot(rstandard(m0), col = colori2)
abline(h=0)

## Variogram per time lag
Vg2 <- Variogram(m1, form = ~time | id_storage_centre)
Vg2
plot(Vg2, smooth = FALSE, xlab = "Time Lag", ylim = c(0.8, 1.1))

# The marginal variance-covariance structure
m2vcov <- getVarCov(m2, individual = "2")  #estimate of R_i, e.g. i=2
nms <- c("1", "2", "3", "4", "5")
dnms <- list(nms, nms) # Dimnames created
dimnames(m2vcov) <- dnms # Dimnames assigned
print(m2vcov)
