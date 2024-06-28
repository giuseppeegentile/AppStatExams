###----------------------###
### Problem 3 (20230207) ###
###----------------------###

library(car)

rm(list = ls())
graphics.off()

data <- read.table('beachVolley.txt', header = T)
head(data)


# a) ----------------------------------------------------------------------

m0 <- lm(price ~ rent.length + center.distance + parking.distance + available.courts + shower + environment + sand.color, data = data)
summary(m0)

m0$coefficients
sum((m0$residuals)^2)/m0$df


# b) ----------------------------------------------------------------------

par(mfrow=c(2,2))
plot(m0)
par(mfrow=c(1,1))

shapiro.test(m0$residuals)
# Assumptions verified


# c) ----------------------------------------------------------------------

linearHypothesis(m0,
                 rbind(c(0,0,1,0,0,0,0,0),
                       c(0,0,0,1,0,0,0,0)),
                 c(0,0))["Pr(>F)"]
# 0.0005378


# d) ----------------------------------------------------------------------

m1 <- lm(price ~ rent.length + center.distance + parking.distance + available.courts + shower + environment, data = data)
summary(m1)

m2 <- lm(price ~ rent.length + center.distance + parking.distance + shower + environment, data = data)
summary(m2)

m3 <- lm(price ~ rent.length + parking.distance + shower + environment, data = data)
summary(m3)

m3$coefficients
sum((m0$residuals)^2)/m3$df


# e) ----------------------------------------------------------------------

new.datum <- data.frame(rent.length = 2, 
                        parking.distance = 100,
                        shower = TRUE,
                        environment = "Indoor")

# Pred. int. for a new obs
Pred <- predict(m3, new.datum, interval = 'prediction', level = 1-0.05) 
Pred
#       fit      lwr      upr
#  69.20856 60.94785 77.46927
