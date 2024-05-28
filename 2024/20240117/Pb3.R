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
head(data)

## Visualize some of the trends
# data.subset <- subset(data, as.numeric(id_storage_centre) %in% seq(1, 40, 2)) # one each 2 centres

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


# a) -------------------------

m0 <- lm(costs ~ time + costs0 + growth:time + rad_less_15_city + size, data = data)
summary(m0)

rbind("Sigma", sqrt(sum((m0$residuals)^2)/m0$df))
m0$coefficients
rbind("AIC", AIC(m0))
