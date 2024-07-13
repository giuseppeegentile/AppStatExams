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

F1 <- factor(data$feature1, labels = c('F','T'))
F2 <- factor(data$feature2, labels = c('F','T'))
F1F2 <- factor(paste(F1, F2, sep = ''))

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

cat("\014") # clear console


#### Multivariate Data ----

options(rgl.printRglwidget = TRUE) #!library(rgl) is useful for 3D plots

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

ellipse(center = c(100, 80), shape = cbind(c(9, 3), c(3, 3)), radius = 8, lwd = 2) #!library(car)
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

mean <- colMeans(data)

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