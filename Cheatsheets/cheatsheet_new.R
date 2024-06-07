###--------------------###
### DATA PREPROCESSING ###------------------------------------------------------
###--------------------###

data <- read.table('data_regression.txt', header = T)
head(data)
dim(data)

n <- dim(data)[1]
p <- dim(data)[2]

colnames(data)
length(unique(data$feature))
typeof(data$feature)
is(data$feature)

table(data$feature)

data$feature <- ifelse(data$feature == "Yes", "Sì", "No")
data$feature <- factor(data$feature, labels = c('F','T'))
# (If you want to specify the "labels" argument, pay attention to write the labels
# in the alphabetical order of the original ones in order to get the correct mapping)

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

#### Multivariate Data ----------------------------------------------------------

graphics.off()

options(rgl.printRglwidget = TRUE)

plot(data)
plot(data[, "feature1_name"], data[, "feature2_name"])

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
# If "asp" (aspect ratio) is specified (e.g. asp = 1), then "xlim" is ignored

hist(data[, "feature1_name"], 
     main = "Histogram of 'feature1'",
     prob = T)
hist(data[, "feature2_name"])

par(mfrow=c(2,2))
# Equivalent to: layout(cbind(c(1, 3), c(2, 4)), widths = c(1, 1), heights = c(1, 1))


#### Categorical Data --------------------------------------------------------

pie(table(data.label), col = color.lab)

barplot(table(data.label) / length(data.label))


#### Save Plots --------------------------------------------------------------

pdf(file = "myplots.pdf", onefile = T)
plot(data[, 3], col = 'red', pch = 8, cex = 1.5)
plot(data[, 3], col = 'green', pch = 19, asp = 10)
plot(data[, 3], col = 'blue', pch = 11, cex = 3)
dev.off()