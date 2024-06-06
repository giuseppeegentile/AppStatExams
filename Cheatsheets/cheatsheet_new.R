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
data$feature <- factor(data$feature, labels=c('F','T'))
# (If you want to specify the "labels" argument, pay attention to write the labels
# in the alphabetical order of the original ones in order to get the correct mapping)

data.label <- data[, "feature_name"]
data <- data[, !colnames(data) %in% "feature_name"]

features <- c("feature1_name", "feature2_name")
data.label <- data[, colnames(data) %in% features]
data <- data[, !colnames(data) %in% features]

data.label <- data[, 9]
data <- data[, -9]
data <- data[, 1:8]

data <- data[, -c(6, 9)]
data <- data[, c(1:5, 7:8, 10)]

