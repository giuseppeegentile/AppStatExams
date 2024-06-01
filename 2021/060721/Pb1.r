rm(list=ls())
data <- read.table('chicca.txt', header=TRUE)
load('mcshapiro.test.RData')
head(data)
names(data)

n <- dim(data)[1]
p <- dim(data)[2]