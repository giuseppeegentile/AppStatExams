rm(list=ls())
data <- read.table('orthopaedics.txt', header=TRUE)
load('mcshapiro.test.RData')
head(data)
names(data)



plot(data[,1:2],pch=19)
head(data)
attach(data)

i1 <- which(data$norm_abnorm=='NO')
i2 <- which(data$norm_abnorm=='AB')
datac <-data
datac$color <- ifelse(data$norm_abnorm == 'NO', 'blue', 'red')

n1 <- length(i1)
n2 <- length(i2)
n = n1+n2
plot(datac[,1:2], main='Plot', pch=19, col = datac$color)

legend("topright", legend=levels(as.factor(datac$norm_abnorm)), fill=unique(datac$color))

head(data)
m <-  colMeans(data[,1:2])
m1 <- colMeans(data[i1,1:2])
m2 <- colMeans(data[i2,1:2])
g = 2
p = 2
S1 <- cov(data[i1,1:2])
S2 <- cov(data[i2,1:2])
Sp  <- (n1-1)*S1+(n2-1)*S2/(n-g)

S1
S2
bartlett.test(data[,1:2], data$norm_abnorm)

mvn(data[i1,1:2])
mvn(data[i2,1:2])



pA<-0.35
pB<-0.65
priors<-c(pA,pB)

data <- data[,1:3]
head(data)

df.qda <- qda(data[,1:2], norm_abnorm, prior=priors) 
df.qda      


Qda.on.df <- predict(df.qda, data[,1:2]) #I use my classifier to predict the training set
Qda.on.df


df = data

#Case p=2, g=2
x = 'incidence'
y = 'tilt'
x  <- seq(min(df[,1]), max(df[,1]), length=200)
y  <- seq(min(df[,2]), max(df[,2]), length=200)
xy <- expand.grid(incidence=x, tilt=y) #change accordingly
names(df)

z  <- predict(df.qda, xy)$post  # P_i*f_i(x,y)
# Uno zi per ogni gruppo
z1 <- z[,1] - pmax(z[,2]) #, z[,3])  # P_1*f_1(x,y)-max{P_j*f_j(x,y)} #pmax Returns the (regular or parallel) maxima and minima of the input values.
z2 <- z[,2] - pmax(z[,1]) #, z[,3])  # P_2*f_2(x,y)-max{P_j*f_j(x,y)}  
#z3 <- z[,3] - pmax(z[,1], z[,2])    # P_3*f_3(x,y)-max{P_j*f_j(x,y)}

# Un contour per ogni gruppo
contour(x, y, matrix(z1, 200), levels=0, drawlabels=F, add=T)  
contour(x, y, matrix(z2, 200), levels=0, drawlabels=F, add=T)
#contour(x, y, matrix(z3, 200), levels=0, drawlabels=F, add=T)

legend("topright", legend=levels(as.factor(df$norm_abnorm)), fill=c('red','blue'), lty=c(1,1)) #only in plot
#add green

#Adding centroids - center of true groups
points(df.qda$means, pch=4,col=c('red','blue') , lwd=2, cex=1.5)



qdaCV<- qda(df[,1:2], norm_abnorm, prior = priors, CV=TRUE)   #priors NOT estimated from dataset

miscCV <- table(classe.vera=norm_abnorm, classe.allocata=qdaCV$class)
miscCV
AERqCV <- 0
G <- 2 #n groups
for(g in 1:G)
  AERqCV <- AERqCV + sum(miscCV[g,-g])/sum(miscCV[g,]) * priors[g] 
AERqCV

#nota: nei priors non include eventuale misclass cost






head(data)
s0.new<- data.frame(incidence = 60, tilt= 0) #change if necessary
predict(df.qda, s0.new)     

#### SVM
group = norm_abnorm
data <- data.frame(df[,1:2],group) 
data$group<-factor(data$group)
mycost = 0.1
# data$group<-as.factor(data$group) # Controllare qual'è giusta

# ATTENZIONE: qui il cost è il contrario della teoria: nella teoria si parla di BUDGET (più è alto, più errori ammettiamo) mentre qui si parla di COSTO (più è alto, più penalizziamo gli errori nel train, quindi in teoria tendiamo ad overfittare)

svmfit <- svm(group ~ . , data=data,  kernel = 'linear', #'linear', 'radial', 
              cost = mycost, scale =FALSE )
summary(svmfit)

#Index of the support vectors
svmfit$index 

#Plot - ONLY case p=2!!!! 
par(mfrow=c(1,2))
set.seed(1)
plot(svmfit, data, col =c('salmon', 'light blue'), pch=19, asp=1, main='',xlab='', ylab='')
# support vectors = crosses

ypred<-predict(svmfit, newdata = s0.new, data=data)
ypred

set.seed(1)
svmfit <- svm(group ~ ., data=data, kernel ='linear', gamma = 1, cost = 0.1)
summary(svmfit)

# Plot the SVM
plot(svmfit , data, col =c('salmon', 'light blue'), pch=19, asp=1,asp=1, main='',xlab='', ylab='')

predict(svmfit, newdata = new, data=data)
# NO




