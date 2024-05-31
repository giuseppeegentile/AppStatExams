df<-read.table("rent.txt",header=T)
library(glmnet)
library(MASS)
library(car)
library(rgl)   #3D plots
library(leaps) #best subset selection
library(tree)  #decision trees
library(corrplot) #correlation
library(glmnet)
head(df)
names(df)

n<-dim(df)[1]
p<-dim(df)[2]
n
p
head(df)
dfn = df[,1:8]
head(dfn)
df$two.bathrooms = as.factor(df$two.bathrooms)
head(df)

fit = lm(price ~ footage + age + renovation+transport+center+supermarket+park+ two.bathrooms+
         two.bathrooms:footage + two.bathrooms:age + two.bathrooms:renovation+two.bathrooms:transport+two.bathrooms:center+two.bathrooms:supermarket+two.bathrooms: park, data = df)
summary(fit)



par(mfrow=c(2,2))
plot(fit)
shapiro.test(residuals(fit))
par(mfrow=c(1,1))
vif(fit)


x <- model.matrix(price~footage + age + renovation+transport+center+supermarket+park+ two.bathrooms+
                    two.bathrooms:footage + two.bathrooms:age + two.bathrooms:renovation+two.bathrooms:transport+two.bathrooms:center+two.bathrooms:supermarket+two.bathrooms, data=df)[,-1] #don't forget the-1!!!!
#vector of response
y <- df$price

fit.lasso <- glmnet(x,y, lambda = 45)
fit.lasso$beta 



lambda.grid <- 10^seq(2,0,length=100) #lambda.grid <- 10^seq(5,-3,length=100), o "by"
lambda.grid

fit.lasso.grid <- glmnet(x,y, lambda = lambda.grid) 

plot(fit.lasso.grid,xvar='lambda',label=TRUE, col = rainbow(dim(x)[2]))
legend('topright', dimnames(x)[[2]], col =  rainbow(dim(x)[2]), lty=1, cex=1)

set.seed(1)
#default: 10-fold CV, if you have low n better add nfolds=5 or 3
cv.lasso <- cv.glmnet(x,y,lambda=lambda.grid)  #alpha=0 is ridge CV!

#Best lambda
bestlam.lasso <- cv.lasso$lambda.min
bestlam.lasso

#Optimal lambda
optlam.lasso <- cv.lasso$lambda.1se
optlam.lasso

#Plot
plot(cv.lasso)
abline(v=log(bestlam.lasso), lty=1)
# Black line = bestlam.lasso: optimizes CV-MSE
# Dashed line = biggest lambda s.t. the error falls within 
# 1 sd of the error of the bestlam.lasso


opt.lasso <- glmnet(x, y, lambda = optlam.lasso, alpha = 1) # 1 = Lasso


opt.lasso$beta

Z0.new <- data.frame(inter = 1,footage=30,age=5,renovation=5,transport=300,center=1000,
                     supermarket=500,park=100)
# If there are interactions, write the as dummy.regressor1=0 etc, with the dot!
predict(opt.lasso, newx=as.matrix(Z0.new),s=bestlam.lasso, type = "response")

