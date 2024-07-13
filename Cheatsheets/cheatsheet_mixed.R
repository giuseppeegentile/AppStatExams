###---------------------###
### LINEAR MIXED MODELS ###------------------------------------------------------
###---------------------###

### Try It Out! ###
rm(list = ls())
graphics.off()
par(mfrow=c(1,1))
cat("\014")
data <- read.table('datasets/data_heteroschedasticity.txt', header = T)
### Try It Out! ###

target <- data$target
time <- data$time
grouping <- data$grouping
class <- data$class


#### Trend Visualization ----

class <- ifelse(class == 1, "closer", "farther")

xy1 <- xyplot(target ~ time | class, #!library(lattice)
              groups = grouping, # f.e. subjects, i from 1 to 40
              data = data,
              type = "l", lty = 1)
update(xy1, xlab = "Time",
       ylab = "Target",
       grid = "h")

## Sample means across time and class

tmcl <- list(time, class)
tMn <- tapply(target, tmcl, FUN = mean)
tMn

## Box-plots for target by class and time
bw1 <- bwplot(target ~ as.factor(time) | class, #!library(lattice)
              data = data)
xlims <- paste('t', unique(time), sep = '')
update(bw1, xlim = xlims, pch = "|")


#### Naive Model ----
# Linear Model with Homoscedastic and Independent Errors

n <- dim(data)[1]

m0 <- lm(target ~ time + reg1 + reg2 + class, data = data)
summary(m0)

par(mar = c(4,4,4,4))
plot(diag(x = sum((m0$residuals)^2)/m0$df, nrow = n/5, ncol = n/5), main = 'Variance-covariance matrix of Y') #!library(plot.matrix)

plot(m0$fitted, m0$residuals, pch = 19)
abline(h = 0, col = 'red')

qqnorm(m0$residuals)
qqline(m0$residuals, col = 'red')

shapiro.test(m0$residuals)

boxplot(rstandard(m0) ~ grouping, col = rainbow(length(unique(grouping))),
        xlab = 'Grouping Factor', ylab = 'Std. Residuals', main = 'Distribution of std. residuals across grouping factor')

boxplot(rstandard(m0) ~ time, col = rainbow(length(unique(time))),
        xlab = 'Time', ylab = 'Std. Residuals', main = 'Distribution of std. residuals across time') 


#### Introduce Heteroschedasticity ----
# Linear Model with Heteroschedastic and Independent Errors [gls() function]

## 1.1 Option 1: VarIdent() # (if time is a categorical variable)

m1.1 <- gls(target ~ time + reg1 + reg2 + class, #!library(nlme)
            weights = varIdent(form = ~1|time), # Var. function; <delta, stratum>-group
            data = data)
summary(m1.1)

plot(m1.1$fitted, m1.1$residuals, pch = 19)
plot(m1.1$fitted, residuals(m1.1, type = "pearson"), pch = 19) # standardized residuals

m1.1$modelStruct$varStruct
m1.1$sigma
intervals(m1.1, which = "var-cov")

m1.1$dims$N - m1.1$dims$p # dofs

par(mar = c(4,4,4,4))
plot(diag(x = c(1, exp(c(m1.1$modelStruct$varStruct)))^2 * m1.1$sigma^2, nrow = n/5, ncol = n/5),
     main = 'Variance-covariance matrix of Y - VarIdent()')

anova(m1.1, m0)


# 1.2 Option 2: VarPower() # (if time is a numerical variable)

m1.2 <- gls(target ~ time + reg1 + reg2 + class, #!library(nlme)
            weights = varPower(form = ~time), # Var. function; <delta, v_it>-group
            data = data)
summary(m1.2)

# or

m1.2 <- update(m1.1, weights = varPower(form = ~time)) # Var. function; <delta, v_it>-group
summary(m1.2)

m1.2$modelStruct$varStruct
m1.2$sigma
intervals(m1.2, which = "var-cov")

par(mar = c(4,4,4,4))
plot(diag(x = unique(data$time)^(2*m1.2$modelStruct$varStruct) * m1.2$sigma^2, nrow = n/5, ncol = n/5),
     main = 'Variance-covariance matrix of Y - VarPower()')

anova(m1.2, m1.1)
AIC(m1.2, m1.1)


##### Residual Analysis -----

plot(m1.2, resid(., type = "response") ~ fitted(.)) # Raw vs. fitted
plot(m1.2, resid(., type = "response") ~ time) # Raw vs. time
bwplot(resid(m1.2) ~ as.factor(time), pch = "|", data = data)

plot(m1.2, resid(., type = "pearson" ) ~ fitted(.)) # Pearson vs. fitted
plot(m1.2, resid(., type = "pearson") ~ time) # Pearson vs. time
bwplot(resid(m1.2, type = "pearson") ~ as.factor(time), pch = "|", data = data)

# This plot illustrate the effect of scaling: the variance of the residuals is virtually constant


#### Introduce Correlation ----
# Linear Model with Heteroschedastic and Dependent Errors [gls() function]

## 2.1 Correlation 1: CorCompSym()
m2.1 <- gls(target ~ time + reg1 + reg2 + class, 
            weights = varPower(form = ~time),
            correlation = corCompSymm(form = ~1|grouping),
            data = data)
summary(m2.1)

# or

m2.1 <- update(m1.2, 
               correlation = corCompSymm(form = ~1|grouping),
               data = data)
summary(m2.1)

intervals(m2.1, which = "var-cov")
# With the estimates of rho, sigma and delta we can estimate the var-cov matrix

# The marginal variance-covariance structure
m2.1vcov <- getVarCov(m2.1, individual = "2") # estimate of R_i, e.g. i=2
nms <- paste('t', unique(time), sep = '')
dnms <- list(nms, nms)
dimnames(m2.1vcov) <- dnms
print(m2.1vcov)

## on the diagonal we have (sigma^2)*TIME_i^(2*delta)
## out of the diagonal (i, j) we have (sigma^2)*TIME_i^(delta)*TIME_j^(delta)*rho

R = matrix(0, nrow = length(unique(time))*7, ncol = length(unique(time))*7)

for(i in 0:6)
{
  R[(i*length(unique(time))+1):(i*length(unique(time))+length(unique(time))),
    (i*length(unique(time))+1):(i*length(unique(time))+length(unique(time)))] <- m2.1vcov
}
plot(R)

print(cov2cor(m2.1vcov), corr = TRUE, stdevs = FALSE) # Estimate of C_i (correlation matrix)

anova(m2.1, m1.2)


## 2.2 Correlation 2: AR(1)

# The semivariogram function can be defined as the complement of the correlation function.

# Variogram per time lag
Vg2 <- Variogram(m1.2, form = ~time | grouping) # N.B.: pay attention to change variable names with the ones that appear in m1.2!
Vg2
plot(Vg2, smooth = FALSE, xlab = "Time Lag", ylim = c(range(Vg2[1]) - 0.1, range(Vg2[1]) + 0.1))

# From these two plots we see that correlation increases/decreases (?) with time lag
# Therefore, a more appropriate structure might be, e.g., an autoregressive process of order 1 AR(1)

m2.2 <- gls(target ~ time + reg1 + reg2 + class, 
            weights = varPower(form = ~time),
            correlation = corAR1(form = ~time|grouping),
            data = data)
summary(m2.2)

# or

m2.2 <- update(m1.2, 
               correlation = corAR1(form = ~time|grouping),
               data = data)
summary(m2.2)

intervals(m2.2, which = "var-cov")

# The marginal variance-covariance structure
m2.2vcov <- getVarCov(m2.2, individual = "2") # Estimate of R_i, e.g. i=2
dimnames(m2.2vcov) <- dnms
m2.2vcov

## on the diagonal we have (sigma^2)*TIME_i^(2*delta)
## out of the diagonal (i, j) we have (sigma^2)*TIME_i^(delta)*TIME_j^(delta)*rho
##                     (i, k)         (sigma^2)*TIME_i^(delta)*TIME_j^(delta)*rho^2...

R = matrix(0, nrow = length(unique(time))*7, ncol = length(unique(time))*7)

for(i in 0:6)
{
  R[(i*length(unique(time))+1):(i*length(unique(time))+length(unique(time))),
    (i*length(unique(time))+1):(i*length(unique(time))+length(unique(time)))] <- m2.2vcov
}
plot(R)

print(cov2cor(m2.2vcov), digits = 2, corr = TRUE, stdevs = FALSE) # Estimate of C_i

anova(m2.2, m2.1) # since they are non-nested, the p-value can't be computed)


## 2.3 Correlation 3: general correlation structure (more computationally expensive)

m2.3 <- gls(target ~ time + reg1 + reg2 + class, 
            weights = varPower(form = ~time),
            correlation = corSymm(form = ~time|grouping),
            data = data)
summary(m2.3)

# or

m2.3 <- update(m2.2, 
               correlation = corSymm(form = ~time|grouping), 
               data = data)
summary(m2.3)

intervals(m2.3, which = "var-cov")

m2.3vcov <- getVarCov(m2.3, individual = "2")
dimnames(m2.3vcov) <- dnms
m2.3vcov

print(cov2cor(m2.3vcov), corr = TRUE, stdevs = FALSE) # (each symmetric entry should be different)

anova(m2.3, m2.2)


##### Residual Analysis -----

# (a) Plots (and boxplots) of raw residuals

panel.bwxplot0 <- function(x,y, subscripts, ...){
  panel.grid(h = -1)
  panel.stripplot(x, y, col = "grey", ...)
  panel.bwplot(x, y, pch = "|", ...)
}
bwplot(resid(m2.1) ~ as.factor(time) | class, 
       panel = panel.bwxplot0,
       ylab = "Residuals", data = data)
# The box-and-whiskers plots clearly show an increasing variance of the residuals with timepoint. 
# This reflects the heteroscedasticity.

# (b) Plots of Pearson residuals vs. fitted values
# Pearson residuals are obtained from the raw residuals by dividing the latter by an
# estimate of the appropriate residual standard deviation, so they should be more homoscedastic

plot(m2.1, resid(., type = "p") ~ fitted(.) | time)
# The scatterplots show a somewhat more balanced pattern.

stdres.plot <- plot(m2.1, resid(., type = "p") ~ jitter(time, factor = 0.5) | rad_less_15_city,
                    id = 0.01, adj = c(-0.3, 0.5), grid = FALSE)
plot(update(stdres.plot,
            xlim = c(range(time)[1] - 1, range(time)[2] + 1), ylim = c(-4.9, 4.9), grid = "h"))


#### Introduce Randomness ----

### Try It Out! ###
rm(list = ls())
graphics.off()
par(mfrow=c(1,1))
cat("\014")
data <- read.table('datasets/data_mixed.txt', header = T)
### Try It Out! ###

##### Random Intercept -----
# Linear Model with Homoschedastic Residuals and Random Intercept [lmer() function]

m1 <- lmer(target ~ reg1 + reg2 + (1|grouping), #!library(lme4) (# reg1 * reg2 = reg1 + reg2 + reg1:reg2)
           data = data)
summary(m1)

confint(m1, oldNames = TRUE)

## Var-Cov matrix of fixed-effects
vcovb <- vcov(m1) 
vcovb
corb <- cov2cor(vcovb) 
nms <- abbreviate(names(fixef(m1)), 5)
rownames(corb) <- nms
corb

## Var-Cov matrix of random-effects and errors
print(vc <- VarCorr(m1), comp = c("Variance", "Std.Dev."))

sigma2_eps <- as.numeric(get_variance_residual(m1)) #!library(insight)
sigma2_eps
sigma2_b <- as.numeric(get_variance_random(m1))
sigma2_b

# ## Let's compute the conditional and marginal var-cov matrix of Y
# sgma <- summary(m1)$sigma # same as sqrt(sigma2_eps)
# 
# A <- getME(m1, "A") # A  --> N x n, A represents the D (not italic)
# I.n <- Diagonal(ncol(A)) # IN  --> n x n
# 
# ## The conditional variance-covariance matrix of Y (diagonal matrix)
# SigmaErr = sgma^2 * (I.n)
# # Conditioned to the random effects b_i, we observe the var-cov of the errors
# # that are independent and homoscedastic
# 
# ## We visualize the first 20 rows/columns of the matrix
# plot(as.matrix(SigmaErr[1:20,1:20]), main = 'Conditional estimated Var-Cov matrix of Y')
# 
# ## The marginal variance-covariance matrix of Y (block-diagonal matrix)
# V <- sgma^2 * (I.n + crossprod(A)) # V = s^2*(I_N+A*A) --> s^2*(I_N) is the error part, s^2*(A*A) is the random effect part
# # -> V is a block-diagional matrix, the marginal var-cov matrix
# 
# # visualization of the first 20 rows/columns
# plot(as.matrix(V[1:20,1:20]), main = 'Marginal estimated Var-Cov matrix of Y')

# Another way to interpret the variance output is to note percentage of the subject variance out 
# of the total, i.e. the Percentage of Variance explained by the Random Effect (PVRE).

PVRE <- sigma2_b/(sigma2_b+sigma2_eps)
PVRE

# Visualization of the random intercepts with their 95% confidence intervals

dotplot(ranef(m1, condVar = T)) #!library(lattice)

# Extract fixed+random intercepts

coef(m1)[[1]][1]
fixef(m1)[1] + ranef(m1)[[1]] # equivalent


###### Prediction ------

# Prediction from mixed model on the training set:
# 1) Without random effects -> re.form = NA
predict_no_re <- predict(m1, re.form = NA)
head(predict_no_re)
# 2) With random effects
predict_re <- predict(m1)
head(predict_re)

# Prediction from mixed model on a test observation from a subject present in the training set:
test.datum <- data.frame(time = 1, reg1 = -1.5, reg2 = 15, class = 'Active', grouping = '83')

# 1) Without random effects -> re.form = NA
predict_no_re <- predict(m1, newdata = test.datum, re.form = NA)
predict_no_re

# 2) With random effects
predict_re <- predict(m1, newdata = test.datum)
predict_re

# Where the difference comes from the random intercept vector and corresponds to the subject 83
re <- ranef(m1)[[1]]
re[row.names(re) == test.datum$grouping, ]

# Prediction from mixed model on a test observation from a subject not present in the training set:
new.datum <- data.frame(time = 1, reg1 = -1.5, reg2 = 15, class = 'Active', grouping = '201')

# 1) Without random effects -> re.form = NA
predict_no_re <- predict(m1, newdata = new.datum, re.form = NA)
predict_no_re # the same as before

# 2) With random effects
predict_re <- predict(m1, newdata = new.datum)
# it does not recognize the subject --> allow.new.levels = T
predict_re <- predict(m1, newdata = new.datum, allow.new.levels = T)
predict_re # the same as before, it uses the average of the random intercept, i.e. 0


###### Diagnostic ------

# 1) Assessing Assumption on the within-group errors
plot(m1)

qqnorm(resid(m1))
qqline(resid(m1), col = 'red', lwd = 2)

# 2) Assessing Assumption on the Random Effects
qqnorm(unlist(ranef(m1)[[1]]), main = 'Normal Q-Q Plot - Random Effects on Intercept')
qqline(unlist(ranef(m1)[[1]]), col = 'red', lwd = 2)


##### Random Intercept + Slope -----
# Linear Model with Homoschedastic Residuals and Random Intercept + Slope [lmer() function]
## Model 2.1: general D

m2.1 <- lmer(target ~ reg1 + reg2 + (1 + reg1|grouping),
             # optional:  control = lmerControl(optimizer = "bobyqa", optCtrl =l ist(maxfun = 2e5)),
             data = data)
summary(m2.1)

confint(m2.1, oldNames = TRUE)

## Var-Cov matrix of fixed-effects
vcovb <- vcov(m2.1) 
vcovb
corb <- cov2cor(vcovb) 
nms <- abbreviate(names(fixef(m2.1)), 5)
rownames(corb) <- nms
corb

## Var-Cov matrix of random-effects and errors
print(vc <- VarCorr(m2.1), comp = c("Variance", "Std.Dev."))

# In this case the variance of random sigma2_R effects represents the mean random 
# effect variance of the model and is given by
# sigma2_b = Var(b0,b1) = sigma2_b0 + 2Cov(b0,b1)*mean(w) + sigma2_b1*mean(w^2)
sigma2_eps <- as.numeric(get_variance_residual(m2.1))
sigma2_eps
sigma2_b <- as.numeric(get_variance_random(m2.1)) # sigma2_b0 + sigma2_b1*mean(data$reg1^2) + 2*Corr(b0,b1)*sigma_b0*sigma_b1*mean(data$reg1)
sigma2_b

PVRE <- sigma2_b/(sigma2_b+sigma2_eps)
PVRE

dotplot(ranef(m2.1, condVar = T))
dotplot(ranef(m2.1, condVar = T), xlim = c(-1, 1))

anova(m2.1, m1)


## Model 2.2: diagonal D

attr(VarCorr(m2.1)$grouping, "correlation")[1, 2]
# The correlation between d_11 and d_22 is very low -> we fit a new model with a diagonal D matrix 

m2.2 <- lmer(target ~ reg1 + reg2 + (1|grouping) + (0 + reg1|grouping),
             # optional:  control = lmerControl(optimizer = "bobyqa", optCtrl = list(maxfun = 2e5)),
             data = data)
summary(m2.2)

confint(m2.2, oldNames = TRUE)

## Var-Cov matrix of fixed-effects
vcovb <- vcov(m2.2) 
vcovb
corb <- cov2cor(vcovb) 
nms <- abbreviate(names(fixef(m2.2)), 5)
rownames(corb) <- nms
corb

## Var-Cov matrix of random-effects and errors
print(vc <- VarCorr(m2.2), comp = c("Variance", "Std.Dev."))
## We observe that the correlation between d_11 and d_22 is very low, 
## we fit a new model with a diagonal D matrix

# In this case the variance of random sigma2_R effects represents the mean random 
# effect variance of the model and is given by
# sigma2_b = Var(b0,b1) = sigma2_b0 + 0 + sigma2_b1*mean(z^2)
sigma2_eps <- as.numeric(get_variance_residual(m2.2))
sigma2_eps
sigma2_b <- as.numeric(get_variance_random(m2.2)) # sigma2_b0 + sigma2_b1*mean(data$time^2)
sigma2_b

PVRE <- sigma2_b/(sigma2_b+sigma2_eps)
PVRE

dotplot(ranef(m2.2, condVar = T))
dotplot(ranef(m2.2, condVar = T), xlim = c(-1, 1))

anova(m2.2, m2.1)


###### Diagnostic ------

# 1) Assessing Assumption on the within-group errors
plot(m2.1)

qqnorm(resid(m2.1))
qqline(resid(m2.1), col = 'red', lwd = 2)

# 2) Assessing Assumption on the Random Effects
qqnorm(unlist(ranef(m2.1)[[1]][, 1]), main = 'Normal Q-Q Plot - Random Effects on Intercept')
qqline(unlist(ranef(m2.1)[[1]][, 1]), col = 'red', lwd = 2)

qqnorm(unlist(ranef(m2.1)[[1]][, 2]), main = 'Normal Q-Q Plot - Random Effects on Slope')
qqline(unlist(ranef(m2.1)[[1]][, 2]), col = 'red', lwd = 2)


#### Extra: Complete Model ----

##### Heteroschedasticity w/ Random Intercept -----
## Model 1. Random intercept, heteroscedastic residuals (varPower of time) [lme() function]

# LMM with homoscedastic residuals
m1 <- lme(target ~ time + reg1 + reg2 + class, #!library(nlme)
          random = ~1|grouping, 
          data = data)

# Update the model including heteroscedastic residuals
m1.h <- update(m1,
               weights = varPower(form = ~time), 
               data = data)
summary(m1.h)

VarCorr(m1.h)

## var-cov matrix of the errors (i.e. of Y, conditional to the random effects), that are independent but heteroscedastic 
m1.hccov <- getVarCov(m1.h, type = "conditional",  individual = "2")
m1.hccov

plot(as.matrix(m1.hccov[[1]]), main = expression(paste('Conditional estimated Var-Cov matrix of ', Y[2])))

## var-cov matrix of Y_i
m1.hcov <- getVarCov(m1.h, type = "marginal", individual = "2")
m1.hcov 

plot(as.matrix(m1.hcov[[1]]), main = expression(paste('Marginal estimated Var-Cov matrix of ', Y[2])))

## correlation matrix of Y_i
cov2cor(m1.hcov[[1]])


###### Residual Analysis ------

## ANALYSIS OF RESIDUALS
# Default residual plot of conditional Pearson residuals
plot(m1.h)

# Plots (and boxplots) of Pearson residuals per time and class
plot(m1.h, resid(., type = "pearson") ~ time | rad_less_15_city,
     id = 0.05)
bwplot(resid(m1.h, type = "p") ~ as.factor(time) | class, 
       panel = panel.bwplot, # User-defined panel (not shown)
       data = data)

# Normal Q-Q plots of Pearson residuals 
qqnorm(m1.h, ~resid(.) | time) 


## ANALYSIS OF RANDOM EFFECTS
# Normal Q-Q plots of predicted random effects
qqnorm(m1.h, ~ranef(.))  


## Computing predictions comparing population average predictions with subject-specific predictions

aug.Pred <- augPred(m1.h,
                    primary = ~time, # Primary covariate
                    level = 0:1, # fixed/marginal (0) and subj.-spec.(1)
                    length.out = 2) # evaluated in two time instants (t1 and t3)

plot(aug.Pred, layout = c(4, 4, 1))


##### Heteroschedasticity w/ Random Intercept + Slope -----
## Model 2.1. random intercept + slope (correlated), heteroscedastic residuals (varPower of time)

m2.1.h <- update(m1.h,
                 random = ~1 + reg1 | grouping,
                 data = data)
summary(m2.1.h)

getVarCov(m2.1.h, individual = "2")  # D_i italic (i=2)

intervals(m2.1.h, which = "var-cov")  # Estimate of theta_D, delta and sigma


## Model 2.2. random intercept + slope independent, heteroscedastic residuals (varPower of time)
m2.2.h <- update(m2.1.h,
                 random = list(grouping = pdDiag(~reg1)), # Diagonal D
                 data = data) 
summary(m2.2.h)

intervals(m2.2.h)

anova(m2.2.h, m2.1.h)  # We test if d_12 = 0 --> d_12 is not statistically different from 0, we can simplify the D structure in diagonal


###### Residual Analysis ------

qqnorm(m2.2.h, ~ranef(.)) # to be interpreted with caution since it might not reflect the real unknown distribution

plot(m2.2.h, resid(., type = "pearson") ~ time | rad_less_15_city,
     id = 0.05)
bwplot(resid(m2.2.h, type = "p") ~ as.factor(time) | rad_less_15_city, 
       panel = panel.bwplot, # User-defined panel (not shown)
       data = data)


## We make predictions comparing population average predictions with subject-specific predictions
aug.Pred <- augPred(m2.2.h,
                    primary = ~time, # Primary covariate
                    level = 0:1, # Marginal(0) and subj.-spec.(1)
                    length.out = 2) # Evaluated in two time instants ((t1 and t3)

plot(aug.Pred, layout = c(4, 4, 1), columns = 2) 