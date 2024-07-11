library(MASS)
library(nlmeU)
library(corrplot)
library(lattice)
library(plot.matrix)
library(lme4)
library(insight)
library(car)
library(rgl)
library(glmnet)
library(ISLR)
library(leaps)
library(nlme)  


data <- read.table("data.txt", header=TRUE)
head(data)
attach(data)
#FROM LM TO LMM-----------------------------------------------------------------------------------------
## LM with HETEROSCEDASTIC INDEPENDENT ERRORS-----------------------------------------------------------
###VarIdent ---------------------------------------------------
lm <- gls(target ~ + ,  # the same as before
             weights = varIdent(form = ~1|regressor), # Var. function; <delta, stratum>-group
             data = data)
summary(lm)

plot(lm$residuals)
# i see the value of the variance per time step
lm$modelStruct$varStruct
intervals(lm, which = "var-cov")  ## 95% CI

###VarPower --------------------------------------------------------------------------------------

lm <- update(lm, weights = varPower(form = ~regressor)) # Var. function; <delta, v_it>-group
summary(lm)

lm$modelStruct$varStruct
intervals(lm, which = "var-cov")



# LINEAR MODELS with HETEROSCEDADSTIC DEPENDENT ERRORS---------------------------------------------
### Correlation 1: CorCompSym()-----------------------------------------------------------------
lm1.form <- formula(target ~ + )
lm1.hd <- gls(lm1.form, weights = varPower(form = ~regressor),
              correlation = corCompSymm(form = ~1|grouping),
              data = data)
summary(lm1.hd)

intervals(lm1.hd, which = "var-cov")
###Correlation 2: AR(1)
lm1.hdar <- update(fm9.2, 
                 correlation = corAR1(form = ~tp|grouping),
                 data = armd)
summary(lm1.hdar)
intervals(lm1.hdar, which = "var-cov")
#LMM---------------------------------------------------------------------------------
##LMM RANDOM INTERCEPT(homoscedastic residuals)-----------------------------------------------------------------------------
# substitute intercept and slope with the variables you have
lmm.ri <- lmer( ~  + (1|grouping), data = data)
summary(lmm.ri)

confint(lmm.ri,oldNames=TRUE)

# fixed effects (coefficients as usual)
fixef(lmm.ri)
# random effects (for each of the level of the grouping) in this case only on the intercept
ranef(lmm.ri)
# fixed effects + random effects, you should see only the intercept vary
head(coef(lmm.ri)$school_id)


## Var-Cov matrix of fixed-effects
vcovb <- vcov(lmm.ri) 
vcovb
corb <- cov2cor(vcovb) 
nms <- abbreviate(names(fixef(lmm.ri)), 5)
rownames(corb) <- nms
corb

## Var-Cov matrix of random-effects and errors
sigma2_eps <- as.numeric(get_variance_residual(lmm.ri))
sigma2_eps
sigma2_b <- as.numeric(get_variance_random(lmm.ri))
sigma2_b

# PVRE
PVRE <- sigma2_b/(sigma2_b+sigma2_eps)
PVRE 

dotplot(ranef(lmm.ri, condVar=T))
###Prediction ----------------------------------------------------------------------
test.data <- data.frame()
# Prediction from mixed model on the training set:
# 1) Without random effects ->  re.form=NA
predict_no_re <- predict(lmm.ri, re.form=NA)
head(predict_no_re) # (almost) same predictions
# 2) With random effects
predict_re <- predict(lmm.ri)
head(predict_re)

# if data from group not trained on -> use alow.new.levels
predict_re <- predict(lmm.ri, newdata=test.data, allow.new.levels = T)
predict_re # the same as before, it uses the average of the random intercept, i.e. 0

###Diagnostic----------------------------------------------------------------------
plot(lmm.ri) # pearson residuals -> check homoscedasticity

qqnorm(resid(lmm.ri))
qqline(resid(lmm.ri), col='red', lwd=2)

# 2) Assessing Assumption on the Random Effects
# substitute subject with the grouping in your exercise
qqnorm(unlist(ranef(lmm.ri)$subject), main='Normal Q-Q Plot - Random Effects on Intercept')
qqline(unlist(ranef(lmm.ri)$subject), col='red', lwd=2)


##LMM RANDOM INTERCEPT + SLOPE(homoscedastic residuals)------------------------------------------------------------
# substitute intercept and slope with the variables you have
lmm.ris <- lmer( ~ + (1+slope|grouping),
                  data = data, control=lmerControl(optimizer="bobyqa",
                                                   optCtrl=list(maxfun=2e5)))
summary(lmm.ris)
confint(lmm.ris,oldNames=TRUE)

# fixed effects (coefficients as usual)
fixef(lmm.ri)
# random effects (for each of the level of the grouping) in the intercept and in the slope
ranef(lmm.ri)
# fixed effects + random effects, you should see a change in the intercept and on the regressor
# on which you are imposing the random effect slope
head(coef(lmm.ri)$school_id)


## Var-Cov matrix of fixed effects
vcovb <- vcov(lmm.ris) 
vcovb
corb <- cov2cor(vcovb) 
nms <- abbreviate(names(fixef(lmm.ris)), 5)
rownames(corb) <- nms
corb

## Var-Cov matrix of random-effects and errors
print(vc <- VarCorr(lmm.ris), comp = c("Variance", "Std.Dev."))

sigma2_eps <- as.numeric(get_variance_residual(lmm.ris))
sigma2_eps
sigma2_b <- as.numeric(get_variance_random(lmm.ris))
sigma2_b

PVRE <- sigma2_b/(sigma2_b+sigma2_eps)
PVRE 

dotplot(ranef(lmm.ris, condVar=T)) # xlim=c(-1,1) in dotplot if you want to zoom

###Prediction -----------------------------------------------------------------------
test.data <- data.frame()
# Prediction from mixed model on the training set:
# 1) Without random effects ->  re.form=NA
predict_no_re <- predict(lmm.ris, re.form=NA)
head(predict_no_re) # (almost) same predictions
# 2) With random effects
predict_re <- predict(lmm.ris)
head(predict_re)

# if data from group not trained on -> use allow.new.levels
predict_re <- predict(lmm.ris, newdata=test.data, allow.new.levels = T)
predict_re # the same as before, it uses the average of the random intercept, i.e. 0

###Diagnostics -----------------------------------------------------------------------
plot(lmm.ris)

qqnorm(resid(lmm.ris))
qqline(resid(lmm.ris), col='red', lwd=2)

# Assessing assumption on the random effects 
# substitute subject with the grouping you have
qqnorm(unlist(ranef(lmm.ris)$subject[,1]), main='Normal Q-Q Plot - Random Effects on Intercept')
qqline(unlist(ranef(lmm.ris)$subject[,1]), col='red', lwd=2)

qqnorm(unlist(ranef(lmm.ris)$subject[,2]), main='Normal Q-Q Plot - Random Effects on Slope')
qqline(unlist(ranef(lmm.ris)$subject[,2]), col='red', lwd=2)

# COMPARISON WITH ANOVA FUNCTION ---------------------------------------------------
## The anova() function will take the model objects as arguments, and return an ANOVA testing 
## whether the more complex model is significantly better at capturing the data than the simpler model.
## df indicates the degrees of freedom used -> the less the better
anova(lmm.ri, lmm.ris)
# The p-value for the test is essentially zero meaning the parameters omitted in the reduced model
# are indeed significant

# if p vlaue is not significant -> use AIC,BIC (the less the better) or df parsimony

# LINEAR MIXED MODEL SEPARATED INTERCEPT AND SLOPE-------------------------------------
# we saw earlier (in the VarCorr(lmm.ris) function) that the correlation between the intercept and time was very low, in this way
# below we model them as two independent contributions->one parameter less to estimate
lmm.dris <- lmer( ~  +  + (1|grouping) + (0 + slope|grouping),
                   data = data, control=lmerControl(optimizer="bobyqa",
                                                    optCtrl=list(maxfun=2e5)))

summary(lmm.dris)
confint(lmm.dris,oldNames=TRUE)

# Var-Cov of fixed effects
vcovb <- vcov(lmm.dris) 
vcovb
corb <- cov2cor(vcovb) 
nms <- abbreviate(names(fixef(lmm.dris)), 5)
rownames(corb) <- nms
corb

## Var-Cov matrix of random-effects and errors
print(vc <- VarCorr(lmm.dris), comp = c("Variance", "Std.Dev."))

# PVRE
sigma2_eps <- as.numeric(get_variance_residual(lmm.dris))
sigma2_eps
# in data$time substitute time with the separated slope you are using
sigma2_b <- as.numeric(get_variance_random(lmm.dris)) + mean(data$time^2)*as.numeric(get_variance_slope(lmm.dris))  
sigma2_b

PVRE <- sigma2_b/(sigma2_b+sigma2_eps)
PVRE 

dotplot(ranef(lmm.dris, condVar=T))

#LINEAR MIXED MODELS WITH HETEROSCEDASTIC RESIDUALS ----------------------------------
## fixed-effects formula
lm.form <- formula( ~  +  ) 

# LMM with homoscedastic residuals
lmm <- lme(lm2.form, random = ~1|subject, data = data)

# update including heteroscedastic residuals
lmm.h <- update(lmm,
                 weights = varPower(form = ~ regressor), 
                 data = data)
summary(lmm.h)

VarCorr(lmm.h)  

# Normal Q-Q plots of Pearson residuals 
qqnorm(lmm.h, ~resid(.) | time.f) 

## ANALYSIS OF RANDOM EFFECTS
# Normal Q-Q plots of predicted random effects
qqnorm(lmm.h, ~ranef(.))  
