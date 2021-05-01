#' LOGISTIC REGRESSION: CORONARY HEART DISEASE
#' Data: framingham.csv
#' Predicting 10-year coronary heart disease occurence in a sample of 4240 patients
#' https://www.kaggle.com/neisha/heart-disease-prediction-using-logistic-regression/data

setwd("~/GitHub/R/DataSets")
df <- read.csv("FraminghamCHD.csv")
View(df)
dim(df)
str(df)

#' Data preprocessing

# df$TenYearCHD <- factor(df$TenYearCHD, labels=c("no", "yes"))
df$gender <- factor(df$male, labels=c("female", "male"))
df$currentSmoker <- factor(df$currentSmoker, labels=c("no", "yes"))
df$BPMeds <- factor(df$BPMeds, labels=c("no", "yes"))
df$prevalentStroke <- factor(df$prevalentStroke, labels=c("no", "yes"))
df$prevalentHyp <- factor(df$prevalentHyp, labels=c("no", "yes"))
df$diabetes <- factor(df$diabetes, labels=c("no", "yes"))
df$education <- NULL                                      # Drop education column

colSums(is.na(df))
df <- df[complete.cases(df), ]                            # Drop incomplete rows
str(df)

#' Exploratory analysis

table(df$TenYearCHD)                                      # Unbalanced sample
table(df$TenYearCHD, df$gender) 

plot(TenYearCHD ~ age, data=df)
plot(TenYearCHD ~ gender, data=df)
plot(TenYearCHD ~ totChol, data=df)

#' Linear probability model
#' OLS is wrong, given the binary nature of DV resp, but let's check the residuals

linear <- lm(TenYearCHD ~ age + gender + cigsPerDay + BPMeds + prevalentStroke + prevalentHyp + 
          totChol + sysBP + diaBP + BMI + heartRate + glucose, data=df)
summary(linear)                              
plot(linear)                                              # Fails normality and homoskedasticity

#' Logit and Probit models

logit  <- glm(TenYearCHD ~ age + gender + cigsPerDay + BPMeds + prevalentStroke + prevalentHyp + 
          totChol + sysBP + diaBP + BMI + heartRate + glucose, family=binomial (link="logit"), data=df)
summary(logit)

probit <- glm(TenYearCHD ~ age + gender + cigsPerDay + BPMeds + prevalentStroke + prevalentHyp + 
          totChol + sysBP + diaBP + BMI + heartRate + glucose, family=binomial (link="probit"), data=df)
summary(probit)

library(stargazer)
stargazer(df, title="Descriptive Statistics", type="text")
stargazer(linear, logit, probit, title="10 Year CHD Classification", type="text")
AIC(linear)

logit$coef                                                # Log odds ratio
probit$coef
confint(logit)                                            # 95% confidence interval (using log-likelihood)
confint.default(logit)                                    # 95% confidence interval (using standard errors)
exp(logit$coef)                                           # Odds ratio
exp(cbind(OddsRatio = coef(logit), confint(logit)))       # Odds Ratio and 95% C.I.

#' Predicted probabilities

predols <- predict(linear, type="response")
summary(predols)

predlogit <- predict(logit, type="response")              # Predicted values of y (in the scale of response variable, i.e. log-odds)
summary(predlogit)

predprobit <-predict(probit, type="response")
summary(predprobit)

#' Marginal effects estimation

head(predict(logit, type="link"))                         # Predicted values of y (in the scale of linear predictors)
dlogis(predict(logit, type="link"))                       # Density function for the logistic distribution of y
LogitScalar <- mean(dlogis(predict(logit, type="link")))  # Mean of density function of y
LogitScalar*coef(logit)                                   # Marginal effects

ProbitScalar <- mean(dnorm(predict(probit, type="link")))
ProbitScalar*coef(probit)

# Accuracy estimation: Confusion matrix

table(df$TenYearCHD, round(fitted(logit)))
table(df$TenYearCHD, round(fitted(probit)))

# McFadden's Pseudo R-squared

library(DescTools)
PseudoR2(logit)
PseudoR2(logit, c("McFadden", "Nagel"))

# Comparing models: Likelihood-ratio test

library(lmtest) 
lrtest(logit, linear)                                    # Can't compare OLS vs GLM models
null  <- glm(TenYearCHD ~ 1, family=binomial (link="logit"), data=df)
lrtest(null, logit)
lrtest(logit, probit)

# install.packages("rcompanion")
library(rcompanion)
nagelkerke(logit)

#' Accuracy estimation using training and test datasets

set.seed(1234)
trainIndex = sample(1:nrow(df), size=round(0.75*nrow(df)), replace=FALSE)
train <- df[trainIndex,]
test  <- df[-trainIndex,]
dim(train); dim(test)

logit  <- glm(TenYearCHD ~ age + male + cigsPerDay + BPMeds + prevalentStroke + prevalentHyp + 
          totChol + sysBP + diaBP + BMI + heartRate + glucose, family=binomial (link="logit"), data=train)
test_x <- test[ , c(1:14)]
predlogit <-predict(logit, newdata=test_x, type="response")
predlogit <- ifelse(predlogit>0.5, 1, 0)

table(test$TenYearCHD, predlogit)                         # Confusion matrix
ClassificationError <- mean(predlogit != test$TenYearCHD) # Classification error
print(paste("Accuracy = ", 1-ClassificationError))        # Accuraty rate

# install.packages("ROCR")
library(ROCR)
pr <- prediction(predlogit, test$TenYearCHD)
prf <- performance(pr, measure = "tpr", x.measure = "fpr")
plot(prf)                                                 # ROC plot: TP vs FP

auc <- performance(pr, measure = "auc")
auc <- auc@y.values[[1]]
auc


#' Multinomial Logit Model
#' Data: Jobs.csv
#' DV:  job (expected job: factor with 3 levels: sales, office, management)
#' IVs: workexp (work experience in years, integer)
#'      gender (factor with 2 levels: male, female)
#'      minority (factor with 2 levels: yes, no)

d <- read.csv("Jobs.csv")
str(d)
View(d)

table(d$job)
xtabs(~ workexp + job, data=d)

d$job <- relevel(d$job, ref="sales")
d$minority <- relevel(d$minority, ref="no")
d$gender <- relevel(d$gender, ref="female")

exp <- factor(d$workexp)
levels(exp)[3:10] <- rep(c("6-7", "8-10", "11-13"), c(2, 3, 3))
table <- xtabs(~ exp + job, data=d)
prop.table(table, 1)
plot(job ~ exp, data=d, off=0)

library("nnet")
ml1 <- multinom(job ~ workexp +  gender + minority, data=d, trace=FALSE)
summary(ml1)                                                 # All data
confint(ml1)                                                 # 95% confidence interval
coef(ml1)
exp(coef(ml1))                                               # Odds ratios

z <- summary(ml1)$coefficients/summary(ml1)$standard.errors
p <-(1 - pnorm(abs(z), 0, 1)) * 2                            # Two-tailed z-test
p

predprob <- fitted(ml1)                                      # Predicted probabilities
head(predprob)


ml2 <- multinom(job ~ workexp +  minority, data=d, subset = gender=="male", trace=FALSE)
summary(ml2)                                                 # Subset analysis for males
ml3 <- multinom(job ~ workexp +  minority, data=d, subset = gender=="female", trace=FALSE)
summary(ml3)


#' Ordinal Logit Model
#' Data: Coupons.txt
#' DV:  rpurchase (likelihood of repeat purchase by customers; ordinal scale: low/medium/high probability)
#' IVs: coupons (whether customer has coupons: 0/1)
#'      peers (recommended by peers or not: 0/1)
#'      quality (product quality: 1 to 5). 

df <- read.table("Coupons.txt")
View(df)
str(df)

df$purchase <- factor(df$rpurchase, levels=c("low probability", "medium probability", "high probability"), ordered=TRUE) 
df$peers <- factor(df$peers, levels=c("0", "1"), ordered=TRUE) 
df$coupon <- factor(df$coupon, levels=c("0", "1"), ordered=TRUE)

table(df$purchase)                                # Frequency tables
table(df$purchase, df$coupon) 
table(df$purchase, df$peers) 

samplesize = floor(0.75*nrow(df))                 # Create train and test data sets
set.seed(1234)
index <- sample(seq_len(nrow(df)), size=samplesize)
train <- df[index,]
test  <- df[-index,]

library(MASS)                                     # Ordered logit model
ol <- polr(purchase ~ coupon + peers + quality , data=train, Hess=TRUE)
summary(ol)
coeftest(ol)                                      # t-tests of coefficients

predicted <- predict(ol, test)
table(test$purchase, predicted)                   # Confusion matrix
mean(as.character(test$purchase) != as.character(predicted))  # Missclassification error

# install.packages("effects")
library("effects")                                 # Graphical plots of effects
Effect(focal.predictors="quality", ol)
plot(Effect(focal.predictors="coupon", ol))
plot(Effect(focal.predictors=c("quality", "coupon"), ol))

