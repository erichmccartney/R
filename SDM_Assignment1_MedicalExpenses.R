#' SDM Assignment 1: Medical Expenses

library(readxl)
setwd("~/GitHub/R/DataSets")
df <- read_excel("HealthInsurance.xlsx", sheet='Data')
dim(df)
str(df)
View(df)

#' Data visualizations
hist(df$medexpense)
hist(log(df$medexpense))

which(! complete.cases(df)) #no missing values
colSums(is.na(df)) #no missing values

df1 <- df[, -c(11, 14:15, 28:29)]

temp <- df1[, c(1:10)]
library(PerformanceAnalytics)
chart.Correlation(temp)


#' High correlations (>0.8) between listprice, sqft, and lotsqft; between pricesold and listprice (1.00); 
#' and between baths, pricesold, listprice, and sqft. Since sqft is a core predictor of pricesold, 
#' we must drop listprice and lotsqft from our analysis to avoid multicollinearity. We keep baths 
#' though it has a 0.85 correlation with sqft because it is an important variable.

#' Regression models

library(stargazer)

m1 <- lm(medexpense ~ healthins + female + blackhisp + age*income + illnesses + ssiratio + private + 
           married + msa, data = df1)

m2 <- lm(medexpense ~ healthins + age + female + blackhisp + logincome + illnesses +  ssiratio +private + 
           married + msa + lowincome, data=df1)

m3 <- lm(medexpense ~ age + female + blackhisp + logincome + illnesses +  ssiratio +private + 
           married + msa + poverty, data=df1)

stargazer(m1, m2, m3, type='text', single.row=TRUE)

m4 <- lm(adom_agentdaysonmarket ~ yrblt + privatepool + communitypool + specialsale + lppersqft + year, data=df)

m5 <- lm(adom_agentdaysonmarket ~ yrblt*lppersqft + privatepool + communitypool + specialsale + year, data=df)

m6 <- lm(adom_agentdaysonmarket ~ year, data=df1)

stargazer(m6, m4, m5, type='text', single.row=TRUE)

#' Test for assumptions

plot(m2)
plot(m5)

shapiro.test(m2$res)                        # Shapiro-Wilk's test of multivariate normality
shapiro.test(m5$res)

bartlett.test(list(m2$res, m2$fit))         # Bartlett's test of homoskedasticity
bartlett.test(list(m5$res, m5$fit))

library("car")                              # Multicollinearity test
vif(m1)
vif(m4)

library(lmtest)
dwtest(m2)                                  # Durbin-Watson test of autocorrelation
dwtest(m5)

#' Was this a good analysis? How do we know?
#' Can we make this analysis better? How?

#'------------------------------------------------------------------

#' Seemingly unrelated regression (SUR) models
#' A system of linear equations that are correlated across the equations
#' but uncorrelated across observations. SUR corrects for correlated errors
#' across equations
#' 
df <- read_excel("HuntersGreenHomeSales.xlsx", sheet='Data')

eq1 <- pricesold - sqft*Beds + sqft*Beds + garages + tileroof + age +
    privatepool + communitypool + specialsale + year

eq2 <- adom_agentdaysonmarket - age*lppersqft + privatepool + communitypool + 
  specialsale + year

library(systemfit)
system <-list(eq1=eq1, eq2=eq2)
sur <- systemfit(system, method="SUR", date=df)
summary (sur)

summary(m2)
summary(m5)

#------------------------------------------------

hist(df1$adom_agentdaysonmarket)
hist(log(df1$adom_agentdaysonmarket))

m4 <- lm(log(adom_agentdaysonmarket) ~ age + privatepool + communitypool +
          specialsale + lppersqft + year, date=df1)

m5 <- lm(log(adom_agentdaysonmarket+1) ~ age + privatepool + communitypool +
           specialsale + lppersqft + year, date=df1)

stargazer(m4, m5, type-"text", single.row = TRUE)

m5$coefficients
exp(m5$coefficients)
