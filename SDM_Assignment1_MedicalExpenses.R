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

table(df$healthins)
table(df$private)
table(df$healthins, df$private)
df$privateclean <- ifelse(df$healthins==0 & df$private==0, NA, df$private)
table(df$privateclean)

df1 <- df[, -c(11, 14:15, 28:29)]
df2 <- df1[sample(1:nrow(df1),5000),]

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
           married + msa, data = df2)

m2 <- lm(medexpense ~ healthins + age + female + blackhisp + logincome + illnesses +  ssiratio +private + 
           married + msa + lowincome, data=df2)

m3 <- lm(medexpense ~ age + female + blackhisp + logincome + illnesses +  ssiratio +private + 
           married + msa + poverty, data=df2)

stargazer(m1, m2, m3, type='text', single.row=TRUE)

#' Test for assumptions

plot(m1)

shapiro.test(m1$res)                        # Shapiro-Wilk's test of multivariate normality

bartlett.test(list(m1$res, m1$fit))         # Bartlett's test of homoskedasticity

library("car")                              # Multicollinearity test
vif(m1)

library(lmtest)
dwtest(m1)                                  # Durbin-Watson test of autocorrelation