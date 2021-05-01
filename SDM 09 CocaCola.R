#' Time Series: Trend, Seasonality, and Lagged Analysis of Multivariate Data
#' Data: Cocacola.csv

setwd("C:/Users/abhatt/Desktop/SDM/Data")
d <- read.csv("CocaCola.csv")
View(d)
str(d)

#' Linear trend model

n <- nrow(d)
d$Time <- seq(1:n)

m1 <- lm(Sales ~ Time, data=d)
summary(m1)

plot(Sales ~ Time, data=d)
abline(m1, col="red")
plot(m1)
plot(m1$res ~ d$t)

#' Additive seasonality model

d$Qtr <- rep(1:4, n/4)
d$Qtr <- factor(d$Qtr)
d$Qtr <- relevel(d$Qtr, 4)
View(d)

m2 <- lm(Sales ~ Time + Qtr, data=d)
summary(m2)

library(stargazer)
stargazer(m1, m2, type="text", single.row=TRUE)

#' Compute RMSE on prediction of a test data sample to detect which model is better

train <- d[1:40, ]
test  <- d[41:56, ]

m1a <- lm(Sales ~ Time, data=train)
m2a <- lm(Sales ~ Time + Qtr, data=train)
pred_test_m1a <- predict(m1, test)
pred_test_m2a <- predict(m2, test)
sqrt(mean((test$Sales - pred_test_m1a) ^ 2))
sqrt(mean((test$Sales - pred_test_m2a) ^ 2))

#' m2 has lower RMSE than m1; hence m2 is the better model

#' Durbin-Watson test for autocorrelation

library(lmtest)
dwtest(m1)
dwtest(m2)

#' Note: DW test shows that m1 has lower autocorrelation than m2. However statistical
#' tests are sometimes misleading. m2 is clearly the better model based on RMSE fit on
#' a separate random sample of test data.

#' ACF and PACF plots

acf(d$Sales)
pacf(d$Sales)

#' Lag model with trend and seasonality

d$SalesLag1 <- c(NA, d$Sales[1:n-1])
d$SalesLag2 <- c(NA, NA, d$Sales[1:54])
d$SalesLag3 <- c(NA, NA, NA, d$Sales[1:53])
View(d)

train <- d[1:40, ]
test  <- d[41:56, ]

m3 <- lm(Sales ~ Time + Qtr + SalesLag1, data=train)
summary(m3) 

m4 <- lm(Sales ~ Time + Qtr + SalesLag1 + SalesLag2 + SalesLag3, data=train)
summary(m4)

stargazer(m1, m2, m3, m4, type="text", single.row=TRUE)

#' Note: Based on adj R2 and residual std error, it appears that m4 has the best fit.
#' However this could be misleading, and we may have an overfitted model. Instead of
#' evaluating a model using the same data that we used to fit the model, we should
#' evaluate the model based on an indepependent (test) data set.

sqrt(mean((test$Sales - predict(m1, test)) ^ 2)) 
sqrt(mean((test$Sales - predict(m2, test)) ^ 2))
sqrt(mean((test$Sales - predict(m3, test)) ^ 2))
sqrt(mean((test$Sales - predict(m4, test)) ^ 2))

#' Note: Based on model prediction on test data, we find that model m2 has the best fit,
#' and models m3 and m4 are overfitted.
