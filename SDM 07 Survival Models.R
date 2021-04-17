#' Survival Analysis000
#' Data Unemployment.csv (3343 obs x 20 variables)
#' Objective: To model the time to finding a job
#' Y: Time, Event (Time to Event)
#' X: LogWage, Age, UnempIns

setwd("~/GitHub/R/DataSets")
d <- read.csv("Unemployment.csv")
str(d)
View(d)
attach(d)

# Descriptive statistics
length(unique(Time))                 # Count of unique values in the Time Column
unique(Time)                         # 20 unique times in the sample: 1-28 
summary(Time)
table(Event)
hist(Time)
hist(LogWage)
hist(Age)
table(UnempIns)
table(UnempIns, Event)

# Kaplan-Meier non-parametric analysis
# install.packages("survival")
library(survival)
y <- Surv(Time, Event)              # Y is a combination of time and event

km1 <- survfit(y ~ 1)               # survfit is kaplan-meier model 
summary(km1)
plot(km1, xlab="Time", ylab="Survival Probability")

# Kaplan-Meier non-parametric analysis by group
km2 <- survfit(y ~ UnempIns)
summary(km2)
plot(km2, xlab="Time", ylab="Survival Probability")

# Nelson-Aalen non-parametric analysis
na <- survfit(y ~ 1, type="aalen") # type producing an error, need to investigate
summary(na)
plot(na, xlab="Time", ylab="Survival Probability")

# Cox proportional hazard model - coefficients and hazard rates
cox <- coxph(y ~ LogWage + Age + UnempIns)
summary(cox)

# Exponential, Weibull, and log-logistic parametric model coefficients
exp <- survreg(y ~ LogWage + Age + UnempIns, dist="exponential")
summary(exp)

weibull <- survreg(y ~ LogWage + Age + UnempIns, dist="weibull")
summary(weibull)

loglogistic <- survreg(y ~ LogWage + Age + UnempIns, dist="loglogistic")
summary(loglogistic)

library(stargazer)
stargazer(cox, exp, weibull, loglogistic, type="text")

