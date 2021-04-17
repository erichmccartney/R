#' Read & preprocess data

library("readxl")
setwd("~/GitHub/R/DataSets")
df <- read_excel("OnlineRetailCampaign.xlsx", sheet="Data")
dim(d)
str(d)
View(d)

#ID Customers and subset data

library(dplyr)
df <- filter(d, visit == "1" | conversion == "1")
dim(df)
str(df)
View(df)

#' Recode factor variable of mens and womens to new variable sex

df$mens = recode_factor(df$mens, "1"="men", "0"="women")
df$mens = relevel(df$mens, "men")
names(df)[names(df) == "mens"] <- "sex"

# Recode factor variable newcustomer
df$newcustomer = recode_factor(df$newcustomer, "1"="New", "0"="Existing")
df$newcustomer = relevel(df$newcustomer, "New")
names(df)[names(df) == "newcustomer"] <- "customer"

#drop unecessary columns
colnames(df)
df1 <- df[, -c(1, 5:6, 10:11)]
col <- c("historysegment", "sex", "customer", "channel", "campaign")
df1[col] <- lapply(df1[col], factor)
str(df1)

#' Question: Do you see any interesting patterns in the data?

#' Data visualization

hist(df$spend)                                     # Not normally distributed
hist(log(d$spend))                                # Not much better
lines(density(log(d$spend)), col="red")           # Density function is a flat line!

colnames(df1)
cor(df1[2:7])                                        # Correlation between numeric variables
library(corrplot)
dtemp <- cor(cbind(df1[, 1], df1[, 2:7]))
corrplot(dtemp, method="circle")                       # No multicollinearity

dtemp <- d[, c(1, 3:7)]
str(dtemp)
library(PerformanceAnalytics)
chart.Correlation(dtemp)

library(ggplot2)
ggplot(df1, aes(x=history, y=log(spend))) +
  geom_point(color= "steelblue") +
  geom_smooth(method="loess", color="red")         # Fit loess (non-linear) plot

table(df1$spend)                                    # 90% of observations are zero!
table(df1$customer)
table(df1$sex)

#' Count Models: Poisson, Quasi-Poisson, and Negative Binomial

poisson <- glm(spend ~ historysegment + history + sex + customer +
                 channel + campaign, family=poisson (link=log), data=df1)
summary(poisson)

#' Poisson models require two additional assumptions: (1) Dispersion, and (2) No excess zeros.
#' Comparing null deviance with df, we see that our model is slightly overdispersed.
#' We can confirm this further using the following dispersion test.

library(AER)
dispersiontest(poisson)

#' Given the overdispersion, it will be appropriate to use QuasiPoisson or Negative Binomial
#' models. However, since the overdispersion is not much, we don't expect to see a big bias 
#' in beta coefficients from the Poission models to the other two models.

qpoisson <- glm(spend ~ historysegment + history + sex + customer +
                  channel + campaign, family=quasipoisson (link=log), data=df1)

library(MASS)
nbinom <- glm.nb(spend ~ historysegment + history + sex + customer +
                   channel + campaign, data=df1)

library(stargazer)
stargazer(poisson, qpoisson, nbinom, type="text", single.row=TRUE)


library(pscl)

df1$spend <- as.integer(df1$spend)

hpoisson <- hurdle(spend ~ historysegment + history + sex + customer +
                     channel + campaign, data=df1, link="logit", dist="poisson")
summary(hpoisson)

hnegbin <- hurdle(spend ~ historysegment + history + sex + customer +
                    channel + campaign, data=df1, link="logit", dist="negbin")
summary(hnegbin)

zip <- zeroinfl(spend ~ historysegment + history + sex + customer +
                  channel + campaign, data=df1, link="logit", dist="poisson")
summary(zip)

zinb <- zeroinfl(spend ~ historysegment + history + sex + customer +
                   channel + campaign, data=df1, link="logit", dist="negbin")
summary(zinb)

stargazer(hpoisson, hnegbin, zip, zinb, type="text", single.row=TRUE)

stargazer(poisson, nbinom, hpoisson, type="text", single.row=TRUE)

#' Note: Stargazer gives you the output of the Poisson/Negative Binomial model, not the 
#' logit model. To see the logit model, you have to use the summary() function.

