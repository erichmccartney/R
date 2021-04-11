#' Poison Regression of Doctor's Visit Data from Australian Health Service
#' Data: DoctorVisits.xlsx (5,190 observations x 12 variables)

#' Read & preprocess data

library("readxl")
setwd("~/GitHub/R/DataSets")
d <- read_excel("DoctorVisits.xlsx", sheet="Data")
str(d)

col <- c("gender", "private", "freepoor", "freeold", "nchronic", "lchronic")
d[col] <- lapply(d[col], factor)
str(d)

#' Question: Do you see any interesting patterns in the data?

#' Data visualization

hist(d$visits)                                     # Not normally distributed
hist(log(d$visits))                                # Not much better
lines(density(log(d$visits)), col="red")           # Density function is a flat line!

cor(d[3:7])                                        # Correlation between numeric variables
library(corrplot)
dtemp <- cor(cbind(d[, 1], d[, 3:7]))
corrplot(dtemp, method="circle")                       # No multicollinearity

dtemp <- d[, c(1, 3:7)]
str(dtemp)
library(PerformanceAnalytics)
chart.Correlation(dtemp)

library(ggplot2)
ggplot(d, aes(x=illness, y=log(visits))) +
  geom_point(color= "steelblue") +
  geom_smooth(method="loess", color="red")         # Fit loess (non-linear) plot

table(d$visits)                                    # 80% of observations are zero!


#' Predictor selection
#' 
#' gender  : Women may require more doctors' office visits than men
#' age     : Older people usually have more ailments and require more visits
#' income  : Typically does not effect visits, because visits are paid by insurance - DROP
#' illness : People with more illnesses will require more doctors' visits
#' reduced : Number of days of reduced activity due to illness or injury will be multicollinear with illnesses - DROP
#' health  : People in good health will require fewer visits
#' private : People with private insurance may visit doctors more than people without insurance
#' freepoor: People with freepoor insurance may visit doctors more than people without insurance
#' freeold : People with freeold insurance may visit doctors more than people without insurance
#' nchronic: People with chronic conditions may require more visits 
#' lchronic: People with chronic conditions limiting activity cannot visit doctors frequently


#' Count Models: Poisson, Quasi-Poisson, and Negative Binomial

poisson <- glm(visits ~ illness + age + gender + health + private + freepoor + freeold
               + nchronic + lchronic, family=poisson (link=log), data=d)
summary(poisson)

#' Poisson models require two additional assumptions: (1) Dispersion, and (2) No excess zeros.
#' Comparing null deviance with df, we see that our model is slightly overdispersed.
#' We can confirm this further using the following dispersion test.

library(AER)
dispersiontest(poisson)

#' Given the overdispersion, it will be appropriate to use QuasiPoisson or Negative Binomial
#' models. However, since the overdispersion is not much, we don't expect to see a big bias 
#' in beta coefficients from the Poission models to the other two models.

qpoisson <- glm(visits ~ illness + age + gender + health + private + freepoor + freeold
                + nchronic + lchronic, family=quasipoisson (link=log), data=d)

library(MASS)
nbinom <- glm.nb(visits ~ illness + age + gender + health + private + freepoor + freeold
                 + nchronic + lchronic, data=d)

library(stargazer)
stargazer(poisson, qpoisson, nbinom, type="text", single.row=TRUE)

#' Question: What patterns do you see in the beta estimates from the above three models?


#' Hurdle Models and Zero-Inflated Models
#' 
#' Some people who are uninsured may not want to visit a doctor to avoid an expensive 
#' out-of-pocket expense. Some insured people who did not have any illnesses may also 
#' have no motivation to visit a doctor. There can be a segment of the sample who fit these 
#' two criteria for whom visits=0, and their lack of visits may not have anything to do with
#' the predictors in our previous Poisson models. These are called excess zeros, whihc are 
#' predicted by a different data generating process (illnesses and insurance) than our 
#' Poisson model above (predictors above). To analyze this data, we can seperate out the 
#' excess zero observations using a logit model and then run a Poisson/QuasiPoisson/Negative 
#' Binomial model for the rest of the data. 
#' 
#' Since we have three types of insurance in the data (private, freepoor, and freeold), we 
#' have to create a new variable called insurance for people who have either of these three
#' types of insurance

d$insurance <- ifelse(d$private=="yes" | d$freepoor=="yes" | d$freeold=="yes", 1, 0)
View(d)
sum(d$insurance)

library(pscl)

hpoisson <- hurdle(visits ~ illness + age + gender + health + private + freepoor + freeold
                   + nchronic + lchronic | insurance + illness, data=d, link="logit", dist="poisson")
summary(hpoisson)

hnegbin <- hurdle(visits ~ illness + age + gender + health + private + freepoor + freeold
                  + nchronic + lchronic | insurance + illness, data=d, link="logit", dist="negbin")
summary(hnegbin)

zip <- zeroinfl(visits ~ illness + age + gender + health + private + freepoor + freeold
                + nchronic + lchronic | insurance + illness, data=d, link="logit", dist="poisson")
summary(zip)

zinb <- zeroinfl(visits ~ illness + age + gender + health + private + freepoor + freeold
                 + nchronic + lchronic | insurance + illness, data=d, link="logit", dist="negbin")
summary(zinb)

stargazer(hpoisson, hnegbin, zip, zinb, type="text", single.row=TRUE)

#' Note: Stargazer gives you the output of the Poisson/Negative Binomial model, not the 
#' logit model. To see the logit model, you have to use the summary() function.

