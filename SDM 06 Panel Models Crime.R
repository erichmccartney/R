#' Panel  Models
#' Crimes in 90 North Carolina Counties over 7 years (1981-1987)
#' Level 2: County, Level 1: Year

setwd("~/GitHub/R/DataSets")
library(readxl)
Crime <- read_excel("CrimeNC.xlsx")
View(Crime)
str(Crime)

#' Data Preparation & Cleaning

colnames(Crime)
names(Crime)[3]  <- "crimerate"
names(Crime)[4]  <- "arrest"
names(Crime)[5]  <- "conviction"
names(Crime)[6]  <- "prison"
names(Crime)[7]  <- "sentence"
names(Crime)[8]  <- "police"
names(Crime)[9]  <- "popdensity"
names(Crime)[10] <- "taxrevenue"
names(Crime)[12] <- "urban"
names(Crime)[13] <- "minority"
names(Crime)[names(Crime)=="pctymle"] <- "youngmale"

Crime$wage <- (Crime$wcon + Crime$wtuc + Crime$wtrd + Crime$wfir + 
    Crime$wser + Crime$wmfg + Crime$wfed + Crime$wsta + Crime$wloc)/9
Crime$wcon <- NULL
Crime$wtuc <- NULL
Crime$wtrd <- NULL
Crime$wfir <- NULL
Crime$wser <- NULL
Crime$wmfg <- NULL
Crime$wfed <- NULL
Crime$wsta <- NULL
Crime$wloc <- NULL
str(Crime)
#' write.csv(Crime, file="Crime.csv")

d <- Crime
remove(Crime)
str(d)
d$urban  <- factor(d$urban)
d$region <- factor(d$region)
d$county <- factor(d$county)
d$year   <- factor(d$year)
str(d)
which(! complete.cases(d))
d <- d[complete.cases(d), ]

#' Descriptive Analysis & Visualization

nrow(d)
unique(d$year)
unique(d$county)
length(unique(d$county))
levels(d$urban)
hist(d$crimerate)
hist(log(d$crimerate))                                # We won't use log transformation
hist(d$wage)
summary(cbind(d[, 3:10], d[, 13:16]))
round(cor(cbind(d[, 3:10], d[, 13:16])), 3)

#' install.packages("corrplot")
library(corrplot)
m <- cor(cbind(d[, 3:10], d[, 13:16]))
corrplot(m, method="circle")

#' Q1: How can the State of North Carolina reduce crimerate?
#'     - By increasing police presence? H1a: beta(police) < 0
#'     - By increasing arrests? H2a: beta(arrest) < 0
#' Q2: What are the marginal effects of police and arrest on crimerate?

cor(d$crimerate, d$police)                            # Positive correlation
cor(d$crimerate, d$arrest)                            # Negative correlation
plot(d$crimerate ~ d$police)
abline(lm(d$crimerate ~ d$police), col="red")
plot(d$crimerate ~ d$arrest)
abline(lm(d$crimerate ~ d$arrest), col="red")

#' OLS Model 

ols1 <- lm(crimerate ~ police + arrest, data=d)
summary(ols1)                                         # OLS ignoring county & year
plot(ols1)                                            # Police presence increases crime?

hist(ols1$res)
qqnorm(ols1$res) 
qqline(ols1$res, col="red")
shapiro.test(ols1$res)                                # Residuals not MV normal

plot(ols1$res ~ ols1$fit)
bartlett.test(list(ols1$res, ols1$fit))
norm <- rnorm(630)
bartlett.test(list(ols1$res, norm))                   # Residuals heteroskedastic

library("car")
vif(ols1)                                             # No multicollinearity in data

ols2 <- lm(crimerate ~ police + arrest + county, data=d)
options(max.print=500)
summary(ols2)                                         # OLS with county as factor

ols3 <- lm(crimerate ~ police + arrest + county + year, data=d)
summary(ols3)                                         # OLS with county and year as factor
                                                      # Police presence still increases crime
library(stargazer)
stargazer(ols1, ols2, ols3, type="text", single.row=TRUE)

#' Panel Data Analysis

#' install.packages("plm")
library(plm)
d <- pdata.frame(d, index=c("county", "year"))

pooled <- plm(crimerate ~ police + arrest, data=d, model="pooling")
summary(pooled)                                       # OLS model
plmtest(pooled)                                       # LM test of pooled model 
plmtest(pooled, effect="twoways", type="bp")          # Data shows panel effect

fixed1 <-  plm(crimerate ~ police + arrest, data=d, model="within")
summary(fixed1)                                       # Fixed effects model
fixef(fixed1)                         
summary(fixef(fixed1))

fixed2 <-  plm(crimerate ~ police + arrest, data=d, model="within", effect="twoways")
summary(fixed2)                                       # Two-way fixed effects model
summary(fixef(fixed2))

random <- plm(crimerate ~ police + arrest, data=d, model="random")
summary(random)                                       # Random effects model
ranef(random)
summary(ranef(random))

stargazer(pooled, fixed1, fixed2, random, type="text", single.row=TRUE)

pFtest(fixed1, pooled)                                 # F test for nested models: FE is better
phtest(fixed1, random)                                 # Hausman test: FE is better
#' Note: pFtest and phtest are available in plm package

# Comparing Subsets of Data: West vs. Central

d1 <- subset(d, d$region %in% c("central", "west"))
str(d1)
table(d1$region)
d1 <- droplevels(d1)

plot(crimerate ~ region, data=d1)
t.test(crimerate ~ region, data=d1)
mod1 <- lm(crimerate ~ police + arrest + region, data=d1)
summary(mod1)                      

#' ---------------- IGNORE THE REMAINDER OF THIS CODE ---------------------
#' We will discuss lag analysis in our time-series class

#' Lag Analysis
#' Q1: Is the effect of police on crimerate realized after many years?
#' Q2: If so, how long will the effect of police patrols last?
#' Note; This is multivariate time-series analysis that we will discuss in class later

plot(crimerate ~ police, data=d)
library(foreign)
coplot(crimerate ~ police|as.factor(year), data=d)
coplot(crimerate ~ police|as.factor(county)*as.factor(year), data=d)
library(car)
scatterplot(crimerate ~ police|as.factor(year), data=d, boxplots=FALSE, smooth=TRUE, reg.line=FALSE)
scatterplot(crimerate ~ arrest|as.factor(year), data=d, boxplots=FALSE, smooth=TRUE, reg.line=FALSE)

n <- length(unique(d$county)); n
cid <- unique(d$county); cid                  
start.time <- Sys.time()
dlag <- NULL                              
for(i in 1:n) {
  temp <- NULL
  temp <- d[d$county == cid[i],]         
  temp$policelag1 <- c(NA, temp$police[1:6])
  temp$policelag2 <- c(NA, NA, temp$police[1:5])
  temp$policelag3 <- c(NA, NA, NA, temp$police[1:4])
  temp$policelag4 <- c(NA, NA, NA, NA, temp$police[1:3])
  temp$policelag5 <- c(NA, NA, NA, NA, NA, temp$police[1:2])
  temp$policelag6 <- c(NA, NA, NA, NA, NA, NA, temp$police[1:1])
  dlag <- rbind(dlag,temp)
  print(i)
}
end.time <- Sys.time()
runtime <- round(end.time - start.time, 2)
runtime

write.csv(dlag, file="CrimesLag.csv")
View(dlag)

random <- plm(crimerate ~ arrest + police + policelag1 + policelag2 + 
          policelag3 + policelag4 + policelag5, data=dlag, 
          index=c("county", "year"), model="random")
summary(random) 
ranef(random)

fixed  <- plm(crimerate ~ arrest + police + policelag1 + policelag2 + 
          policelag3 + policelag4 + policelag5, data=dlag, 
          index=c("county", "year"), model="within")
summary(fixed) 

stargazer(pooled, fixed, random, type="text")

#' Note: pFtest and phtest are meant for plm models, not for lmer models.
#' For lmer models, there is a Hausman test in the ivpanel package.
#' Syntax: hausman(fixed, random)
