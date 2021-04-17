#' Multi-Level Models using lme4
#' Math Achievement: Analysis of math scores of 7145 students' from 160 schools
#' Data at two levels: Level 1 = Students, Level 2 (block/group) = Schools

setwd("C:/Users/abhatt/Desktop/SDM/Data")

#' Read data sets from nlme library

library(nlme)
data(MathAchieve)
View(MathAchieve)
data(MathAchSchool)
View(MathAchSchool)
write.csv(MathAchieve, file="MathAchieve.csv")
write.csv(MathAchSchool, file="MathAchSchool.csv")

#' Merge the two data sets into a common data frame

d <- merge(MathAchieve, MathAchSchool, by="School")
View(d)
cor(d$MEANSES.x, d$MEANSES.y)
d$MEANSES.y <- NULL
d$MEANSES <- d$MEANSES.x
d$MEANSES.x <- NULL
View(d)
write.csv(d, file="MathAch.csv")
rm(MathAchieve)
rm(MathAchSchool)
str(d)

length(unique(d$School))
class(d$School)
hist(d$MathAch)

#' Linear mixed model (LMM) analyis using lme4 package
#' LMM: an extension of linear models (LM) where the linear predictor contains random effects 
#' in addition to the usual fixed effects.
#' LME: linear mixed effects; mixed effects models contain both fixed and random effects.
#' Note: In lme4, random effects are specified within parentheses; fixed effects outside parenthesis.

library(lme4)                  

#' Interclass correlation (ICC) can be computed using merTools package
#' install.packages("merTools")
library(merTools)
ICC(outcome="MathAch", group="School", data=d)

#' Random intercept model: How do math achievement vary across schools?
#' Note: If REML (Restricted Maximum Likelihood) is set to FALSE, we use standard MLE
m1 <- lmer(MathAch ~ 1 + (1 | School), data=d, REML=FALSE)   
summary(m1)
confint(m1)
AIC(m1)
fixef(m1)                                       # Magnitude of fixed effect
ranef(m1)                                       # Magnitude of random effect
coef(m1)                                        # Magnitude of total effect

#' Random intercept model with fixed Level 1 predictors 
m2 <- lmer(MathAch ~ SES + Sex + Minority + (1 | School), data=d, REML=FALSE) 
summary(m2)
confint(m2)
anova(m1, m2)                                   # Chi-sq test for comparing nested models 

#' Random intercept model with fixed Level 1 and Level 2 predictors 
m3 <- lmer(MathAch ~ SES + Sex + Minority + Sector + Size + (1 | School), data=d, REML=FALSE) 
summary(m3)
ranef(m3)
anova(m2, m3)

library(stargazer)
stargazer(m1, m2, m3, type="text")

#' Model with two Level 2 random intercepts
m4 <- lmer(MathAch ~ 1 + (1 | School) + (1 | Size), data=d, REML=FALSE)   
summary(m4)
anova(m1, m4)                                   # Adding size adds no further explanation to schools
coef(m4)

#' Model with two Level 2 random intercepts
m5 <- lmer(MathAch ~ 1 + (1 | School) + (1 | Sector), data=d, REML=FALSE)
summary(m5)
anova(m1, m5)                                   # m1 and m5 are not nested models
coef(m5)

#' Random intercept model with nested Level 2 predictor (in 3-level models)
#' Examines random intercept varying among sector and school within sector
m6 <- lmer(MathAch ~ 1 + (1 | Sector/School), data=d, REML=FALSE)
summary(m6)                          
anova(m1, m6)
coef(m6)
#' Question: Is Sector a fixed effect (as in m3) or a random effect (as in m6)?
#' One modeler's random effect is another modeler's fixed effect. 

#' What are we trying to do in the following models?
m7 <- lmer(MathAch ~ 1 + (1 | Sector/Size), data=d, REML=FALSE)
summary(m7)
coef(m7)

m8 <- lmer(MathAch ~ 1 + (1 | Sector/School) + (1 | Size), data=d)
summary(m8)                                     # Why are the random effects of size zero?
ranef(m8)
coef(m4)

m9 <- lmer(MathAch ~ SES + Sex + Minority + (1 | School) + (1 | Sector), data=d, REML=FALSE)
summary(m9)
coef(m9)
anova(m2, m9)
#' Note: We can model School and Sector as separate random effects because they are at different levels.
#' Alternatively, we can model this as a 3-level problem with School nested within Sector (m6).

#' Correlated random slope and random intercept model
#' Note: Random slope to the left of |, random intercept to the right of |
m10 <- lmer(MathAch ~ SES + Sex + Minority + (1 + SES | School), data=d, REML=FALSE)
summary(m10)
coef(m10)
anova(m2, m10)                                   
#' Question: Why is it that m9 is better fit than m2, but m10 is not?

