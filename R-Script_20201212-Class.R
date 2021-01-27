# Logistic Regression

rm(list=ls())
library(rio)
gavin=import("6304 December 5 Logistic Regression Data.xlsx",
             sheet="Gavin Fishing")
colnames(gavin)=tolower(make.names(colnames(gavin)))
attach(gavin)

# Parameterizing Model Combinations.

output0=glm(success~1,data=gavin,family=binomial)
output1=glm(success~place,data=gavin,family=binomial)
output2=glm(success~bait,data=gavin,family=binomial)
output3=glm(success~place+bait,data=gavin,family=binomial)
summary(output3)
summary(output0)
summary(output1)
summary(output2)

# Getting beta coefficients and confidence intervals.

coef(output3)
confint(output3)

# Building data frame of output

gavin.coefficients=cbind("Beta Coef"=coef(output3),
                         confint(output3))
gavin.coefficients

#Creating data frame of variable combinations.
#Note UNIQUE function for levels of factor variables.

gavin.predictions=expand.grid(bait=unique(gavin$bait),
                              place=unique(gavin$place))

#Adds new column to data frame which is probability predictions.

gavin.predictions$pred_prob=predict(output3,
                                    newdata=gavin.predictions,type="response")
gavin.predictions

#----------------------------------------------

# Childhood Myopia

rm(list=ls())
myopia=import("6304 December 5 Logistic Regression Data.xlsx",
              sheet="Myopia",skip=2)
colnames(myopia)=tolower(make.names(colnames(myopia)))
attach(myopia)

# Conducting the logistic regression.

myopia.out=glm(myopic~age+gender+mommy+dadmy+tvhr,
               data=myopia, family="binomial")
summary(myopia.out)

# Easier reporting of beta coefficients and confidence intervals.

beta.info=cbind("beta"=coef(myopia.out),confint(myopia.out))
beta.info

# Now Comes Predictions.

pred.data=expand.grid(age=seq(5,9,1),gender=unique(myopia$gender),
                      mommy=unique(myopia$mommy),dadmy=unique(myopia$dadmy),
                      tvhr=quantile(myopia$tvhr,c(.2,.4,.6,.8)))

pred.data$pred.beta=predict(myopia.out,
                            newdata=pred.data,type="link")
pred.data$pred.prob=plogis(pred.data$pred.beta)

pred.data=pred.data[order(pred.data$pred.prob),]
pred.data$index=seq(1,nrow(pred.data),1)
plot(pred.data$index,pred.data$pred.prob,pch=19,col="red",
     main="Myopia Logistic Curve")

# ANOVA 

rm(list=ls())
library(rio)
library(car)
cats1=import("6304 December 12 ANOVA Data Set.xlsx",
             sheet="One Way Cats")
cats1$Flavor=as.factor(cats1$Flavor)

# Equality of variances test.       

leveneTest(Eaten~Flavor,data=cats1)

# Conducting and interpreting the ANOVA -- cats1.

cats1.out=aov(Eaten~Flavor,data=cats1)
summary(cats1.out)
names(cats1.out)
cats1.out$coefficients
beef=subset(cats1,Flavor=="Beef")
mean(beef$Eaten)
chicken=subset(cats1,Flavor=="Chicken")
mean(chicken$Eaten)
mean(chicken$Eaten)-mean(beef$Eaten)
cats1.out$coefficients
gilligan=TukeyHSD(cats1.out)
gilligan
plot(gilligan)

# par(mar sets the margins on the upcoming plot.
# Order of values is:  bottom, left, top, right.
# Default values are 5.1,4.1,4.1,2.1
# Set, plot, reset.

par(mar=c(5.1,8,4.1,2.1))
plot(gilligan,las=2)
par(mar=c(5.1,4.1,4.1,2.1)) 

# Releveling Command

cats1a=cats1
cats1a$Flavor=relevel(cats1a$Flavor,"Kidney")
cats1a.out=aov(Eaten~Flavor,data=cats1a)
summary(cats1a.out)
cats1.out$coefficients
cats1a.out$coefficients
kidney=subset(cats1,Flavor=="Kidney")
mean(kidney$Eaten)
skipper=TukeyHSD(cats1a.out)
par(mar=c(5.1,8,4.1,2.1))
plot(skipper,las=2)
par(mar=c(5.1,4.1,4.1,2.1))

par(mfrow=c(2,1))
par(mar=c(5.1,8,4.1,2.1))
plot(gilligan,las=2,cex.axis=.6)
plot(skipper,las=2,cex.axis=.6)
par(mar=c(5.1,4.1,4.1,2.1))
par(mfrow=c(1,1))

# Randomized Block Cats

cats2=import("6304 December 12 ANOVA Data Set.xlsx",
             sheet="Randomized Block Cats")
cats2$Flavor=as.factor(cats2$Flavor)
cats2$Cat=as.factor(cats2$Cat)
levels(cats2$Flavor)
levels(cats2$Cat)
leveneTest(Eaten~Flavor,data=cats2)
leveneTest(Eaten~Cat,data=cats2)
cats2.out=aov(Eaten~Flavor+Cat,data=cats2)
summary(cats2.out)
cats2.out$coefficients
ginger=TukeyHSD(cats2.out)
ginger
par(mfrow=c(1,2))
par(mar=c(5.1,6,4.1,2.1))
plot(ginger,las=2,cex.axis=.6)
par(mfrow=c(1,1))
par(mar=c(5.1,4.1,4.1,2.1)) 

# Reading in cats3 data.

cats3=import("6304 December 12 ANOVA Data Set.xlsx",
             sheet="Randomized Block Extra Cats")
cats3$Flavor=as.factor(cats3$Flavor)
cats3$Cat=as.factor(cats3$Cat)
levels(cats3$Cat)
mr.smooches=subset(cats3,Cat=="Mr. Smooches")
itty.bitty=subset(cats3,Cat=="Itty-Bitty")
apollonia=subset(cats3,Cat=="Apollonia")
hist(mr.smooches$Eaten,xlim=c(0,3),col="red",
     main="Comparing Cats")
hist(itty.bitty$Eaten,col="blue",add=TRUE)
hist(apollonia$Eaten,col="green",add=TRUE)

# Equality of variances test on cats3.

leveneTest(Eaten~Flavor,data=cats3)
leveneTest(Eaten~Cat,data=cats3)

# Conducting the ANOVA on cats3.

cats3.out=aov(Eaten~Flavor+Cat,data=cats3)
summary(cats3.out)
cats3.out$coefficients
maryann=TukeyHSD((cats3.out))
maryann
par(mfrow=c(1,2))
par(mar=c(5.1,6,4.1,2.1))
plot(maryann,las=2,cex.axis=.7)
par(mar=c(5.1,4.1,4.1,2.1))

# Cats3 releveled for Mr. Smooches

cats3a=cats3
cats3a$Cat=relevel(cats3a$Cat,"Mr. Smooches")
cats3a.out=aov(Eaten~Flavor+Cat,data=cats3a)
summary(cats3a.out)
cats3a.out$coefficients
wally=TukeyHSD((cats3a.out))
par(mfrow=c(1,2))
par(mar=c(5.1,6,4.1,2.1))
plot(wally,las=2,cex.axis=.6)
par(mar=c(5.1,4.1,4.1,2.1))
par(mfrow=c(1,1))

# Cats3 releveled for Itty-Bitty

cats3b=cats3
cats3b$Cat=relevel(cats3b$Cat,"Itty-Bitty")
cats3b.out=aov(Eaten~Flavor+Cat,data=cats3b)
summary(cats3b.out)
cats3b.out$coefficients
beaver=TukeyHSD((cats3b.out))
par(mfrow=c(1,2))
par(mar=c(5.1,6,4.1,2.1))
plot(beaver,las=2,cex.axis=.6)
par(mar=c(5.1,4.1,4.1,2.1))
par(mfrow=c(1,1))

# Now Notice What Happens in the Plot

cats3c=cats3
cats3c$Cat=relevel(cats3c$Cat,"Mr. Smooches")
cats3c$Cat=relevel(cats3c$Cat,"Itty-Bitty")
cats3c.out=aov(Eaten~Flavor+Cat,data=cats3c)
summary(cats3c.out)
cats3c.out$coefficients
eddie=TukeyHSD((cats3c.out))
par(mfrow=c(1,2))
par(mar=c(5.1,6,4.1,2.1))
plot(eddie,las=2,cex.axis=.6)
par(mar=c(5.1,4.1,4.1,2.1))
par(mfrow=c(1,1))


# Returning to Assignment 4 Data

old.cars=import("6304 Assignment 4 Data.xlsx")
names(old.cars)
some.old.cars=subset(old.cars,Year<80)
some.old.cars$Year=as.factor(some.old.cars$Year)
old.cars.out=aov(MPG~Year,data=some.old.cars)
old.cars.contrasts=TukeyHSD(old.cars.out)
plot(old.cars.contrasts,las=2,cex.axis=.25)
