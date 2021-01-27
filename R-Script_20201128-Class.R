rm(list=ls())
library(rio)

# Warehouse Costs

warehouse=import("6304 November 28 Data Sets.xlsx",
                 sheet="Warehouse Cost",skip=2)
colnames(warehouse)=tolower(make.names(colnames(warehouse)))
attach(warehouse)
names(warehouse)
warehouse.out=lm(cost.000~sales.000+orders,data=warehouse)
summary(warehouse.out)

#Linearity
plot(warehouse$cost.000,warehouse.out$fitted.values,
     pch=19,main="Warehouse Actuals v. Fitted")
abline(0,1,col="red",lwd=3)
cor(warehouse$cost.000,warehouse.out$fitted.values)
cor(warehouse$cost.000,warehouse.out$fitted.values)^2

#Normality
names(warehouse.out)
qqnorm(warehouse.out$residuals,pch=19,
       main="Warehouse Normality Plot")
qqline(warehouse.out$residuals,lwd=3,col="red")
hist(warehouse.out$residuals,col = "red")
library(moments)
skewness(warehouse.out$residuals)

#Equality of Variances
plot(warehouse$cost.000,rstandard(warehouse.out),
     pch=19,main="Warehouse Residual Plot")
abline(0,0,col="red",lwd=3)

#modify y-axis to better identify outliers, ends beyond 3std's
plot(warehouse$cost.000,rstandard(warehouse.out),
     pch=19,
     main="Warehouse Residual Plot",
     ylim=c(-4,4))
abline(0,0,col="red",lwd=3)

#Identifying high leverage points.
lev=hat(model.matrix(warehouse.out))
plot(lev,pch=19,ylim=c(0,.5))
abline(3*mean(lev),0,col="red",lwd=3)
abline(2*mean(lev),0,col="blue",lwd=3)

#A Prediction
maryann=data.frame(nrow=1,ncol=2)
colnames(maryann)=c("sales.000","orders")
maryann[1,1]=300
maryann[1,2]=3000
predict(warehouse.out,maryann,interval="predict")
predict(warehouse.out,maryann,interval="confidence")
confint(warehouse.out)

# MPG Data

rm(list=ls())
cars=import("6304 November 28 Data Sets.xlsx",sheet="MPG")
colnames(cars)=tolower(make.names(colnames(cars)))
attach(cars)
plot(horsepower,mpg,pch=19,main="MPG and Horsepower")
plot(weight,mpg,pch=19,main="MPG and Weight")
plot(cars,pch=19)

#A simple regression first
cars.out=lm(mpg~horsepower,data=cars)
summary(cars.out)
confint(cars.out)
plot(horsepower,mpg,pch=19,main="MPG and Horsepower")
abline(cars.out,lwd=3,col="red")
plot(cars$mpg,rstandard(cars.out),pch=19,
     main="Residual Plot")
abline(0,0,col="red",lwd=3)

#A data transform.
#Squaring the horsepower variable.
#The hard way to do it.
cars$horsepower2=cars$horsepower^2
#And conducting the regression.
cars2.out=lm(mpg~horsepower+horsepower2,data=cars)
summary(cars.out)
summary(cars2.out)
#How's the fit?
par(mfrow=c(1,2))
plot(cars$mpg,cars.out$fitted.values,pch=19,
     main="Main Effects Model")
abline(0,1,lwd=3,col="red")
plot(cars$mpg,cars2.out$fitted.values,pch=19,
     main="Squared Term Model")
abline(0,1,lwd=3,col="red")
par(mfrow=c(1,1))

#The easy way to do it.
#First let's Kondo the data frame.
cars=cars[,-4]
cars2=lm(mpg~horsepower+I(horsepower^2))
summary(cars2.out)
#So let's throw in everything.
cars3.out=lm(mpg~horsepower+weight+I(horsepower^2)+
               I(weight^2),data=cars)
summary(cars3.out)
#No identifiable nonlinear relationship with weight.
cars3.out=lm(mpg~horsepower+weight+I(horsepower^2),
             data=cars)
summary(cars3.out)
#What about an interaction?
cars4.out=lm(mpg~horsepower+weight+I(horsepower^2)+
               horsepower:weight,data=cars)
#An easier way.
cars4.out=lm(mpg~horsepower+weight+
               horsepower:weight+I(horsepower^2))
summary(cars4.out)
#Cars3.out is the best model fit.
plot(cars$mpg,cars3.out$fitted.values,pch=19,
     main="Cars3 Actual v. Forecast")
abline(0,1,lwd=3,col="red")
qqnorm(cars3.out$residuals,pch=19,
       main="Cars3 Normality Plot")
qqline(cars3.out$residuals,lwd=3,col="red")
hist(cars3.out$residuals, col = "red")
plot(cars$mpg,rstandard(cars3.out),pch=19,
     main="Cars3 Normality Plot")
abline(0,0,col="red",lwd=3)


#Child Abuse with Binary Variables

rm(list=ls())
abuse1=import("6304 November 28 Data Sets.xlsx",
              sheet="Child Abuse with Binaries")
colnames(abuse1)=tolower(make.names(colnames(abuse1)))
attach(abuse1)
no.binary.out=lm(reported.victims~pop.under.18,data=abuse1)
summary(no.binary.out)
with.binary.out=lm(reported.victims~pop.under.18+se.state,data=abuse1)
summary(with.binary.out)
confint(with.binary.out)
str(abuse1)

#A better way to model a categorical variable.
abuse2=import("6304 November 28 Data Sets.xlsx",
              sheet="Child Abuse with Binaries 2")
colnames(abuse2)=tolower(make.names(colnames(abuse2)))
attach(abuse2)
better.binary.out=lm(reported.victims~pop.under.18+se.state,data=abuse2)
summary(better.binary.out)
unique(abuse2$se.state)

#House Appraisals

rm(list=ls())
house=import("6304 November 28 Data Sets.xlsx",
             sheet="House Appraisals")
colnames(house)=tolower(make.names(colnames(house)))
str(house)
house$garage=as.factor(house$garage)
house$baths=as.factor(house$baths)
unique(house$baths)
levels(house$garage)
attach(house)
house.out=lm(appraised.value~land.acres+house.size.sqft+
               age+rooms+baths+garage,data=house)
#OR
house.out=lm(appraised.value~.-address,data=house)
summary(house.out)

#Linearity
plot(house$appraised.value,house.out$fitted.values,
     pch=19,main="House Data Actuals v. Fitted")
abline(0,1,lwd=3,col="red")

#Normality
qqnorm(house.out$residuals,pch=19)
qqline(house.out$residuals,lwd=3,col="red")

#Equality of Variances
plot(house.out$fitted.values,rstandard(house.out),pch=19)
abline(0,0,col="red",lwd=3)
house.out=lm(appraised.value~garage,data = house)
summary(house.out)

#Wages

rm(list=ls())
library(rio)

#Read in data.

wages=import("6304 November 28 Wages Data.xlsx",which="Fixed Data")
colnames(wages)=tolower(make.names(colnames(wages)))
attach(wages)

#Copy the continuous variables to a new data object.

some.of.wages=subset(wages,select=c("wage","yearsed",
                                    "experience","age"))

#Correlation analysis of the continuous variables.

plot(some.of.wages,main="Some of Everything with 
Some of Everything")
cor(some.of.wages)
round(cor(some.of.wages),3)

#First put a correlation matrix into an object.

library(corrplot)
xx=cor(some.of.wages)
corrplot(xx,method="circle")
corrplot(xx,method="pie")
corrplot(xx,method="ellipse")
corrplot(xx,method="color")
corrplot(xx,method="number")
corrplot(xx,method="square")
corrplot(xx,method="circle",type="upper")
corrplot(xx,method="circle",type="lower")

#Correlation matrix with p values.

library(Hmisc)
xx=rcorr(as.matrix(some.of.wages))
xx

#Conducting a Regression -- Continuous Variables Only

regout=lm(wage~yearsed+experience+age,data=some.of.wages)
summary(regout)
plot(some.of.wages$wage, regout$fitted.values, pch=19)
abline(0,1,col="red", lwd=3)

#Verifying the r^2 value.
cor(regout$fitted.values,some.of.wages$wage)^2
plot(some.of.wages$wage,regout$fitted.values,pch=19,
     main="Actual v. Fitted Values")

#Exploring binary variables.
#Using the Union variable -- two levels.

regout=lm(wage~yearsed+experience+age+union,data=wages)
summary(regout)

# Adding gender to the model.

regout=lm(wage~yearsed+experience+age+union+gender,
          data=wages)
summary(regout)

# Adding race to the model -- three levels.

regout=lm(wage~yearsed+experience+age+union+gender+race,
          data=wages)
summary(regout)
confint(regout)

# What if you don't want "Hispanic" as the base case?

wages$race=as.factor(wages$race)
wages$race=relevel(wages$race, 'White')
regout=lm(wage~yearsed+experience+age+union+gender+race,
          data=wages)
summary(regout)

# All Variables -- the "kitchen sink" model.

regout=lm(wage~yearsed+experience+age+union+gender+
            race+marr+south+occupation+sector,data=wages)
summary(regout)

#Back to only continuous variables.

regout=lm(wage~yearsed+experience+age,data=some.of.wages)
summary(regout)

#Variance Inflation Factors (VIF)
#Measure of Multicollinearity - 
#correlation of independents.
#How much the variance of a beta coefficient is 
#being inflated by multicollinearity.

#Evidence of Multicollinearity.
plot(some.of.wages)
xx=cor(some.of.wages)
corrplot(xx,method="number")
corrplot(xx,method="ellipse")

#Variance Inflation Factors (VIF)
#Measure of Multicollinearity - 
#correlation of independents.
#How much the variance of a beta coefficient is being
#inflated by multicollinearity.

library(car)
vif(regout)

#Back to the kitchen sink model.

regout=lm(wage~yearsed+experience+age+union+
            gender+race+marr+south+occupation+sector,
          data=wages)
summary(regout)

#Dump Experience, Keep Age
regout=lm(wage~yearsed+age+union+gender+race+
            marr+south+occupation+sector,data=wages)
summary(regout)
vif(regout)

#Dump Age, Keep Experience
regout=lm(wage~yearsed+experience+union+gender+
            race+marr+south+occupation+sector,data=wages)
summary(regout)

#Model with Experience and other 
#continuous variables, Union and Gender
regout=lm(wage~yearsed+experience+union+gender,data=wages)
summary(regout)

#Bringing in Occupation
regout=lm(wage~yearsed+experience+union+gender+occupation,
          data=wages)
summary(regout)

#Only two levels of Occupation seem to have a contribution.
#Now we collapse Occupation to "Professional & Management" 
#and "Other"

wages$pm=NA
for(i in 1:length(wages$occupation)){
  if(wages$occupation[i]=="Management"|
     wages$occupation[i]=="Professional"){
    wages$pm[i]="ProfMgt"}
  else{
    wages$pm[i]="Other"
  }
}

#And conduct a regression with the new variable.
regout=lm(wage~yearsed+experience+union+gender+pm,
          data=wages)
summary(regout)


#Let's separate out Professional and Management.
for(i in 1:length(wages$occupation)){
  wages$pm[i]="Another"
  if(wages$occupation[i]=="Management"){
    wages$pm[i]="Management"}
  if (wages$occupation[i]=="Professional"){
    wages$pm[i]="Professional"
  }
}

#And re-run the regression.
regout=lm(wage~yearsed+experience+union+gender+pm,
          data=wages)
summary(regout)
#And evaluate the standardized residuals.
stdresids=rstandard(regout)
plot(regout$fitted.values,stdresids,pch=19)
abline(0,0,col="red",lwd=3)

#We have an outlier.  Can we get rid of it?  
#We have to find it first.

boxplot(wages$wage,col="red",ylim=c(0,50),pch=19)
max(wages$wage)

#This statement finds the data frame row 
#that's the max value.
which(wages$wage==44.5)
wages[171,]

#Or combine the statements.
wages[which(wages$wage==44.5),]

#Now we create a new data frame that's a copy 
#except for the outlier.
reduced.wages=wages[-171, ]

#Or...
reduced.wages=wages[-which(wages$wage==44.5),]

boxplot(reduced.wages$wage,col="red",ylim=c(0,50),pch=19)

#And rerun the regression.
regout=lm(wage~yearsed+experience+union+gender+pm,
          data=reduced.wages)
summary(regout)
plot(regout$fitted.values,rstandard(regout),pch=19)
abline(0,0,col="red",lwd=3)
abline(3,0,col="red",lwd=3)
qqnorm(regout$residuals,pch=19)
qqline(regout$residuals,col="red",lwd=3)
hist(regout$residuals,col="red")
skewness(regout$residuals)
plot(density(regout$residuals),lwd=3,
     main="Density Plot of Residuals")

#Leverage of Points

lev=hat(model.matrix(regout))
plot(lev,pch=19)
abline(3*mean(lev),0,col="red",lwd=3)
reduced.wages[lev>(3*mean(lev)),]
reduced.wages[lev>(3*mean(lev)),1]

#So let's get rid of the high leverage data points.
no.leverage=reduced.wages
gilligan=reduced.wages[lev>(3*mean(lev)),1]
no.leverage=no.leverage[-gilligan,]

# OR

no.leverage=reduced.wages
no.leverage=
  no.leverage[-(reduced.wages[lev>(3*mean(lev)),1]),]

#And re-run the regression.
regout=lm(wage~yearsed+experience+union+gender+pm,
          data=no.leverage)
summary(regout)

#And look again at the residuals and leverage.
plot(regout$fitted.values,rstandard(regout),pch=19)
abline(0,0,col="red",lwd=3)
qqnorm(regout$residuals,pch=19)
qqline(regout$residuals,col="red",lwd=3)
hist(regout$residuals,col="red")
plot(density(regout$residuals),lwd=3,
     main="Density Plot of Residuals")
lev=hat(model.matrix(regout))
plot(lev,pch=19)
abline(3*mean(lev),0,col="red",lwd=3)
reduced.wages[lev>(3*mean(lev)),1]
plot(no.leverage$wage, regout$fitted.values, pch=19)
abline(0,1,col="red",lwd=3)
