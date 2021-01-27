# Pre-processing

#1 Load and Clean file

rm(list=ls())
library(rio)
library(moments)
cars=import("6304 Assignment 4 Data.xlsx")
colnames(cars)=tolower(make.names(colnames(cars)))

#2 Convert year and origin

str(cars)
cars$year=as.factor(cars$year)
cars$origin=as.factor(cars$origin)
str(cars)
attach(cars)

#3 Random Sample of 70% of data

unum=31484443
set.seed(unum)
random.cars=cars[sample(1:nrow(cars),(NROW(cars)*.7)),]
random.cars$cylinders=as.factor(random.cars$cylinders)
random.cars$make=as.factor(random.cars$make)
random.cars$model=as.factor(random.cars$model)
str(random.cars)

# Analysis

#1 Multiple Regression - MPG vs. CI, HP, and LB
cars.out=lm(mpg~cubic.inches+horsepower+weight,
          data=random.cars)

#2 Model Output
summary(cars.out)

#3 Confidence Interval
confint(cars.out)

#4 Conformity with LINE assumptions of regression
#Linearity
plot(random.cars$mpg,cars.out$fitted.values,
     pch=19,main="Cars Actual v. Fitted Values")
abline(0,1,col="red",lwd=3)
#Normality
qqnorm(cars.out$residuals,pch=19,main="Cars Normality Plot")
qqline(cars.out$residuals,col="red",lwd=3)
#Equality of Variances
plot(rstandard(cars.out),pch=19,
     main="Cars Standardized Residuals")
abline(0,0,col="red",lwd=3)

#5 Identifying high leverage points.
leverages=hat(model.matrix(cars.out))
plot(leverages,pch=19,main="Leverage Plot, MPG Data")
abline(3*mean(leverages),0,col="red",lwd=3)
abline(2*mean(leverages),0,col="blue",lwd=3)
# Report ONLY the year, make, and model
random.cars[leverages>(3*mean(leverages)),c(6, 8, 9)]

#6 Square Horsepower and Weight in Model
#only hp^2
cars1.out=lm(mpg~cubic.inches+horsepower+weight+I(horsepower^2)
             ,data=random.cars)
summary(cars1.out)

#only weight^2
cars2.out=lm(mpg~cubic.inches+horsepower+weight+
               I(weight^2),data=random.cars)
summary(cars2.out)

#both squared is best model
cars3.out=lm(mpg~cubic.inches+horsepower+weight+I(horsepower^2)+
               I(weight^2),data=random.cars)
summary(cars3.out)

par(mfrow=c(1,2))
plot(random.cars$mpg,cars.out$fitted.values,pch=19,
     main="Main Effects Model")
abline(0,1,lwd=3,col="red")
plot(random.cars$mpg,cars3.out$fitted.values,pch=19,
     main="Squared Term Model")
abline(0,1,lwd=3,col="red")
par(mfrow=c(1,1))

plot(random.cars$mpg,cars3.out$fitted.values,pch=19,
     main="Cars3 Actual v. Forecast")
abline(0,1,lwd=3,col="red")
qqnorm(cars3.out$residuals,pch=19,
       main="Cars3 Normality Plot")
qqline(cars3.out$residuals,lwd=3,col="red")
hist(cars3.out$residuals, col = "red")
plot(random.cars$mpg,rstandard(cars3.out),pch=19,
     main="Cars3 Normality Plot")
abline(0,0,col="red",lwd=3)

#7 Add Year to Model
cars4.out=lm(mpg~cubic.inches+year+horsepower+weight,
          data=random.cars)
summary(cars4.out)
confint(cars4.out)

par(mfrow=c(1,3))
plot(random.cars$mpg,cars.out$fitted.values,pch=19,
     main="Main Effects Model")
abline(0,1,lwd=3,col="red")
plot(random.cars$mpg,cars3.out$fitted.values,pch=19,
     main="Squared Term Model")
abline(0,1,lwd=3,col="red")
plot(random.cars$mpg,cars4.out$fitted.values,pch=19,
     main="Main Effects Model w/Year")
abline(0,1,lwd=3,col="red")
par(mfrow=c(1,1))

plot(random.cars$mpg,cars4.out$fitted.values,
     pch=19,main="Cars4 Actual v. Fitted Values")
abline(0,1,col="red",lwd=3)
qqnorm(cars4.out$residuals,pch=19,main="Cars4 Normality Plot")
qqline(cars4.out$residuals,col="red",lwd=3)
plot(rstandard(cars4.out),pch=19,
     main="Cars4 Standardized Residuals")
abline(0,0,col="red",lwd=3)
leverages=hat(model.matrix(cars4.out))
plot(leverages,pch=19,main="Leverage Plot, MPG Data")
abline(3*mean(leverages),0,col="red",lwd=3)
abline(2*mean(leverages),0,col="blue",lwd=3)

cars5.out=lm(mpg~cubic.inches+year+horsepower+weight+I(horsepower^2)+
               I(weight^2),data=random.cars)

par(mfrow=c(1,4))
plot(random.cars$mpg,cars.out$fitted.values,pch=19,
     main="Main Effects Model")
abline(0,1,lwd=3,col="red")
plot(random.cars$mpg,cars3.out$fitted.values,pch=19,
     main="Squared Term Model")
abline(0,1,lwd=3,col="red")
plot(random.cars$mpg,cars4.out$fitted.values,pch=19,
     main="Main Effects Model w/Year")
abline(0,1,lwd=3,col="red")
plot(random.cars$mpg,cars5.out$fitted.values,pch=19,
     main="Squared Term Model w/Year")
abline(0,1,lwd=3,col="red")
par(mfrow=c(1,1))

plot(random.cars$mpg,cars5.out$fitted.values,pch=19,
     main="Cars5 Actual v. Forecast")
abline(0,1,lwd=3,col="red")
qqnorm(cars5.out$residuals,pch=19,
       main="Cars5 Normality Plot")
qqline(cars5.out$residuals,lwd=3,col="red")
hist(cars5.out$residuals, col = "red")
plot(random.cars$mpg,rstandard(cars5.out),pch=19,
     main="Cars5 Normality Plot")
abline(0,0,col="red",lwd=3)
