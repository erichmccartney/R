# Time Series Regression

rm(list=ls())
library(car)
library(rio)
quebec=import("6304 December 5 Time Series Lecture Data Sets.xlsx",
              sheet="Quebec Car Sales",skip=3)
colnames(quebec)=tolower(make.names(colnames(quebec)))
names(quebec)
colnames(quebec)[2]="unit.sales"
names(quebec)
quebec$year=as.numeric(format(quebec$yrmo,'%y'))
quebec$month=as.numeric(format(quebec$yrmo,'%m'))
quebec$item=seq(1:nrow(quebec))
names(quebec)

# Reviewing the raw data.

plot(quebec$item,quebec$unit.sales,pch=19,
     main="Quebec Car Sales")
plot(quebec$item,quebec$unit.sales,pch=19,type="l",
     main="Quebec Car Sales")
plot(quebec$item,quebec$unit.sales,pch=19,type="o",
     main="Quebec Car Sales")

# Basic Regression 

regout=lm(unit.sales~item,data=quebec)
summary(regout)
abline(regout,col="red",lwd=3)

# Durbin Watson Test

durbinWatsonTest(regout)

# Building Seasonal Indices
# First Creating a Data Frame to hold them.

indices=data.frame(month=1:12,average=0,index=0)
for(i in 1:12) {
  count=0
  for(j in 1:nrow(quebec)) {
    if(i==quebec$month[j]) {
      indices$average[i]=
        indices$average[i]+quebec$unit.sales[j]
      count=count+1
    }
  }
  indices$average[i]=indices$average[i]/count
  indices$index[i]=
    indices$average[i]/mean(quebec$unit.sales)}

# Deseasonalizing the original data

for(i in 1:12) {
  for(j in 1:nrow(quebec)) {
    if(i==quebec$month[j]) {
      quebec$deseason[j]=
        quebec$unit.sales[j]/indices$index[i]
    }
  }
}

# Conducting the deseasonalized regression

desreg.out=lm(deseason~item,data=quebec)
plot(quebec$item,desreg.out$residuals,type="o",pch=19,
     lwd=3,main="Deseasonalized Residuals")
plot(quebec$item,rstandard(desreg.out),type="o",pch=19,
     lwd=3,main="Standardized Deseasonalized Residuals")
abline(0,0,col="red",lwd=3)
durbinWatsonTest(desreg.out)
plot(quebec$item,quebec$deseason,type="o",
     lwd=3,main="Deseasoned Data Plot")
points(quebec$item,desreg.out$fitted.values,
       type="l",col="red",lwd=3)

# Reseasonalizing the forecasts 
# and calculating error manually

for(j in 1:nrow(quebec)) {
  xx=quebec$month[j]
  quebec$reseason.y.hat[j]=
    desreg.out$fitted.values[j]*indices$index[xx]
  quebec$reseason.error[j]=quebec$production[j]-
    quebec$reseason.y.hat[j]
}
plot(quebec$item,quebec$reseason.y.hat,pch=19,
     type="o",lwd=3, main="Actual v. Reseasoned Values")
points(quebec$item,quebec$unit.sales,pch=19,
       type="o",lwd=2,col="red")
cor(quebec$item,quebec$reseason.y.hat)
cor(quebec$unit.sales,quebec$reseason.y.hat)

# Calculating Forecast Error for Reseasoned Forecasts

quebec$error=quebec$unit.sales-quebec$reseason.y.hat

# Plotting Reseasoned Error

plot(quebec$item,quebec$error,pch=19,
     main="Reseasoned Error")
abline(0,0,col="red",lwd=3)
plot(quebec$item,quebec$error,pch=19,
     type="o",main="Reseasoned Error")
abline(0,0,col="red",lwd=3)

# Standardizing the Error and Replotting

quebec$stdized.error=scale(quebec$error)
plot(quebec$item,quebec$stdized.error,pch=19,
     type="o",main="Reseasoned Standardized Error")
abline(0,0,col="red",lwd=3)