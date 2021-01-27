# Pre-processing

#1 Load and Clean file

rm(list=ls())
library(rio)
library(car)
beers=import("6304 Assignment 5 Data.xlsx")
colnames(beers)=tolower(make.names(colnames(beers)))

#2 Rename

str(beers)
names(beers)
colnames(beers)[2]="mo.ML"
names(beers)

#3 Add sequence variable
beers$item=seq(1:nrow(beers))
names(beers)
str(beers)

#4 Add year and month columns
beers$year=as.numeric(format(beers$date,'%y'))
beers$month=as.numeric(format(beers$date,'%m'))
names(beers)
str(beers)

#Analysis

#1 Line Plot
plot(beers$item,beers$mo.ML,pch=19,type="o",
     main="Beers Sales",
     xlab= "Item",
     ylab= "Monthly Beer Production (ML)")

#2 Paramaterize base time series
beertimeseries = ts(beers)
beertimeseries = ts(beers, frequency = 12, start = c(56,1))
print(beertimeseries)
plot.ts(beertimeseries)

regout=lm(mo.ML~item,data=beers)
summary(regout)

#3 Time series data with simple regression line layered on the graph
plot(beers$item,beers$mo.ML,pch=19,type="o",
     main="Beers Sales",
     xlab= "Item",
     ylab= "Monthly Beer Production (ML)")
abline(regout,col="red",lwd=3)

#4 Durbin Watson Test
durbinWatsonTest(regout)

#5 Building Seasonal Indices

# First Creating a Data Frame to hold them.
indices=data.frame(month=1:12,average=0,index=0)
for(i in 1:12) {
  count=0
  for(j in 1:nrow(beers)) {
    if(i==beers$month[j]) {
      indices$average[i]=
        indices$average[i]+beers$mo.ML[j]
      count=count+1
    }
  }
  indices$average[i]=indices$average[i]/count
  indices$index[i]=
    indices$average[i]/mean(beers$mo.ML)}

# Deseasonalizing the original data
for(i in 1:12) {
  for(j in 1:nrow(beers)) {
    if(i==beers$month[j]) {
      beers$deseason[j]=
        beers$mo.ML[j]/indices$index[i]
    }
  }
}

#6 Conducting the deseasonalized regression

#Simple Regression
desreg1.out=lm(deseason~item,data=beers)
summary(desreg1.out)

plot(beers$item,desreg1.out$residuals,type="o",pch=19,
     lwd=3,main="Deseasonalized Residuals")
plot(beers$item,rstandard(desreg1.out),type="o",pch=19,
     lwd=3,main="Standardized Deseasonalized Residuals")
abline(0,0,col="red",lwd=3)
durbinWatsonTest(desreg1.out)
plot(beers$item,beers$deseason,type="o",
     lwd=3,main="Deseasoned Data Plot")
points(beers$item,desreg1.out$fitted.values,
       type="l",col="red",lwd=3)

#2nd Order Regression
desreg2.out=lm(deseason~item+I(item^2),data=beers)
summary(desreg2.out)


plot(beers$item,desreg2.out$residuals,type="o",pch=19,
     lwd=3,main="Deseasonalized Residuals")
plot(beers$item,rstandard(desreg2.out),type="o",pch=19,
     lwd=3,main="Standardized Deseasonalized Residuals")
abline(0,0,col="red",lwd=3)
durbinWatsonTest(desreg2.out)
plot(beers$item,beers$deseason,type="o",
     lwd=3,main="Deseasoned Data Plot")
points(beers$item,desreg2.out$fitted.values,
       type="l",col="red",lwd=3)

#3rd Order Regression
desreg3.out=lm(deseason~item+I(item^2)+I(item^3),data=beers)
summary(desreg3.out)

plot(beers$item,desreg3.out$residuals,type="o",pch=19,
     lwd=3,main="Deseasonalized Residuals")
plot(beers$item,rstandard(desreg3.out),type="o",pch=19,
     lwd=3,main="Standardized Deseasonalized Residuals")
abline(0,0,col="red",lwd=3)
durbinWatsonTest(desreg3.out)
plot(beers$item,beers$deseason,type="o",
     lwd=3,main="Deseasoned Data Plot")
points(beers$item,desreg3.out$fitted.values,
       type="l",col="red",lwd=3)

#7 Reseasonalizing the forecasts 
#Simple Regression
for(j in 1:nrow(beers)) {
  xx=beers$month[j]
  beers$reseason.y1.hat[j]=
    desreg1.out$fitted.values[j]*indices$index[xx]
  beers$reseason.error1[j]=beers$mo.ML[j]-
    beers$reseason.y1.hat[j]
}
plot(beers$item,beers$reseason.y1.hat,pch=19,
     type="o",lwd=3, main="Actual v. Simple Reseasoned Values")
points(beers$item,beers$mo.ML,pch=19,
       type="o",lwd=2,col="red")
cor(beers$item,beers$reseason.y1.hat)
cor(beers$mo.ML,beers$reseason.y1.hat)

#2nd Order Regression
for(j in 1:nrow(beers)) {
  xx=beers$month[j]
  beers$reseason.y2.hat[j]=
    desreg2.out$fitted.values[j]*indices$index[xx]
  beers$reseason.error2[j]=beers$mo.ML[j]-
    beers$reseason.y2.hat[j]
}
plot(beers$item,beers$reseason.y2.hat,pch=19,
     type="o",lwd=3, main="Actual v. 2nd Order Reseasoned Values")
points(beers$item,beers$mo.ML,pch=19,
       type="o",lwd=2,col="red")
cor(beers$item,beers$reseason.y2.hat)
cor(beers$mo.ML,beers$reseason.y2.hat)

#3rd Oder Regression
for(j in 1:nrow(beers)) {
  xx=beers$month[j]
  beers$reseason.y3.hat[j]=
    desreg3.out$fitted.values[j]*indices$index[xx]
  beers$reseason.error3[j]=beers$mo.ML[j]-
    beers$reseason.y3.hat[j]
}
plot(beers$item,beers$reseason.y3.hat,pch=19,
     type="o",lwd=3, main="Actual v. 3rd Order Reseasoned Values")
points(beers$item,beers$mo.ML,pch=19,
       type="o",lwd=2,col="red")
cor(beers$item,beers$reseason.y3.hat)
cor(beers$mo.ML,beers$reseason.y3.hat)

#######################################################
# Calculating Forecast Error for Reseasoned Forecasts
#Simple Regression
beers$error1=beers$mo.ML-beers$reseason.y1.hat

plot(beers$item,beers$error1,pch=19,
     main="Reseasoned Error1")
abline(0,0,col="red",lwd=3)
plot(beers$item,beers$error1,pch=19,
     type="o",main="Reseasoned Error1")
abline(0,0,col="red",lwd=3)

beers$stdized.error1=scale(beers$error1)
plot(beers$item,beers$stdized.error1,pch=19,
     type="o",main="Reseasoned Standardized Error1")
abline(0,0,col="red",lwd=3)

#2nd Order Regression
beers$error2=beers$mo.ML-beers$reseason.y2.hat

plot(beers$item,beers$error2,pch=19,
     main="Reseasoned Error2")
abline(0,0,col="red",lwd=3)
plot(beers$item,beers$error2,pch=19,
     type="o",main="Reseasoned Error2")
abline(0,0,col="red",lwd=3)

beers$stdized.error2=scale(beers$error2)
plot(beers$item,beers$stdized.error2,pch=19,
     type="o",main="Reseasoned Standardized Error2")
abline(0,0,col="red",lwd=3)

#3rd Order Regression
beers$error3=beers$mo.ML-beers$reseason.y3.hat

plot(beers$item,beers$error3,pch=19,
     main="Reseasoned Error3")
abline(0,0,col="red",lwd=3)
plot(beers$item,beers$error3,pch=19,
     type="o",main="Reseasoned Error3")
abline(0,0,col="red",lwd=3)

beers$stdized.error3=scale(beers$error3)
plot(beers$item,beers$stdized.error3,pch=19,
     type="o",main="Reseasoned Standardized Error3")
abline(0,0,col="red",lwd=3)
