# Erich McCartney

rm(list=ls())
#install.packages("rio")
#install.packages("moments")
library(rio)
library(moments)


# Pre-processing

#1. Load master data file
clist=import("6304 Assignment 3 Data.xlsx")
colnames(clist)=tolower(make.names(colnames(clist)))
attach(clist)

#2 Primary Data set of n=70 randomly selected cars

library(dplyr)
vt.2000.eg.clist=filter(clist, region == "vermont" & 
        year >= "2000" & condition %in% c("excellent", "good"))
        
unum=31484443
set.seed(unum)
random.clist=vt.2000.eg.clist[sample(1:nrow(vt.2000.eg.clist),70),]

#Analysis

#1 Simple Linear Regression: PRICE as dependent and ODOMETER as independent
attach(random.clist)
odo.out=lm(price~odometer, data=random.clist)
summary(odo.out)
confint(odo.out)
plot(random.clist$price,odo.out$fitted.values,
     pch=19,
     main="Miles & Price Plot by Fitted Values")
abline(0,1,lwd=3,col="red")

#6 Determine asking price
#Subset Cadillac DTS model
cadillac.dts.clist = filter(clist, make == "cadillac" &
  model == "dts")
#Linear Regression
dts.out=lm(price~odometer, data = cadillac.dts.clist)
summary(dts.out)
confint(dts.out)
plot(cadillac.dts.clist$price,dts.out$fitted.values,
     pch=19,
     main="Miles & Price Plot by Fitted Values")
abline(0,1,lwd=3,col="red")


#Prediction Parameters
newdata = data.frame(
  year = 2011,
  odometer = 178662,
  condition = "excellent")
#Predict listing price
predict(dts.out, newdata, interval = "confidence")
