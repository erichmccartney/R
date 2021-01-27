# Erich McCartney

rm(list=ls())
#install.packages("rio")
#install.packages("moments")
library(rio)
library(moments)


# Pre-processing

#1. Load master data file
taxtt=import("6304 Assignment 2 Data.xlsx")
colnames(taxtt)=tolower(make.names(colnames(taxtt)))
attach(taxtt)

#2. Split data by facility
unique(facility)

facility1.taxtt=subset(taxtt, facility=="Bunnyville")
facility2.taxtt=subset(taxtt, facility=="Dale")
facility3.taxtt=subset(taxtt, facility=="Oblong")
facility4.taxtt=subset(taxtt, facility=="Piopolis")

#3 Random Samples
unum=31484443
set.seed(unum)

random.facility1.taxtt=facility1.taxtt[sample(1:nrow(facility1.taxtt),70),]
random.facility2.taxtt=facility2.taxtt[sample(1:nrow(facility2.taxtt),70),]
random.facility3.taxtt=facility3.taxtt[sample(1:nrow(facility3.taxtt),70),]
random.facility4.taxtt=facility4.taxtt[sample(1:nrow(facility4.taxtt),70),]

# Analysis

#1 90% Confidence interval for Dale
results=t.test(random.facility2.taxtt$transaction.time,conf.level = 0.90)
results

#3 Alpha of 0.05 for Oblong at 8 and 9:15

results=t.test(random.facility3.taxtt$transaction.time, mu=8, 
               alternative = "greater")
results

results=t.test(random.facility3.taxtt$transaction.time, mu=9.25, 
               alternative = "greater")
results

#4 two-tailed hypothesis test would yield p = .05 on Oblong

results=t.test(random.facility3.taxtt$transaction.time, mu = 9.433952, 
               alternative = "two.sided")
results

#5 Notched boxplots for all facilities by transaction time
boxplot(random.facility1.taxtt$transaction.time, 
        random.facility2.taxtt$transaction.time,
        random.facility3.taxtt$transaction.time,
        random.facility4.taxtt$transaction.time,
        main="Transaction time by Facility",
        xlab="Facility Name",
        ylab="Transaction Time",
        names=c("Bunnyville", "Dale", "Oblong", "Piopolis"),
        col="red", pch=1, notch=TRUE)

#6 Statistical difference between Dale and Bunnyville

results=t.test(random.facility1.taxtt$transaction.time, 
               random.facility2.taxtt$transaction.time,
               mu=0,
               alternative=c("two.sided"))
               
results
boxplot(random.facility1.taxtt$transaction.time, 
        random.facility2.taxtt$transaction.time,
        main="Transaction time by Facility",
        xlab="Facility Name",
        ylab="Transaction Time",
        names=c("Bunnyville", "Dale"),
        col="red", pch=1, notch=TRUE)
