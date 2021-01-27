rm(list=ls())
#install.packages("rio")
#install.packages("moments")
library(rio)
library(moments)
grades=import("6304 EBAIS Presession Data Set.xlsx",
               sheet="scores")
colnames(grades)=tolower(make.names(colnames(grades)))
attach(grades)
# Descriptive Statistics
mean(total.points)
mean(grades$total.points)
median(total.points)
sd(total.points)
summary(total.points)
quantile(total.points,probs=seq(0,1,.25))
quantile(total.points,probs=seq(0,1,.2))
quantile(total.points,probs=seq(0,1,.15))
min(total.points)
max(total.points)
# Structure of the Data Frame
str(grades)
# Fundamental Graphics
hist(total.points)
hist(total.points,col="red",main="My Little Red Histogram")
plot(density(total.points),lwd=3)
boxplot(total.points,col="red",
        main="Total Points Boxplot",pch=19)
skewness(total.points)
kurtosis(total.points)
# Random Sample from Data
my.sample.grades=grades[sample(1:nrow(grades),10),]
mean(my.sample.grades)
set.seed(99)
my.sample.grades=grades[sample(1:nrow(grades),10),]
mean(my.sample.grades)
# Subsetting Data
my.subset.grades=subset(grades,total.points<250)
my.subset.grades=subset(grades,total.points==75)
