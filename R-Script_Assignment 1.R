# Erich McCartney

rm(list=ls())
#install.packages("rio")
#install.packages("moments")
library(rio)
library(moments)


# Pre-processing

#1. Load master data file

insbmi=import("6304 Assignment 1 Data.xlsx",
              sheet="insurance")
colnames(insbmi)=tolower(make.names(colnames(insbmi)))
attach(insbmi)

#2.	Create 2 subsets; BMI under 25, BMI of 30 and over 

smaller.insbmi=subset(insbmi,bmi<25)
larger.insbmi=subset(insbmi,bmi>=30)
nrow(smaller.insbmi)
nrow(larger.insbmi)


#3 Create 2 data sets of n=50 random samples from each subset in #2

unum=31484443
set.seed(unum)
random.smaller.insbmi=smaller.insbmi[sample(1:nrow(smaller.insbmi),50),]
random.larger.insbmi=larger.insbmi[sample(1:nrow(larger.insbmi),50),]

# Analysis

#1 1.	Mean, Median, StDev, Skewness, and Kurtosis of 
  #the BMI variable for the lower BMI group

summary(random.smaller.insbmi$bmi)
mean(random.smaller.insbmi$bmi)
median(random.smaller.insbmi$bmi)
sd(random.smaller.insbmi$bmi)
skewness(random.smaller.insbmi$bmi)
kurtosis(random.smaller.insbmi$bmi)

 
hist(random.smaller.insbmi$bmi, col="red") 
plot(density(random.smaller.insbmi$bmi),lwd=3)
qqnorm(random.smaller.insbmi$bmi,pch=19)
qqline(random.smaller.insbmi$bmi,col="red",lwd=3)


#2. Boxplot of age variable for lower bmi group

boxplot(random.smaller.insbmi$age,col="green",
        main="Boxplot of Age of BMI<25",pch=19)

#3. Quantile of the charges variable

quantile(insbmi$charges,probs=seq(0,1,.10))

#4. simple histogram of charges variable for the higher bmi group

hist(random.larger.insbmi$charges, breaks=10, xlim=c(0,100000), col="blue",
     main="Histogram of Charges of BMI 30 & Above",)
skewness(random.larger.insbmi$charges)

#5. Dual Boxplot Charges by BMI Group

boxplot(smaller.insbmi$charges, larger.insbmi$charges,
        main="Charges by BMI Group",
        names=c("Smaller People", "Bigger People"),
        col="red", pch=1)

                   
