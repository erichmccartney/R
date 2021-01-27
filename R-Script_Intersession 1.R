rm(list=ls())
library(rio)

# The IQ Data
# Independent Sampling

iq=import("6304 Intersession 1 Data.xlsx",sheet="IQ")
colnames(iq)=tolower(make.names(colnames(iq)))
attach(iq)
results=t.test(age.25,age.60,mu=0,
               alternative = c("two.sided"))
results
boxplot(age.25,age.60,col="red",
        main="IQ Scores of Age Groups",
        names=c("Younger","Older"))
boxplot(age.25,age.60,col="red",
        main="IQ Scores of Age Groups",
        names=c("Younger","Older"),notch=TRUE)
results=t.test(age.60,age.25,mu=0,
               alternative = c("two.sided"))
results
boxplot(age.60,age.25,col="red",
        main="IQ Scores of Age Groups",
        names=c("Older","Younger"),notch=TRUE)

# The Rat Pups Data
# Paired Comparison

rats=import("6304 Intersession 1 Data.xlsx",
            sheet="Rat Pups")
colnames(rats)=tolower(make.names(colnames(rats)))
attach(rats)
results=t.test(male,female,mu=0,
               alternative=c("two.sided"),paired=TRUE)
results
boxplot(male,female,main="Rat Pup Data Compariston",
        col=c("blue","pink"),
        names=c("Little Boy Rats","Little Girl Rats"))
boxplot(male,female,main="Rat Pup Data Compariston",
        col=c("blue","pink"),
        names=c("Little Boy Rats","Little Girl Rats"),
        notch=TRUE)
results=t.test(female,male,mu=0,
               alternative=c("two.sided"),paired=TRUE)
results
boxplot(female,male,main="Rat Pup Data Compariston",
        col=c("pink","blue"),
        names=c("Little Girl Rats","Little Boy Rats"),
        notch=TRUE)
