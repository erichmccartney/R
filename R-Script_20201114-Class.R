rm(list=ls())

# Working with Normal Distributions

pnorm(36,72,14)
pnorm(48,72,14) 
pnorm(48,78,14)
pnorm(48,72,9)
dnorm(48,78,14)
curve(dnorm(x,72,14),from=30,to=120,lwd=3,ylim=c(0,.05))
curve(dnorm(x,78,9),from=30,to=120,lwd=3,col="red",add=TRUE)
abline(v=48,lwd=3,col="blue")
my.norms=rnorm(5000,72,14)
mean(my.norms)
sd(my.norms)
hist(my.norms,col="red",main="My Little Red Histogram")
plot(density(my.norms),lwd=3,main="My 5000 Normal Random Deviates")
qqnorm(my.norms,pch=19)
qqline(my.norms,col="red",lwd=3)
library(moments)
skewness(my.norms)
kurtosis(my.norms)

# The Uniform Distribution

my.uniforms=runif(1000,0,1)
hist(my.uniforms, col="red") 
my.uniforms=runif(10000,0,1)
hist(my.uniforms, col="red") 
plot(density(my.uniforms),lwd=3)
qqnorm(my.uniforms,pch=19)
qqline(my.uniforms,col="red",lwd=3)
skewness(my.uniforms)
kurtosis(my.uniforms)
punif(.4,0,1)
punif(.4,0,2)
punif(.4,0,2,lower.tail=FALSE)
curve(dunif(x,0,1),from=0,to=1,lwd=3)
curve(dunif(x,0,1),from=-.5,to=1.5,lwd=3)
curve(dnorm(x,72,14),from=30,to=120,lwd=3,ylim=c(0,.05))
for(i in 8:13){
  curve(dnorm(x,72,i),from=30,to=120,lwd=3,add=TRUE)
}

# The Weibull Distribution

curve(dweibull(x,shape=3,scale=5),from=0,to=20,lwd=3)
curve(dweibull(x,shape=3,scale=5),from=0,to=20,lwd=3,ylim=c(0,.5))
for(i in 1:5){
  curve(dweibull(x,shape=i,scale=5),from=0,to=20,lwd=3,add=TRUE)
}
curve(dweibull(x,shape=3,scale=5),from=0,to=20,lwd=3,ylim=c(0,.5))
for(i in 1:5){
  curve(dweibull(x,shape=3,scale=i),from=0,to=20,lwd=3,add=TRUE)
}

# Working with Data Frames

gilligan=data.frame()
for(i in 1:1000){
  gilligan[i,1]=i
  gilligan[i,2]=i^2
}
colnames(gilligan)=c("First","Second")
gilligan[750,]
head(gilligan)
tail(gilligan)
my.row=data.frame()
my.row[1,1]=1001
my.row[1,2]=my.row[1,1]^2
colnames(my.row)=c("First","Second")
gilligan=rbind(gilligan,my.row)
gilligan[1001,]
tail(gilligan)
plot(gilligan,col="red",
     main="My Little Red Exponential Curve",pch=19)

# Confidence Intervals in R
# Large Sample Size

rm(list=ls())
library(rio)
gpa50=import("November 14 Data Sets.xlsx",
             sheet="Fifty GPAs")
colnames(gpa50)=tolower(make.names(colnames(gpa50)))
attach(gpa50)
t.test(my.50.gpas)
maryann=t.test(my.50.gpas)
maryann 
names(maryann)
maryann$conf.int
maryann$conf.int[1]
maryann$conf.int[2]

# Confidence Intervals in R
# Small Sample Size

gpa15=import("November 14 Data Sets.xlsx",sheet="Fifteen GPAs")
colnames(gpa15)=tolower(make.names(colnames(gpa15)))
attach(gpa15)
t.test(my.15.gpas)
ginger=t.test(my.15.gpas)
ginger 
ginger$conf.int[2]-ginger$conf.int[1]
maryann$conf.int[2]-maryann$conf.int[1]

# Hypothesis Testing in R

pipe=import("November 14 Data Sets.xlsx",sheet="Sewer Pipe")
colnames(pipe)=tolower(make.names(colnames(pipe)))
attach(pipe)
mean(breaking.point)
sd(breaking.point)
skipper=t.test(breaking.point,mu=2400,alternative="greater")
skipper
thurston=t.test(breaking.point,mu=2400,alternative="two.sided")
thurston

maryann=t.test(my.50.gpas,mu=2.1,alternative="greater")
maryann 
maryann$p.value
