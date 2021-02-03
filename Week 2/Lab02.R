##Clear memory##
rm(list=ls())
setwd("C:/R Studio Files/Teaching/POLS6481-Spring2021-UH-lab/Week 2")

# install.packages("car")
library(car) ##For the influencePlot command##
library(foreign) ##For loading a Stata dataset##

CEOdta<-read.dta("CEOSAL1.DTA")
sorted <- CEOdta[order(CEOdta$roe),]

plot(sorted$roe, sorted$salary, pch=19)
abline(lm(sorted$salary~sorted$roe), col="red")
reg1<-lm(sorted$salary~sorted$roe); summary(reg1)

summary(sorted$roe); summary(sorted$salary)

coef(reg1)[1]+coef(reg1)[2]*0
coef(reg1)[1]+coef(reg1)[2]*mean(sorted$roe)

bhat = cov(sorted$salary,sorted$roe)/var(sorted$roe) # slope
ahat = mean(sorted$salary) - bhat*mean(sorted$roe) # intercept

round(mean(reg1$residuals), digits=3) # check assumption 1
round(cor(reg1$residuals, sorted$roe), digits=3) # check assumption 2

reg2<-lm(sorted$lsalary~sorted$roe); summary(reg2)

plot(sorted$roe, sorted$lsalary, pch=19)
abline(reg2, col="red")


a<-predict(reg2, interval="confidence")



lines(sorted$roe,a[,2], lty=5)



lines(sorted$roe,a[,3], lty=5) 



##Examine residuals##
reg2.res<-resid(reg2)
summary(reg2.res)
stem(reg2.res)
hist(reg2.res, breaks=50)
plot(CEOdta$roe, reg2.res, ylab="Residuals", xlab="Return on Equity") 
abline(h=0)

hv2<-hatvalues(reg2) # leverage
plot(reg2.res, hv2); abline(h=.0191, col="red")
student2<-rstudent(reg2); stem(student2)
plot(hv2, student2); abline(h=1.96, col="red"); abline(h=-1.96, col="red")
cooks2<-cooks.distance(reg2); stem(cooks2) # cook's distance
influencePlot(reg2, main="Influence Plot", sub="Circle size is proportial to Cook's Distance")

data <- read.csv("accidents50.csv")
plot(data$Density, data$Deaths.per.100k, pch=19)
plot(log(data$Density), log(data$Deaths.per.100k), pch=19)
reg3<-lm(log(data$Deaths.per.100k) ~ log(data$Density)); summary(reg3)
plot(log(data$Density), log(data$Deaths.per.100k), pch=19); abline(reg3, col="red")

##Measures of influence##
hv3<-hatvalues(reg3)
student3<-rstudent(reg3)
cooks3<-cooks.distance(reg3)
dffits3<-dffits(reg3)
diag<-as.data.frame(cbind(data$State,hv3,student3,cooks3,dffits3))

diags<-diag[order(-hv3),] 
# head(round(diags, digits=3))
head(diags)
dfbeta3<-dfbeta(reg3); head(dfbeta3)

plot(hv3, abs(student3), pch=19); abline(v=.08, col="blue"); abline(h=2, col="red")

dummy<-ifelse(data$State=="Alaska",c(1),c(0)) 
reg4<-lm(log(data$Deaths.per.100k) ~ log(data$Density) + dummy)
summary(reg4)
plot(log(data$Density), log(data$Deaths.per.100k), pch=19); abline(reg3, col="black"); abline(reg4, col="blue")
remove = -c(2)
reg5<-lm(log(data$Deaths.per.100k) ~ log(data$Density), subset=remove)
summary(reg5)
abline(reg5, col="green")