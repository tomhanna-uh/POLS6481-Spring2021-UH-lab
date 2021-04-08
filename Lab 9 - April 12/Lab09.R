library(here) ##helps with loading data independent of location or device
library(foreign) ##For loading Stata data##
#install.packages(c("fBasics", "lmtest"))
library(fBasics) ##For skewness and kurtosis tests##
library(lmtest) ##For Ramsey's RESET##

oecd<-read.dta(here("data","oecd.dta"))

qqnorm(oecd$EMPLOY)
hist(oecd$EMPLOY, freq = FALSE)
skewness(oecd$EMPLOY); kurtosis(oecd$EMPLOY)

qqnorm(oecd$GDP)
qqnorm(log(oecd$GDP))


plot(oecd$GDP, oecd$EMPLOY, pch=19); abline(h=0, col="grey")
plot(log(oecd$GDP), oecd$EMPLOY, pch=19); abline(h=0, col="grey")
plot(EMPLOY ~ GDP, oecd, pch=19, cex=.75, log="x"); abline(h=0, col="grey")

oecd$growth1<-oecd$GDP
oecd$growth2<-oecd$GDP^2
oecd$reciprocal<-1/oecd$GDP

##Linear model
linmod<-lm(EMPLOY~growth1, data=oecd); summary(linmod)
plot(oecd$growth1, oecd$EMPLOY, pch=19, cex=.75); abline(linmod, col="red", lwd=2)
qqnorm(linmod$residuals)
plot(oecd$growth1, linmod$residuals, pch=19, cex=.5); abline(h=0, col = "red")
source(here("other scripts","white-test.r"))
white.test(linmod)
resettest(linmod)

##Quadratic Model##
quadmod<-lm(EMPLOY~growth1+growth2, data=oecd); summary(quadmod)
plot(oecd$growth1, oecd$EMPLOY, pch=19, cex=.67); lines(sort(oecd$growth1), fitted(quadmod)[order(oecd$growth1)], col='red', type='b', pch=19)
qqnorm(quadmod$residuals)
plot(oecd$growth1, quadmod$residuals, cex=.5); abline(h=0, col = "red")
white.test(quadmod)
resettest(quadmod)

anova(linmod, quadmod)
library(car); vif(quadmod)

##Calculating marginal effects##
summary(oecd$growth1)
meangdp <- mean(oecd$growth1)
mingdp <- min(oecd$growth1)
maxgdp <- max(oecd$growth1)
coef(summary(quadmod))["growth1","Estimate"]+2*coef(summary(quadmod))["growth2","Estimate"]*meangdp
coef(summary(quadmod))["growth1","Estimate"]+2*coef(summary(quadmod))["growth2","Estimate"]*mingdp
coef(summary(quadmod))["growth1","Estimate"]+2*coef(summary(quadmod))["growth2","Estimate"]*maxgdp
plot(oecd$growth1, oecd$EMPLOY, pch=21, cex=.75)
lines(sort(oecd$growth1), fitted(quadmod)[order(oecd$growth1)], col='red', type='b', pch=19)
lines(sort(oecd$growth1), fitted(linmod)[order(oecd$growth1)], col='blue', type='b', pch=19)

slope = quadmod$coefficients[2] + (2*quadmod$coefficients[3]*oecd$growth1)
vce = vcov(quadmod); sequad = sqrt(vce[2,2] + 4*oecd$growth2*vce[3,3] + 4*oecd$growth1*vce[2,3])
cimax = slope + 2.07*sequad; cimin = slope - 2.07*sequad
par(fig=c(0,1,0,0.75), new=FALSE); plot(oecd$growth1, slope, pch=21, cex=.75, xlab="GDP", ylab="d EMPLOY/d GDP"); arrows(oecd$growth1,cimax,oecd$growth1,cimin,col="blue",angle=90,length=0.1,code=3); abline(h=0, col="red")
par(fig=c(0,1,0.45,1), new=TRUE); boxplot(oecd$growth1, horizontal=TRUE, axes=FALSE)
par(mfrow=c(1, 1))

##Reciprocal Model##
remod<-lm(EMPLOY~reciprocal, data=oecd); summary(remod)
plot(oecd$growth1, oecd$EMPLOY, pch=".", cex=3)
lines(sort(oecd$growth1), fitted(remod)[order(oecd$growth1)], col='red', type='b')
qqnorm(remod$residuals)
plot(remod$fitted.values, remod$residuals); abline(h=0, col="red")
white.test(remod)

##Marginal Effects of Reciprocal Model##
-1*coef(summary(remod))["reciprocal","Estimate"]/(meangdp^2)
-1*coef(summary(remod))["reciprocal","Estimate"]/(mingdp^2)
-1*coef(summary(remod))["reciprocal","Estimate"]/(maxgdp^2)

marginalfx <- -1*coef(summary(remod))["reciprocal","Estimate"]/oecd$growth2
plot(oecd$growth1, marginalfx, pch=21, cex=.75); abline(h=0, col="red")

plot(oecd$growth1, oecd$EMPLOY, pch=".", cex=3)
lines(sort(oecd$growth1), fitted(remod)[order(oecd$growth1)], col='blue', type='b', pch=19)
lines(sort(oecd$growth1), fitted(quadmod)[order(oecd$growth1)], col='green', type='b', pch=19)
abline(linmod, col='purple', lwd=4)