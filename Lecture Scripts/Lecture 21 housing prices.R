rm(list=ls())
library(foreign)

data <- read.dta("C:/HPRICE2.DTA")
sorted <- data[order(data$price),]

plot(sorted$lnox, sorted$lprice, pch=19, cex=.67)
plot(sorted$rooms, sorted$lprice, pch=19, cex=.67)

ex6.7 <- lm(lprice ~ lnox + rooms, sorted); round(summary(ex6.7)$coefficients, digits=3)
b0 = ex6.7$coef[1] + ex6.7$coef[2]*mean(sorted$lnox)
abline(a=b0, b=ex6.7$coef[3], col=4, lwd=3)
summary(ex6.7)$r.squared

p197 <- lm(lprice ~ lnox + rooms + log(dist) + stratio, sorted); round(summary(p197)$coefficients, digits=3)
b0 = p197$coef[1] + p197$coef[2]*mean(sorted$lnox) + p197$coef[4]*mean(log(sorted$dist)) + p197$coef[5]*mean(sorted$stratio)
abline(a=b0, p197$coef[3], col=3, lwd=3)
summary(p197)$r.squared

sorted$roomssq <- (sorted$rooms)^2
eq6.14 <-  lm(lprice ~ lnox + rooms + roomssq + log(dist) + stratio, sorted); round(summary(eq6.14)$coefficients, digits=3)
summary(eq6.14)$r.squared
anova(p197, eq6.14)
library(car); linearHypothesis(eq6.14, c("roomssq=0"))

intercept = eq6.14$coef[1] + eq6.14$coef[2]*mean(sorted$lnox) + eq6.14$coef[5]*mean(log(sorted$dist)) + eq6.14$coef[6]*mean(sorted$stratio)
vertex = -eq6.14$coef[3]/(2*eq6.14$coefficients[4])
x <- seq(min(sorted$rooms),max(sorted$rooms), length=101)
yhatplot = intercept + eq6.14$coef[3]*x + eq6.14$coef[4]*(x^2)
lines(x,yhatplot,pch=".", col=2, lwd=3)
abline(v=vertex, lty=3)

dydx = eq6.14$coefficients[3]+(x*2*eq6.14$coefficients[4])
plot(x,dydx,pch="."); abline(h=0, col="red"); abline(v=vertex, lty=3)
