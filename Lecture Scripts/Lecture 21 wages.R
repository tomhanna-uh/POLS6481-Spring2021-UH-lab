rm(list=ls())
library(foreign)

data <- read.dta("C:/WAGE1.DTA")
sorted <- data[order(data$exper),]
plot(sorted$exper, sorted$wage, pch=19, cex=.67)

linear <- lm(wage ~ exper, sorted); summary(linear)$coefficients
abline(linear, col=4, lwd=3)
summary(linear)$r.squared
anova(linear)

quad <- lm(wage ~ exper + expersq, sorted); summary(quad)$coefficients
#plot(sorted$exper, sorted$wage, pch=19, cex=.67)
lines(sorted$exper, quad$fitted.value, lwd=2, col=2)
summary(quad)$r.squared
anova(quad)
library(car); linearHypothesis(quad, c("expersq=0"))

intercept = quad$coefficients[1]
vertex = -quad$coefficients[2]/(2*quad$coefficients[3])
x <- seq(1,51, length=501)
yhatplot = intercept + quad$coefficients[2]*x + quad$coefficients[3]*(x^2)
plot(x,yhatplot,pch=".", ylim=c(0,25))

dydx = quad$coefficients[2]+(x*2*quad$coefficients[3])
plot(x,dydx,pch="."); abline(h=0, col="red"); abline(v=vertex, lty=3)

round(vcov(quad), digits=6)
a = vcov(quad)[2,2]
c = vcov(quad)[3,3]
b = vcov(quad)[2,3]
sedydx = sqrt(a + (4*b*x) + (4*c*x^2))
plot(x,sedydx,pch=".")
#abline(v=vertex, lty=3)
#abline(v=mean(sorted$exper), lty=2)
#abline(v=mean(x), lty=1)

t = qt(.975,quad$df.residual)
upper = dydx+(t*sedydx)
lower = dydx-(t*sedydx)
plot(x,dydx,pch=".");lines(x,upper);lines(x,lower);abline(h=0,col=2)

# the remaining three lines show how to use predict.lm to create prediction interals around y-hat
# ?predict.lm
a<-predict(linear, interval="confidence")
plot(sorted$exper, sorted$wage, pch=19, cex=.55); abline(linear, col=4, lwd=3)
lines(sorted$exper,a[,2], col=4, lty=3, lwd=2); lines(sorted$exper,a[,3], col=4, lty=3, lwd=2)
