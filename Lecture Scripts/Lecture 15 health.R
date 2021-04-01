options(scipen=999)
library(foreign)
oecd <- read.dta("C:/OECD_health.dta")
plot(oecd$health_expend, oecd$life_expect, pch=18, ylim=c(50,90))
summary(bivar <- lm(life_expect ~ health_expend, oecd))
abline(bivar, col="blue", lwd=2)
plot(oecd$health_expend, bivar$resid, pch=18); abline(h=0, col="red")

qqnorm(oecd$life_expect)

plot(oecd$health_expend, log(oecd$health_expend), pch="-", cex=2)

plot(log(oecd$health_expend), oecd$life_expect, pch=16, xlim=c(4.5, 9.5))
summary(nonlin <- lm(life_expect ~ log(health_expend), oecd))
abline(nonlin, col="blue", lwd=2)

plot(oecd$health_expend, oecd$life_expect, pch=18, ylim=c(50,90))
plot(oecd$health_expend, oecd$life_expect, pch=18, ylim=c(65,85))
x <- seq(from=1, to=9000, length.out=91)
yhat <- 42.4 + 4.64*log(x)
lines(x, yhat, col="blue", lwd=1.5)