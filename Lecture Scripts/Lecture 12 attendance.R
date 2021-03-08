library(foreign)
attend <- read.dta("C:/attend_new.dta")
attend <- read.dta("C:/Users/sjbasing/attend_new.dta")
View(attend)
hist(attend$attend)
plot(attend$attend, attend$final, pch=".", cex=3)
sorted <- attend[order(attend$attend),]
lines(sorted$attend, sorted$samplemean, type="l", lwd=2, col="blue")
# samplemean is average score among students for each integer level of attendance
# see attendance.do Stata do-file... but why didn't I use egen??

simple <- lm(final~ attend, attend); summary(simple)
uhat = simple$residuals
yhat = simple$fitted.values
plot(uhat ~ yhat, cex=.6); abline(h=0, col="red", lwd=1.5)
uhatsq = uhat^2; summary(lm(uhatsq ~ attend$attend))
yhatsq = yhat^2; white <- lm(uhatsq ~ yhat + yhatsq)
summary(white)
qf(.05, 2, summary(white)$df[2], lower.tail=F)

# run white-test.R, then run:
white.test(simple, squares.only = F)
# test shows presence of heteroskedasticity in simple regression model

model <- lm(final ~ attend + ACT + priGPA, attend)
round(summary(model)$coef, digits=3)

uhat = model$residuals
yhat = model$fitted.values
plot(uhat ~ yhat, cex=.6); abline(h=0, col="red")

# uhatsq = uhat^2; yhatsq = yhat^2; white <- lm(uhatsq ~ yhat + yhatsq); summary(white)$fstatistic
# Wooldridge's special version of White's test reveals no heteroskedasticity!

plot(uhat ~ attend$attend, cex=.7); abline(h=0, col="red", lwd=1.5)

library(lmtest)
vce <- vcov(model); coeftest(model,vce)       # ordinary standard errors
library(sandwich)
newse <- vcovHC(model); coeftest(model,newse) #robust standard errors
se.ols = sqrt(diag(vce)); se.rob = sqrt(diag(newse)); compare.se <- cbind(se.ols, se.rob); round(compare.se, digits=5)

#WLS shortcut using "analytic weights" generated based on attendance (note atrocious fit)
attend$attendsq <- attend$attend^2
attend$uhatsq <- uhat^2
fglsmod <- lm(uhatsq ~ attend + attendsq, data=attend); summary(fglsmod)
attend$wsq <- predict(fglsmod)
wlsmod <- lm(final ~ attend + ACT + priGPA, data=attend, weights = 1/wsq); summary(wlsmod)

library(stargazer); stargazer(model, wlsmod, type="text", title="", single.row=FALSE, omit.stat=c("ser"))