library(here)

library(car)
library(foreign)

# uncomment or install package manually if needed
# install.packages("lmtest"); 
library(lmtest)
#uncomment or install package manually if needed
# install.packages("sandwich"); 
library(sandwich)

abdta<-read.dta(here("data","abortions.dta"))
popdta<-read.dta(here("data","populations.dta"))

base<-lm(abortion~price+income+picket+funds, data=abdta);summary(base)
avPlots(base, pch = 19)
plot(base)
abdta$uhat<-base$residuals; abdta$yhat<-base$fitted.values
plot(abdta$income, abdta$yhat); abline(lm(yhat~income, abdta))
plot(abdta$income, abdta$uhat); abline(h=0, col="red")

# Part 1: Diagnosing Heteroskedasticity
abdta$uhatsq<-abdta$uhat^2
bp<-lm(abdta$uhatsq~abdta$price+abdta$income+abdta$picket+abdta$funds); summary(bp)

# LM test (optional)
rsq <- summary(bp)$r.squared
nrsq = NROW(abdta$uhat)*rsq
dfbp = bp$rank-1
pchisq(nrsq, dfbp, ncp=0, lower.tail=FALSE, log.p=FALSE)

##Breusch-Pagan and Cook-Weisberg Tests
bptest(base)
ncvTest(base)

#White Test (Alternative Method)
abdta$yhatsq = (abdta$yhat)^2
whitetest<-lm(uhatsq ~ yhat + yhatsq, abdta); summary(whitetest)
nr2 = NROW(whitetest$residuals)*summary(whitetest)$r.squared
dfwhite = whitetest$rank-1
pchisq(nr2, dfwhite, ncp=0, lower.tail=FALSE, log.p=FALSE)

# run r-script "white-test.R"
white.test(base)
rm(white.test)

#B. Attempting to "fix" heteroskedasticity 
mdata <- merge(abdta, popdta, by="state")
mdata$pop1990k <- mdata$pop1990/1000
plot(mdata$pop1990k, mdata$uhat, pch=19); abline(h=0, col="red")

#FGLS
mdata$pop1990ksq <- mdata$pop1990k^2
mdata$uhatsq <- (mdata$uhat)^2
fglsmod <- lm(mdata$uhatsq ~ mdata$pop1990k + mdata$pop1990ksq); summary(fglsmod)
mdata$wsq <- predict(fglsmod)
mdata$w <- sqrt(mdata$wsq)
mdata$one_w <- 1/mdata$w
mdata$abortion_w <- mdata$abortion/mdata$w
mdata$price_w <- mdata$price/mdata$w
mdata$income_w <- mdata$income/mdata$w
mdata$picket_w <- mdata$picket/mdata$w
mdata$funds_w <- mdata$funds/mdata$w
fglsmod.1 <- lm(abortion_w ~ one_w + price_w + income_w + picket_w + funds_w - 1, data=mdata);summary(fglsmod.1)
fglsmod.2 <- lm(abortion_w ~ 0 + one_w + price_w + income_w + picket_w + funds_w, data=mdata);summary(fglsmod.2)

#WLS shortcut using "analytic weights" 
wlsmod <- lm(abortion ~ price + income + picket + funds, data=mdata, weights = 1/wsq); summary(wlsmod)

#Wooldridge's WLS
mdata$luhatsq <- log(mdata$uhatsq)
wooldridge <- lm(luhatsq ~ price + income + picket + funds + pop1990k + pop1990ksq, data=mdata); summary(wooldridge)
mdata$woolfit <- exp(wooldridge$fitted.values)
mdata$wool.w = 1/mdata$woolfit
wooldridge.wls<-lm(abortion~price+income+picket+funds, data=mdata, weights=mdata$wool.w);summary(wooldridge.wls)

#Robust Standard Errors
lm.mod <- lm(abortion ~ price + income + picket + funds, data=mdata); summary(lm.mod)
robse<-vcovHC(lm.mod, type="HC1"); coeftest(lm.mod,robse)
robust<-hccm(lm.mod); coeftest(lm.mod,robust)

#log transformed dependent variable
mdata$lnabortion <- log(mdata$abortion)
log.mod <- lm(lnabortion ~ price + income + picket + funds, data=mdata); summary(log.mod)
plot(log.mod$fitted.values, log.mod$residuals, pch = 19); abline(h=0, col = "red")
plot(mdata$pop1990k, log.mod$residuals, pch = 19); abline(h=0, col = "red")
bptest(log.mod)
