library(foreign)
cps <- read.dta("C:/cps08.dta")
cps$jit <- jitter(cps$yrseduc, factor = 1.5, amount=NULL)
plot(cps$jit, cps$ahe, pch = 21); abline(lm(ahe~yrseduc, cps), col="black")

simple <- lm(ahe~yrseduc, data=cps); summary(simple)

cps$uhat<-simple$residuals; cps$yhat<-simple$fitted.values
plot(jitter(cps$yhat, factor=1.5, amount=NULL), cps$uhat); abline(h=0, col="red")

cps$uhatsq<-cps$uhat^2
bp<-lm(cps$uhatsq~cps$yrseduc); summary(bp)
rsq<-summary(bp)$r.squared
NROW(cps$uhat)*rsq
qchisq(.05, 1, lower.tail=FALSE)
#install.packages("lmtest")
library(lmtest); bptest(simple)

#Robust Standard Errors
library(sandwich)
robse<-vcovHC(simple, type="HC1"); coeftest(simple,robse)
library(car)
robust<-hccm(simple); coeftest(simple,robust) # compare to coeftest(simple)

#FGLS
cps$yrseducsq <- cps$yrseduc^2
fglsmod <- lm(cps$uhatsq ~ cps$yrseduc + cps$yrseducsq); summary(fglsmod)
cps$wsq <- predict(fglsmod)
cps$w <- sqrt(cps$wsq)
cps$one_w <- 1/cps$w
cps$yrseduc_w <- cps$yrseduc/cps$w
cps$ahe_w <- cps$ahe/cps$w
fglsmod <- lm(ahe_w ~ one_w + yrseduc_w - 1, data=cps); summary(fglsmod)
#fglsalt <- lm(ahe_w ~ 0 + one_w + yrseduc_w, data=cps); summary(fglsalt)

#WLS shortcut using "analytic weights" 
wlsmod <- lm(ahe ~ yrseduc, data=cps, weights = 1/wsq); summary(wlsmod)
rm(list=ls())