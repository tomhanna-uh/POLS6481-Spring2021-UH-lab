#install.packages("sandwich"); install.packages("lmtest"); install.packages("stargazer")
library(sandwich); library(lmtest); library(foreign)
smoke <- read.dta("C:/SMOKE.DTA")
#smoke <- read.dta("C:/Users/Scott/SMOKE.DTA")
plot(jitter(smoke$lincome, factor=5), jitter(smoke$cigs, factor=5), pch = 18)
abline(lm(smoke$cigs ~ smoke$lincome), lwd=2, col="blue")

example8.7 <- lm(cigs ~ lincome + lcigpric + educ + age + agesq + restaurn, smoke) #; summary(example8.7)
quatro <- lm(cigs ~ lincome + lcigpric + educ + restaurn, smoke)
anova(quatro, example8.7) # dropping age and agesq not generally recommended
summary(quatro)

plot(quatro$fitted.values, quatro$residuals, pch = 19, cex=.75)
abline(h = 0, col = "red", lwd=3)

vce <- vcov(quatro)
coeftest(quatro,vce) # ordinary standard errors
quatro$newse <- vcovHC(quatro)
coeftest(quatro,quatro$newse) #robust standard errors

# white-test.R contains code for white.test
# run that, then run the following line:
white.test(quatro)

uhatsq <- quatro$residuals^2
breusch <- lm(uhatsq ~ lincome + lcigpric + educ + restaurn, smoke); summary(breusch)
summary(breusch$fitted.values)
yhat = quatro$fitted.values; yhatsq = (quatro$fitted.values)^2
white <- lm(uhatsq ~ yhat + yhatsq); summary(white)
# above is Wooldridge's special version of the white test
vif(white)

luhatsq <- log(uhatsq)
wooldridge <- lm(luhatsq ~ lincome + lcigpric + educ + restaurn, smoke); summary(wooldridge)
summary(wooldridge$fitted.values)
hhat <- exp(wooldridge$fitted.values)
w <- 1/hhat # not necessary to take square root

weighted <- lm(cigs ~ lincome + lcigpric + educ + restaurn, smoke, weights = w)
summary(weighted)

library(stargazer)
stargazer(quatro, weighted, type="text", title="", single.row=FALSE, omit.stat=c("adj.rsq"))

rw = sqrt(w)
yw = smoke$cigs/rw; consw = 1/rw; lincw = smoke$lincome/rw; lcigw  = smoke$lcigpric/rw; educw = smoke$educ/rw; restw = smoke$restaurn/rw
matrix = cbind(yw, consw, lincw, lcigw, educw, restw)
dataframe = data.frame(matrix)
fgls <- lm(yw ~ consw + lincw + lcigw + educw + restw - 1, dataframe)
summary(fgls)

# below is only a replication of Wooldridge's example 8.7, p. 288-289
# run lines 2 and 3 or 4, then:
test8.35 <- lm(cigs ~ lincome + lcigpric + educ + age + agesq + restaurn, smoke); summary(test8.36)
residsq <- test8.35$residuals^2
testbp <- lm(residsq ~ lincome + lcigpric + educ + age + agesq + restaurn, smoke); summary(testbp)
lresidsq = log(residsq)
test8.32 <- lm(lresidsq ~ lincome + lcigpric + educ + age + agesq + restaurn, smoke); summary(test8.32)
test8.33 = exp(test8.32$fitted.values)
w = 1/test8.33 # not necessary to take square root
test8.36 <- lm(cigs ~ lincome + lcigpric + educ + age + agesq + restaurn, smoke, weights = w); summary(test8.36)
