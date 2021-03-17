# install.packages("faraway"); install.packages("lmtest")
library(faraway)
data("divusa")
divusa
plot(divusa$year, divusa$divorce, type="b", pch="*", cex=1.5, xlab="Year", ylab="Divorces")

# divorce <- ts(divusa, start=1920, end=1996, frequency=1)
# rm(divorce)

# Start by checking for stationarity
pacf(divusa$divorce)
library(dplyr)
divusa$lag.divorce <- lag(divusa$divorce, n=1L, order_by=divusa$year)
head(cbind(divusa$divorce, divusa$lag.divorce))
cor(divusa$divorce, divusa$lag.divorce, use="complete.obs")

library(randtests)
runs.test(divusa$divorce, alternative="two.sided", threshold=mean(divusa$divorce))

# Dickey-Fuller test for unit root
dftest.L <- lm(divorce ~ lag.divorce, divusa); summary(dftest.L)$coef[2,1:2]; confint(dftest.L)
divusa$dif.divorce <- divusa$divorce - divusa$lag.divorce
dftest.D <- lm(dif.divorce ~ lag.divorce, divusa); summary(dftest.D)$coef[2,1:3]; confint(dftest.D)

# divorce rate looks to be I(1)
# are any explanatory variables I(1) also? 
divusa$lag.femlab <- lag(divusa$femlab, n=1L, order_by=divusa$year)
head(cbind(divusa$femlab, divusa$lag.femlab))
cor(divusa$femlab, divusa$lag.femlab, use="complete.obs")

divusa$lag.marriage <- lag(divusa$marriage, n=1L, order_by=divusa$year)
head(cbind(divusa$marriage, divusa$lag.marriage))
cor(divusa$marriage, divusa$lag.marriage, use="complete.obs")

divusa$lag.birth <- lag(divusa$birth, n=1L, order_by=divusa$year)
head(cbind(divusa$birth, divusa$lag.birth))
cor(divusa$birth, divusa$lag.birth, use="complete.obs")

# could include Dickey-Fuller tests for unit roots in X's

# static models
static <- lm(divorce ~ unemployed + femlab + marriage + birth + military, data=divusa)
summary(static)

library(lmtest)
dwtest(static)

residual <- static$residuals
lag.residual <- lag(residual, n=1L, order_by=divusa$year)
cor(residual, lag.residual, use="complete.obs")
plot(divusa$year, residual, type="l"); abline(h=0, col="red")

runs.test(residual, alternative="two.sided", threshold=0)

#pw.mod <- arima(divusa$divorce, order = c(1,0,0), xreg=cbind(divusa$unemployed,divusa$femlab,divusa$marriage,divusa$birth,divusa$military))
#pw.mod$coef

#install.packages("orcutt")
#library(orcutt)
#co <- cochrane.orcutt(static); co # somehow this estimator isn't converging right now
#install.packages("prais")
library(prais)
prais_winsten(divorce ~ unemployed + femlab + marriage + birth + military, data=divusa)
detach(package:prais)

install.packages("tseries")
library(tseries)
adf.test(divusa$divorce, k=0)
adf.test(divusa$divorce, k=1)
# ?adf.test
# adf.test(divusa$divorce)

lagdv <- lm(divorce ~ lag.divorce + unemployed + femlab + marriage + birth + military, data=divusa)
summary(lagdv)
dwtest(lagdv)

divusa$d.divorce <- c(NA, diff(divusa$divorce))
divusa$d.unemp <- c(NA, diff(divusa$unemployed))
divusa$d.feml <- c(NA, diff(divusa$femlab))
divusa$d.marr <- c(NA, diff(divusa$marriage))
divusa$d.birth <- c(NA, diff(divusa$birth))
divusa$d.milt <- c(NA, diff(divusa$military))
deltas <- lm(d.divorce ~ d.unemp + d.feml + d.marr + d.birth + d.milt, divusa)
summary(deltas) #compare to prais-winsten

dwtest(deltas)
resid.deltas <- deltas$residuals
lag.resid.deltas <- c(NA, diff(resid.deltas))
cor(resid.deltas, lag.resid.deltas, use="complete.obs")
sequence <- which(divusa$year != 1920)
plot(sequence, resid.deltas, type="l"); abline(h=0, col="red")

runs.test(resid.deltas,alternative="two.sided",threshold=0)
