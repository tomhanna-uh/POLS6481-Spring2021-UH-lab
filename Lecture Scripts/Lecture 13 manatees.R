options(scipen=999)

lagged <- function(x, k) {
  if (k>0) {return (c(rep(NA, k), x)[1 : length(x)] );}
  else {return (c(x[(-k+1) : length(x)], rep(NA, -k)));}
}

manatees <- read.csv("C:/manatees.csv")
plot(manatees$Year, manatees$Kills, type="l", pch=19, lwd=2, cex=.75, xlab="Year", ylab="Deaths")
plot(manatees$Year, manatees$Boats, type="l", pch=19, lwd=2, cex=.75, xlab="Year", ylab="Boats")
plot(manatees$Boats, manatees$Kills, type="b", pch=19, lwd=1, cex=.75, xlab="Boats", ylab="Kills")
pacf(manatees$Kills)
library(dplyr)
manatees$lag.Kills <- lag(manatees$Kills, n=1L, order_by=manatees$Year)
head(cbind(manatees$Kills, manatees$lag.Kills))
cor(manatees$Kills, manatees$lag.Kills, use="complete.obs")
summary(lm(manatees$Kills ~ manatees$lag.Kills))

manatees$lag.Boats <- lag(manatees$Boats, n=1L, order_by=manatees$Year)
head(cbind(manatees$Boats, manatees$lag.Boats))
cor(manatees$Boats, manatees$lag.Boats, use="complete.obs")
summary(lm(manatees$Boats ~ manatees$lag.Boats))

# Dickey-Fuller test for unit root in y (Kills)
dftest.L <- lm(Kills ~ lag.Kills, manatees); summary(dftest.L)$coef[2,1:2]; confint(dftest.L)
(summary(dftest.L)$coef[2,1]-1)/summary(dftest.L)$coef[2,2]; qt(.025, 31)
manatees$d.Kills <- manatees$Kills - manatees$lag.Kills
dftest.D <- lm(d.Kills ~ lag.Kills, manatees); summary(dftest.D)$coef[2,1:3]; confint(dftest.D)

# Dickey-Fuller test for unit root in X (Boats)
dftest.L.B <- lm(Boats ~ lag.Boats, manatees); summary(dftest.L.B)$coef[2,1:2]; confint(dftest.L.B)
(summary(dftest.L.B)$coef[2,1]-1)/summary(dftest.L.B)$coef[2,2]; qt(.025, 31)
manatees$d.Boats <- manatees$Boats - manatees$lag.Boats
dftest.D.B <- lm(d.Boats ~ lag.Boats, manatees); summary(dftest.D.B)$coef[2,1:2]; confint(dftest.D.B)

# cannot perform runs test because Kills = 33 in consecutive years (1985, 1986), so not d.Kills is not dichotomous

# static models
static <- lm(manatees$Kills ~ manatees$Boats); summary(static)
trend <- lm(manatees$Kills ~ manatees$Boats + manatees$Year); summary(trend)

plot(manatees$Year, static$residuals, type = "b", pch=20); abline(h=0, col="red")
library(tseries)
runs.test(factor(sign(resid(static))))
detach(package:tseries)

# Kills is I(1), and Boats is I(1); "cointegrated" means residuals are I(0)
noise = as.vector(resid(static))
lnoise = lagged(noise, 1)
head(cbind(noise, lnoise))
cor(noise, lnoise, use="complete.obs")
summary(lm(noise ~ lnoise))
dnoise = noise-lnoise

# Dickey-Fuller test for autoregression
dftest.L.U <- lm(noise ~ lnoise, use="complete.obs"); summary(dftest.L.U); confint(dftest.L.U)
dftest.D.U <- lm(dnoise ~ lnoise, use="complete.obs"); summary(dftest.D.U); confint(dftest.D.U)

# Durbin-Watson tests for serial autocorrelation using residuals of static model
num = sum(dnoise[2:33]*dnoise[2:33])
denom = sum(noise*noise)
dw = num/denom; dw

library(lmtest)
dwtest(static)

# unnecessary Cochrane-Orcutt re-analysis to account for (meager) serial autocorrelation
#install.packages("orcutt")
library(orcutt)
co <- cochrane.orcutt(static); co

# unnecessary Prais-Winsten re-analyses to account for (meager) serial autocorrelation
#install.packages("prais")
library(prais)
prais_winsten(Kills ~ Boats, data=manatees)
detach(package:prais)

# unnecessary first differenced model that performs really poorly
summary(lm(manatees$d.Kills ~ manatees$d.Boats))