library(foreign)
fertility <- read.dta("C:/FERTIL3.dta")
attach(fertility)
pchrule <- 16 - 3*ww2 - 8*pill
plot(year, gfr, pch=pchrule); title(main = "Births per 1000 Women, Age 15-44")
plot(year, pe, pch=17); title(main = "Personal Income Tax Exemption")

model.static <- lm(gfr ~ pe + ww2 + pill); summary(model.static)
plot(fertility$year, model.static$residuals, pch=18); title(main = "Residuals from Static Model"); abline(h=0, col="red")

#myts <- ts(fertility, start=c(1913), end=c(1984), frequency=1)
model.lagdv <- lm(gfr ~ gfr_1 + pe + ww2 + pill); summary(model.lagdv)

resids.lagdv <- model.lagdv$residuals
year <- subset(fertility$year, year>1913)
plot(year, resids.lagdv, pch=18); title(main = "Residuals from Lag DV Model"); abline(h=0, col="red")

install.packages("TSA")
library(TSA)
dww2 <- zlag(ww2, d=1); dpill <- zlag(pill, d=1)
cww2 <- ww2-dww2; cpill <- pill-dpill
model.diffs <- lm(cgfr ~ cpe + cww2 + cpill)
summary(model.diffs)
