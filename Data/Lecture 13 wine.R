wine <- read.table("C:/wine.txt", header = TRUE, as.is = FALSE)
median = summary(wine$PRICE)[3]
plot(wine$VINTAGE, wine$PRICE, type = "b", pch=20); abline(h = median, col="purple")

# winets <- ts(wine, start=c(1952), end=c(1980), frequency=1)

model <- lm(PRICE ~ TEMP + RAIN1 + RAIN2 + AGE, data = wine)
summary(model)
#postest <- wine[ which(model$residuals != NA), ]
#plot(wine$VINTAGE, model$residuals, type = "b", pch=20); abline(h=0, col="red")

unbroken <- wine[ which(wine$VINTAGE > 1956), ]
plot(unbroken$VINTAGE, unbroken$PRICE, type = "b", pch=20); abline(h=mean(unbroken$PRICE), col="red")
plot(unbroken$VINTAGE, unbroken$PRICE, type = "b", pch=20); abline(h=median(unbroken$PRICE), col="red")
plot(unbroken$VINTAGE, unbroken$PRICE, type = "b", pch=20); abline(lm(unbroken$PRICE ~ unbroken$VINTAGE), col="red")
summary((lm(unbroken$PRICE ~ unbroken$VINTAGE)$residuals))

model24 <- lm(PRICE ~ TEMP + RAIN1 + RAIN2 + AGE, data = unbroken)
summary(model24)
plot(unbroken$VINTAGE, model24$residuals, type = "b", pch=20); abline(h=0, col="red")

#install.packages("randtests")
library(randtests)
runs.test(unbroken$PRICE,alternative="two.sided",threshold=mean(unbroken$PRICE))
runs.test(unbroken$PRICE,alternative="two.sided",threshold=median(unbroken$PRICE))
runs.test(model24$residuals,alternative="two.sided",threshold=0)

# library(erer) *** run R script bsLag.R ***
dfdata <- ts(unbroken[, c("VINTAGE", "PRICE")])
lag.wine.1 <- bsLag(h=dfdata, lag=1) # note: not created as a data frame!
dwtest.1 <- lm(PRICE.t_0 ~ PRICE.t_1, data = lag.wine.1); summary(dwtest.1)$coef[2,]
confint(dwtest.1)[2,]
t = (summary(dwtest.1)$coef[2] - 1)/(sqrt(vcov(dwtest.1)[2,2]))

#install.packages("lmtest")
library(lmtest)
dwtest(model24)


