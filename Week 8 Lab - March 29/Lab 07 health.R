library(here)
library(foreign); library(car)

health <- read.dta(here("data","OECD_health.dta"))

plot(life_expect ~ health_expend, data = health, pch=19)
abline(lm(health$life_expect ~ health$health_expend))

subset <- subset(health, country != "South Africa")

plot(life_expect ~ health_expend, data = subset, pch=19)
abline(lm(subset$life_expect ~ subset$health_expend))

#This plots the same data using the GGPLOT2 package; install ggplot2 if needed
#install.packages("ggplot2")
library(ggplot2)
plot <- ggplot(data=subset,aes(life_expect,health_expend)) +
    geom_point() +
    geom_smooth(method="lm")
plot + ggtitle("Plot of Life Expectancy by Health Expenditure") +
    xlab("Health Expenditure") +
    ylab("Life Expectancy")

plot2 <- ggplot(data=subset,aes(life_expect,health_expend)) +
    geom_point() +
    geom_smooth(method="auto")
plot2 + ggtitle("Plot of Life Expectancy by Health Expenditure") +
    xlab("Health Expenditure") +
    ylab("Life Expectancy")

par(mfrow=c(1,2))
hist(subset$life_expect, freq = FALSE)
qqnorm(subset$life_expect); qqline(subset$life_expect)
hist(log(subset$life_expect), freq = FALSE)
qqnorm(log(subset$life_expect)); qqline(log(subset$life_expect))
hist(subset$health_expend, freq = FALSE)
qqnorm(subset$health_expend); qqline(subset$health_expend)
par(mfrow=c(1,1))

plot(log(life_expect) ~ log(health_expend), data = subset, pch=19)
abline(lm(log(subset$life_expect) ~ log(subset$health_expend)))

subset$health.1k = subset$health_expend/1000
linlin <- lm(life_expect ~ health.1k, subset); summary(linlin)
plot(subset$health.1k, linlin$residuals, pch=19); abline(h=0, col="red") #plot(linlin)

linlog <- lm(life_expect ~ log(health.1k), subset); summary(linlog)
plot(subset$health.1k, linlog$residuals, pch=19); abline(h=0, col="red") #plot(levlog)

uhat <- linlog$residuals; uhatsq <- uhat^2
x <- subset$health.1k; xsq <- (subset$health.1k)^2
white <- lm(uhatsq ~ x + xsq); summary(white)

loglog <- lm(log(life_expect) ~ log(health.1k), subset); summary(loglog)
plot(subset$health.1k, loglog$residuals, pch=19); abline(h=0, col="red") #plot(loglog)

expu <- exp(loglog$residuals)
alph <- mean(expu)
subset$lifehat <- alph*exp(fitted(loglog))
logcheck <- lm(life_expect~lifehat, subset); summary(logcheck)

uhatlog <- loglog$residuals; uhatlogsq <- uhatlog^2
whitelog <- lm(uhatlogsq ~ x + xsq); summary(whitelog)

plot(subset$health.1k, subset$life_expect, pch=21, cex=.75)
lines(sort(subset$health.1k), fitted(logcheck)[order(subset$health.1k)], col='red', type='b', pch=19)
abline(lm(life_expect~health.1k, subset), col='purple', lwd=3)

rm(health); rm(subset); rm(alph); rm(expu)
rm(linlin); rm(linlog); rm(loglog); rm(logcheck)