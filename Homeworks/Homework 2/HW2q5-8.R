data <- read.csv("C:/globaled.csv")
names(data)
attach(data)

pairs(data[,c(6,5,7,9)], pch=19)

VCM <- cov(data[,c(6,5,7,9)])
VCM
round(cor(VCM),digits=3)

question5 <- lm(ypcgr ~ edavg)
summary(question5)

question5.5 <- lm(ypcgr ~ ypc60)
summary(question5.5)

question6 <- lm(ypcgr ~ edavg + ypc60)
summary(question6)
plot(ypc60, edavg, pch=19)

# an aside: 
summary(lm(ypcgr ~ ypc60))
# simple regression of average growth on 1960 growth also negative coeff, but smaller -- 1/3 as large!

question7 <- lm(ypcgr ~ edavg + ypc60 + testavg)
summary(question7)
plot(edavg, testavg, pch=19)
library(car); vif(question7)

question8 <- lm(ypcgr ~ ypc60 + testavg)
summary(question8)
summary(lm(question8$residuals ~ edavg))

question9 <- lm(LSI ~ DEMOCRACY)
summary(question9)
question9a <- lm(DEMOCRACY ~ CORRUPT + LIFE)
summary(question9)$r.squared
summary(question8)$sigma; summary(question9)$sigma

summary(lm(DEMOCRACY ~ LIFE + CORRUPT))
