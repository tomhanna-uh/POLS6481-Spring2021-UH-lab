data <- read.csv("C:/NYCmarathon.csv")
cor(data$Representative.age, data$Average.finish.time, use="complete.obs")

newdata <- data[ which(data$Representative.age>20), ]
x <- newdata$Representative.age
y <- newdata$Average.finish.time
cor(x,y, use="complete.obs")
summary(lm(y~x))
d <- x/10 # alternatively (x-5)/10
summary(lm(y~d))
cor(d,y, use="complete.obs")

h <- y/60
summary(lm(h~x))
summary(lm(h~x))$coef[2,1]*60
summary(lm(h~x))$coef[2,2]*60
cor(x,h, use="complete.obs")

library(QuantPsyc)
lm.beta(lm(y~x))
lm.beta(lm(y~d))
lm.beta(lm(h~x))

xstd <- Make.Z(x)
ystd <- Make.Z(y)
options(scipen=999)
summary(lm(ystd ~ xstd))
round(summary(lm(ystd ~ xstd))$coef, digits=3)