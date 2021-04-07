library(foreign)
mroz <- read.dta("C:/MROZ.DTA")

# chapter 7, pages 249-251
eq7.29 <- lm(inlf ~ nwifeinc + educ + exper + expersq + age + kidslt6 + kidsge6, mroz)
summary(eq7.29)
# Figure 7.3, page 250 (note that educ takes on values from 5 to 17) -- summary(mroz$educ)
educlt5 <- seq(0, 5, length=501)
educge5 <- seq(5, 17, length=1201)
yhatlt5 <- eq7.29$coef[1] + eq7.29$coef[2]*50 + eq7.29$coef[3]*educlt5 + eq7.29$coef[4]*5 + eq7.29$coef[5]*25 + eq7.29$coef[6]*30 + eq7.29$coef[7] + eq7.29$coef[2]*0
yhatge5 <- eq7.29$coef[1] + eq7.29$coef[2]*50 + eq7.29$coef[3]*educge5 + eq7.29$coef[4]*5 + eq7.29$coef[5]*25 + eq7.29$coef[6]*30 + eq7.29$coef[7] + eq7.29$coef[2]*0
plot(educge5, yhatge5, type="l", lty=1, lwd=2, col="chartreuse4", xlim=c(0,17), ylim=c(-0.2,0.6), xlab="educ", ylab="probability")
abline(h=0, col="black"); abline(h=.5, col="black", lty=2)
lines(educlt5, yhatlt5, type="l", lty=3, lwd=2, col="chartreuse2")

# see also exercise 7 on page 260

# chapter 8, page 294
vce=vcov(eq7.29)
library(lmtest)
coeftest(eq7.29, vcov = vce)[,1:3]
library(sandwich)
robust = vcovHC(eq7.29, type = "HC0")
coeftest(eq7.29, vcov = robust)[,1:3]

# chapter 17, pages 593-595
logit <- glm(inlf ~ nwifeinc + educ + exper + expersq + age + kidslt6 + kidsge6, mroz, family=binomial(link="logit"))
summary(logit)
probit <- glm(inlf ~ nwifeinc + educ + exper + expersq + age + kidslt6 + kidsge6, mroz, family=binomial(link="probit"))
summary(probit)

library(stargazer)
stargazer(eq7.29, logit, probit, type="text", title="Table17.1 LPM, Logit, and Probit Estimates of Labor Force Participation", 
          single.row=FALSE, ci=FALSE, omit.stat=c("n", "f", "ser", "aic"))
# http://127.0.0.1:26691/help/library/stargazer/help/list%20of%20statistic%20codes

# add some graphs
yhat.logit.0 <- logit$coef[1] + logit$coef[2]*50 + logit$coef[3]*educge5 + logit$coef[4]*5 + logit$coef[5]*25 + logit$coef[6]*30
phat.logit.0 <- (1+exp(-yhat.logit.0))^-1
yhat.logit.1 <- logit$coef[1] + logit$coef[2]*50 + logit$coef[3]*educge5 + logit$coef[4]*5 + logit$coef[5]*25 + logit$coef[6]*30 + logit$coef[7]*1
phat.logit.1 <- (1+exp(-yhat.logit.1))^-1
yhat.logit.2 <- logit$coef[1] + logit$coef[2]*50 + logit$coef[3]*educge5 + logit$coef[4]*5 + logit$coef[5]*25 + logit$coef[6]*30 + logit$coef[7]*2
phat.logit.2 <- (1+exp(-yhat.logit.2))^-1
plot(educge5, phat.logit.0, type="l", lty=1, lwd=2, col="darkblue", xlim=c(0,17), ylim=c(0,1), xlab="educ", ylab="probability", bty = "n")
abline(h=0); abline(h=.5, col="black", lty=2); abline(h=1)
lines(educge5, phat.logit.1, type="l", lty=2, lwd=2, col="cyan4")
lines(educge5, phat.logit.2, type="l", lty=3, lwd=2, col="chartreuse4")
