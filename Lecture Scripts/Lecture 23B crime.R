load("C:/crime1.RData"); attach(data)
cor(narr86, inc86, use="complete.obs")

plot(narr86 ~ inc86, pch=19, cex=.75)

pmodel <- glm(narr86 ~ pcnv + inc86 + black + hispan, poisson)
#lmodel <- lm(narr86 ~ pcnv + inc86 + black + hispan)
#library(stargazer)
#stargazer(lmodel, pmodel, type="text", title="", single.row=FALSE, omit.stat=c("f", "ser"))
summary(pmodel)
postest=data.frame(data,pred=pmodel$fitted)
fits=lm(narr86~pred, postest); summary(fits)$r.squared

confint(pmodel)
beta  <- pmodel$coef

exp(beta[3])
exp(beta[3])-1
# Interpretation: each $100 increase in income is associated with a .86% decrease in the mean number of arrests.
exp(confint(pmodel,3))
exp(confint(pmodel,3))-1
# Interpretation: 95% confidence interval is a .7% to 1% decrease in the mean number of arrests.

exp(beta[4])-1
exp(confint(pmodel,4))-1
# Interpretation: black male has 90% higher number of expected arrests than white male, all else equal
exp(beta[5])-1
exp(confint(pmodel,5))-1
# Interpretation: hispanic male has 60% higher number of expected arrests than white male, all else equal

# Fix pcnv = 0 & income = 55; compare white vs. black vs. hispanic male
# white
wm55 = beta[1] + beta[2]*0 + beta[3]*55 + beta[4]*0 + beta[5]*0; wm55
exp(wm55)                   # expected or mean # of arrests
exp(-exp(wm55))             # probability of 0 arrests
exp(-exp(wm55))*(exp(wm55)) # probability of 1 arrest
dpois(1, exp(wm55))
dpois(2, exp(wm55))

# Black
bm55 = beta[1] + beta[2]*0 + beta[3]*55 + beta[4]*1 + beta[5]*0; bm55
exp(bm55)                   # expected or mean # of arrests
exp(bm55)/exp(wm55) - 1     # proportionate change versus white male
exp(-exp(bm55))             # probability of 0 arrests
exp(-exp(bm55))*(exp(bm55)) # probability of 1 arrest
dpois(1, exp(bm55))
dpois(2, exp(bm55))

# hispanic
hm55 = beta[1] + beta[2]*0 + beta[3]*55 + beta[4]*0 + beta[5]*1; hm55
exp(hm55)                   # expected or mean # of arrests
exp(hm55)/exp(wm55) - 1     # proportionate change versus white male
exp(-exp(hm55))             # probability of 0 arrests
exp(-exp(hm55))*(exp(hm55)) # probability of 1 arrest
dpois(1, exp(hm55))
dpois(2, exp(hm55))

# install.packages("car")
library(car)
linearHypothesis(pmodel, c("black=0"))
summary(pmodel)$coef[4,3]^2
linearHypothesis(pmodel, c("hispan=0"))
summary(pmodel)$coef[5,3]^2
linearHypothesis(pmodel, c("black=0", "hispan=0"))
linearHypothesis(pmodel, c("black = hispan"))

#  hypothesis tests of equality of coefficients
diff = pmodel$coef[4] - pmodel$coef[5]
vce = vcov(pmodel)
sediff = sqrt(vce[4,4] + vce[5,5] - vce[4,5] - vce[5,4])
diff/sediff; (diff/sediff)^2

se.naive = sqrt(summary(pmodel)$coef[4,2]^2 + summary(pmodel)$coef[5,2]^2)
se.naive; sediff
diff/se.naive; (diff/se.naive)^2

# robust standard errors
library(sandwich)
robvar<-vcovHC(pmodel, type="HC0")
robse <- sqrt(diag(robvar))
sediffr = sqrt(robvar[4,4] + robvar[5,5] - robvar[4,5] - robvar[5,4])
diff/sediffr; (diff/sediffr)^2
linearHypothesis(pmodel, c("black = hispan"), vcov=robvar)

r.est <- cbind("Estimate"=coef(pmodel), "Robust SE"=robse, "Pr(>|z|)" = 2*pnorm(abs(coef(pmodel)/robse), lower.tail=FALSE),
               LL = coef(pmodel) - 1.96*robse, UL = coef(pmodel) + 1.96*robse)
options(scipen=999); round(r.est, digits=5)

## GRAPHICAL DISPLAY FOR PRESENTATION AND COMPARISON
plot(narr86 ~ inc86, ylab="Number of Arrests",
     xlab="Income (Hundreds of $)", ylim=c(0,2),
     pch=19, bg="green", cex=.75, lwd=2)
seqinc <- seq(min(inc86),max(inc86), length=101)
lpw <- beta[1] + beta[3]*seqinc
curvew <- exp(lpw)
lpb <- beta[1] + beta[4] + beta[3]*seqinc
curveb <- exp(lpb)
lph <- beta[1] + beta[5] + beta[3]*seqinc
curveh <- exp(lph)
lines(curvew ~ seqinc,lwd=2); lines(curveb ~ seqinc,lwd=2); lines(curveh ~ seqinc,lwd=2) 