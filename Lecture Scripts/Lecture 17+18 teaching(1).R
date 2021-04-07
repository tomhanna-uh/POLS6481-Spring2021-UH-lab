library(foreign)
# library(compactr)
daPe <- read.dta("C:/logist.dta")

linprob <- lm(grade ~ gpa + tuce + psi, daPe)
summary(linprob)
yhat = predict(linprob)
plot(daPe$psi, yhat, ylim=c(-.1,1.1), pch=19, cex=.7); abline(h=0); abline(h=1)
plot(linprob$fitted.values, linprob$residuals, pch=19); abline(h=0, col="red")

gpas <- seq(min(daPe$gpa), max(daPe$gpa), length.out = 1000)
probA <- linprob$coef[1] + linprob$coef[2]*gpas + linprob$coef[3]*21.94 + linprob$coef[4]*.5
plot(gpas, probA, pch="-"); abline(h=0)

# Goldberger correction
# first, recode out-of-bounds values to lowest and highest in-bounds predictions in sample
yhat.pos <- ifelse(yhat<0, c(0.0070157), c(yhat))
plot(yhat, yhat.pos, pch=19)
yhat.01 <- ifelse(yhat.pos>1, c(.9773322), c(yhat.pos))
# second, generate weights
what = 1/(yhat.01*(1-yhat.01))
plot(yhat, sqrt(what))
wls <- lm(grade ~ gpa + tuce + psi, daPe, weights = what); summary(wls)

# Table of predicted probabilities of A, by GPA {q1,median,q3} and PSI {0,1}
summary(daPe)
gpa = c(2.812, 3.065, 3.515, 2.812, 3.065, 3.515)
tuce = c(21.94, 21.94, 21.94, 21.94, 21.94, 21.94)
psi = c(0,0,0,1,1,1)
table.data <- data.frame(gpa, tuce, psi)
round(predict(linprob, newdata=table.data, interval='confidence'), digits=3)
round(predict(linprob, newdata=table.data, interval='prediction'), digits=3)

probit <- glm(grade ~ gpa + tuce + psi, data = daPe, binomial(link = "probit"), na.action=na.exclude)
summary(probit)
phat.p = predict(probit, newdata = table.data, type="response")

xindex <- seq(min(daPe$gpa), max(daPe$gpa), length.out = 101)
yhat.probit.1 <- probit$coef[1] + probit$coef[2]*xindex + probit$coef[3]*22.5 + probit$coef[4]*1
phat.probit.1 <- pnorm(yhat.probit.1)
yhat.probit.0 <- probit$coef[1] + probit$coef[2]*xindex + probit$coef[3]*22.5 + probit$coef[4]*0
phat.probit.0 <- pnorm(yhat.probit.0)

eplot(main = "Effect of GPA on Probability of an A",
      xlab = "GPA",
      ylab = "Predicted Probability of an A",  
      xlim = mm(c(2.0,4.0)),
      ylim = mm(c(0, 1)))
lines(xindex, phat.probit.1, lty=1, lwd=1.6)
lines(xindex, phat.probit.0, lty=2, lwd=1.6)
abline(h=0.5, lwd=1.5, col="grey")
# need to add a legend

logit <- glm(grade ~ gpa + tuce + psi, data = daPe, binomial(link = "logit"), na.action=na.exclude)
summary(logit)
phat.l = predict(logit, newdata = table.data, type="response")
phat.p; phat.l