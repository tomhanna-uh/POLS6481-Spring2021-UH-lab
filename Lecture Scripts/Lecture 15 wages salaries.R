# compare level-level model with log-level model using WAGE1 data
library(foreign); wages <- read.dta("C:/WAGE1.DTA")
plot(wages$educ, wages$wage, pch=18); abline(lm(wage~educ, wages), col="blue")
eq.2.27 <- lm(wage~educ, wages); summary(eq.2.27)
plot(wages$educ, eq.2.27$residuals, pch=18); abline(h=0, col="red")

plot(wages$wage, wages$lwage, pch=18)
qqnorm(wages$wage)
qqnorm(wages$lwage)

plot(wages$educ, wages$lwage, pch=18); abline(lm(lwage~educ, wages), col="blue")
eq.2.44 <- lm(lwage~educ, wages); summary(eq.2.44)
plot(wages$educ, eq.2.44$residuals, pch=18); abline(h=0, col="red")

# compare level-level model with log-log model using CEOSAL1 data
salaries <- read.dta("C:/CEOSAL1.DTA")
plot(salaries$roe, salaries$salary, pch=18)
eq.2.39 <- lm(salary~roe, salaries); summary(eq.2.39); abline(eq.2.39, col="blue")
plot(salaries$roe, eq.2.39$residuals, pch=18); abline(h=0, col="red")

plot(salaries$lsales, salaries$lsalary, pch=18)
eq.2.45 <- lm(lsalary~lsales, salaries); summary(eq.2.45); abline(eq.2.45, col="blue")
plot(salaries$lsales, eq.2.45$residuals, pch=18); abline(h=0, col="red")

eq.6.25 <- lm(salary~sales + roe, salaries); summary(eq.6.25)

eq.6.26 <- lm(lsalary~lsales + roe, salaries); summary(eq.6.26)
salaries$l.fitted <- eq.6.26$fitted.values
salaries$e.l.fitted <- exp(salaries$l.fitted)
a0 = exp(((summary(eq.6.26)$sigma)^2)/2)
a1 = mean(exp(eq.6.26$residuals))
salaries$fitted <- salaries$e.l.fitted*a1
fit <- lm(salary~fitted, salaries); summary(fit)$r.squared
