library(here)

library(haven)
charity <- read_dta(here("data","charity.dta"))
View(charity)
names(charity)

#Just the regression problem 1
q1 <- lm(gift ~ mailsyear, data = charity)
summary(q1)

#Problem 1 - Test B1 = 1 (parts 2 and 3)

confint(q1)

#Just the regression problem 2
q2 <- lm(gift ~ mailsyear + avggift + propresp, data = charity)
summary(q2)

#Problem 2 - Test B1 = 1

confint(q2)

#Problem 2 - F Test avggift  and propresp  B1 and B2 = 0
library(car)
linearHypothesis(q2,c("avggift=0","propresp=0"))

#Problem 3 - Heteroskedasticity

#Plot residuals against fitted values

charity$res <- q2$residuals
par(mfrow=c(2,2))
plot(q2,which=1)
plot(charity$mailsyear,charity$res,pch=19,col="purple")
plot(charity$avggift,charity$res,pch=19,col="red")
plot(charity$propresp,charity$res,pch=19,col="blue")
par(mfrow=c(1,1))

#Test for heteroskedasticity


#I use the white test script in the "other scripts" directory
source(here("other scripts","white-test.r"))
white.test(q2)


#Re-estimate q2 using FGLS

charity$sqr <- (charity$res)^2
charity$sqgift <- (charity$gift)^2
fgls <- lm(charity$sqr ~ charity$gift + charity$sqgift)
charity$wsq <- predict(fgls)
wls <- lm(gift ~ mailsyear + avggift + propresp, data = charity, weights = 1/wsq)
summary(wls)

#Problem 4 - Reestimate q2 with robust standard errors
library(sandwich)
robse<-vcovHC(q2)
sqrt(robse[2,2]); sqrt(robse[3,3]); sqrt(robse[4,4])
coeftest(q2,robse)

##Part 2
library(haven)
beauty <- read_dta(here("data","beauty.dta"))
View(beauty)
names(beauty)

#Just the regression problem 5
q5 <- lm(wage ~ looks, data = beauty)
summary(q5)

#Just the regression problem 6
q6 <- lm(wage ~ looks + educ + exper, data = beauty)
summary(q6)

#Just the regression problem 7
q7 <- lm(wage ~ looks + educ + exper + female, data = beauty)
summary(q7)


#q 7.5
q75 <- lm(wage ~ female, data =  beauty)
summary(q75)

#q8 Heteroskedasticity

#plot residuals against regressors and/or fitted values

beauty$res <- q7$residuals
par(mfrow=c(3,2))
plot(beauty$looks,beauty$res,pch=19,col="purple")
plot(beauty$educ,beauty$res,pch=19,col="red")
plot(beauty$exper,beauty$res,pch=19,col="blue")
plot(beauty$female,beauty$res,pch=19,col="blue")
plot(q7,which=1)
par(mfrow=c(1,1))

#Test for heteroskedasticity

beauty$fit <- predict(q7)
beauty$sqr <- (beauty$res)^2
bpreg = lm(sqr ~ looks + educ + exper + female, data = beauty)
summary(bpreg)

#computed the F-stat by pulling the R^2 and DF directly from the model
#using summary
beautyF <- (summary(bpreg)$r.squared/4)/((1-summary(bpreg)$r.squared)/summary(bpreg)$df[2])

#Re-estimate q7 with robust standard errors

q7rse <- vcovHC(q7)

coeftest(q7,q7rse)

