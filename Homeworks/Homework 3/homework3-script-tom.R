library(here)

library(haven)
charity <- read_dta(here("data","charity.dta"))
View(charity)
names(charity)

#Just the regression problem 1
q1 <- lm(gift ~ mailsyear, data = charity)
summary(q1)

#Problem 1 - Test B1 = 1

#Just the regression problem 2
q2 <- lm(gift ~ mailsyear + avggift + propresp, data = charity)
summary(q2)

#Problem 2 - Test B1 = 1

#Problem 2 - F Test avggift  and propresp  B1 and B2 = 0


#Problem 3 - Heteroskedasticity

#Plot residuals against fitted values

#Test for heteroskedasticity

#Re-estimate q2 using FGLS


#Problem 4 - Reestimate q2 with robust standard errors


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

#q8 Heteroskedasticity

#plot residuals against regressors and/or fitted values

#Test for heteroskedasticity

#Re-estimate q7 with FGLS

#Re-estimate q7 with robust standard errors

