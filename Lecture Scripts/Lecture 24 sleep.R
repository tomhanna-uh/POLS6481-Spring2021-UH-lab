library(foreign)
sleep <- read.dta("C:/SLP75_81.DTA")

static75 <- lm(slpnap75 ~ totwrk75 + educ75 + male, sleep); summary(static75)

static81 <- lm(slpnap81 ~ totwrk81 + educ81 + male, sleep); summary(static81)

dynamic <- lm(cslpnap ~ ctotwrk + ceduc, sleep); summary(dynamic)

hist(sleep$cslpnap)
hist(sleep$ctotwrk)
hist(sleep$ceduc)

#panel <- read.dta("C:/Users/Scott/SLPpanel.dta")
panel <- read.dta("C:/SLPpanel.dta")
pooledA <- lm(slpnap ~ totwrk + educ, panel); summary(pooledA)

library(plm)

pooled <- plm(slpnap ~ totwrk + educ, panel, model="pooling"); summary(pooled)

fixed <- plm(slpnap ~ totwrk + educ, panel, model="within"); summary(fixed)

random <- plm(slpnap ~ totwrk + educ, panel, model="random"); summary(random)

phtest(fixed, random)