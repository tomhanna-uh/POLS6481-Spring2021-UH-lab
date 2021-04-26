library(foreign)
data <- read.dta("C:/divorce_Frees.dta")

#install.packages("plm")
library(plm)
pdata<-pdata.frame(data, c("state", "time"))
pdim(pdata)

pooling <- plm(divorce ~ afdc + unemploy, pdata, index=c("area", "year"), model="pooling")
summary(pooling)
pdwtest(pooling)

fixed <- plm(divorce ~ afdc + unemploy, pdata, index=c("area", "year"), model="within")
summary(fixed)
pdwtest(fixed)

random <- plm(divorce ~ afdc + unemploy, pdata, index=c("area", "year"), model="random")
summary(random)
pdwtest(random)
phtest(fixed, random)

library(stargazer)
stargazer(pooling, fixed, random, type="text")
