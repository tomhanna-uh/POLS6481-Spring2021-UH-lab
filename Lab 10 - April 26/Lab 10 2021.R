library(here)
library(foreign)
library(tseries) #For lagging
library(lmtest) #For test for joint significance
library(plm) #For plm command  --- if needed install.packages("plm")

library(pcse) #For panel corrected standard errors -- if needed install.packages("pcse")

data<-read.dta(here("data","fatality.dta"))
pdata<-pdata.frame(data, c("state", "year"))
pdim(pdata)

pdata$vfrall<-10000*pdata$mrall

# Analyses of 1982 and 1988
plot(pdata$beertax[pdata$year==1982], pdata$vfrall[pdata$year==1982],
     xlab="Tax on Case of Beer", ylab="vfrall", xlim=c(0,3), ylim=c(1,4))
bimod.82<-lm(pdata$vfrall[pdata$year==1982]~pdata$beertax[pdata$year==1982])
abline(bimod.82, col="red")

plot(pdata$beertax[pdata$year==1988], pdata$vfrall[pdata$year==1988],
     xlab="Tax on Case of Beer", ylab="vfrall", xlim=c(0,3), ylim=c(1,4))
bimod.88<-lm(pdata$vfrall[pdata$year==1988]~pdata$beertax[pdata$year==1988])
abline(bimod.88, col="red")

## Pooled OLS for all years, with control variables (logged real income per capita, average vehicle miles per driver)
pdata$lincperc<-log(data$perinc/1000)
pdata$vmilespd<-data$vmiles/1000

base<-lm(vfrall~beertax+lincperc+vmilespd, data=pdata)
summary(base)

pdata$lag.vfrall<-lag(pdata$vfrall, 1) 
lagbase<-lm(vfrall~beertax+lincperc+vmilespd+lag.vfrall, data=pdata); summary(lagbase)

pdata$lag.beertax<-lag(pdata$beertax, 1) 
cor(pdata$lag.beertax, pdata$beertax, use="na.or.complete")

beercheck<-lm(beertax~lag.vfrall, data=pdata); summary(beercheck)

##Fixed effects estimators
fixed1 <-lm(vfrall ~ beertax+lincperc+vmilespd+factor(state) - 1, data=pdata)
summary(fixed1)
anova(base, fixed1)

fixed2 <- plm(vfrall~beertax+lincperc+vmilespd, data=pdata, index=c("state", "year"), model="within")
summary(fixed2)
plmtest(fixed2, type=c("bp"))

fixed3 <- plm(vfrall~beertax+lincperc+vmilespd, data=pdata, index=c("state", "year"), model="pooling")
summary(fixed3)
pFtest(fixed2, fixed3)

##State random effects regression
random <- plm(vfrall~beertax+lincperc+vmilespd, data=pdata, index=c("country", "year"), model="random")
summary(random)
phtest(fixed2, random)

##Feasible GLS
pcse(fixed1, groupN = data$state, groupT = data$year, pairwise=F)
pcse(base, groupN = data$state, groupT = data$year, pairwise=F)

fgls2<-pggls(vfrall~beertax+lincperc+vmilespd, data=pdata, 
             model="within", effect ="individual")
summary(fgls2)

##Examine all the data by state over time
library(lattice) #install.packages("lattice")
xyplot(vfrall~year|state, type="l",data=pdata) 