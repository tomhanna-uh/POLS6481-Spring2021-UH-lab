library(here)
library(foreign); library(car)

crab <- read.table(here("data","crab.txt"))
colnames(crab)=c("Obs","C","S","W","Wt","Sa")
crab=crab[,-1] #removes "Obs" column

attach(crab)
plot(W, Sa, pch=19)
abline(lm(Sa ~ W))

lmodel=lm(Sa ~ W); summary(lmodel)
# lmodel=glm(Sa ~ 1+ W, family=gaussian); summary(lmodel)

pmodel=glm(Sa ~ 1+ W, family=poisson(link=log))
summary(pmodel)

print=data.frame(crab,pred=pmodel$fitted)
print
fits=lm(Sa~pred, print); summary(fits)$r.squared

pmodel$linear.predictors
exp(pmodel$linear.predictors)

nmodel=glm(crab$Sa~1, family=poisson(link=log))
summary(nmodel)
nmodel$fitted

pmodel$linear.predictors[crab$W==25]
pmodel$linear.predictors[crab$W==26]
pmodel$fitted[crab$W==25]
pmodel$fitted[crab$W==26]
exp(-pmodel$fitted[crab$W==25])
exp(-pmodel$fitted[crab$W==26])
exp(-pmodel$fitted[crab$W==25])*pmodel$fitted[crab$W==25]
exp(-pmodel$fitted[crab$W==26])*pmodel$fitted[crab$W==26]

pmodel$fitted[crab$W==26]/pmodel$fitted[crab$W==25]
pmodel$fitted[2]/pmodel$fitted[6]

confint(pmodel,2)
exp(confint(pmodel,2))-1
# install.packages("sandwich")
library(sandwich)
robvar<-vcovHC(pmodel, type="HC0")
robse <- sqrt(diag(robvar))
r.est <- cbind(Estimate= coef(pmodel), "Robust SE" = robse, "Pr(>|z|)" = 2*pnorm(abs(coef(pmodel)/robse), lower.tail=FALSE),
               LL = coef(pmodel) - 1.96*robse, UL = coef(pmodel) + 1.96*robse)
options(scipen=999); round(r.est, digits=5)

library(car)
robvar<-hccm(pmodel)

library(stargazer)
stargazer(r.est)
