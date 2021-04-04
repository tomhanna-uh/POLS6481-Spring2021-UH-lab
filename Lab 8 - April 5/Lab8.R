library(here)
library(stargazer)
library(car)
library(foreign)

data <- read.dta(here("data","conf06.dta"))
conf06 <- subset(data, data$nominee!="ALITO")
vars <- c("vote", "nominee", "sameprty", "qual", "lackqual", "EuclDist2", "strngprs")
conf <- conf06[vars]
conf$numvote<-as.numeric(conf$vote)-1
conf$numstrngprs<-as.numeric(conf$strngprs)-1
stargazer(conf, type="text", digits=3)
stargazer(as.data.frame(conf), type="text", digits=3)

# analyze tables of proportions in different conditions
weak <- table(conf$vote[conf$strngprs=="weak"], conf$sameprty[conf$strngprs=="weak"])
round(prop.table(weak,2), digits=3)
strong<-table(conf$vote[conf$strngprs=="strong"], conf$sameprty[conf$strngprs=="strong"])
round(prop.table(strong,2), digits=3)

same<-table(conf$vote[conf$sameprty==1], conf$strngprs[conf$sameprty==1])
round(prop.table(same, 2), digits=3)
notsame<-table(conf$vote[conf$sameprty==0], conf$strngprs[conf$sameprty==0])
round(prop.table(notsame, 2), digits=3)

# probit
myprobit<- glm(numvote~lackqual+EuclDist2+numstrngprs+sameprty, family = binomial(link = "probit"), data = conf)
summary(myprobit)

meanqual<-mean(conf$lackqual)
minqual<-min(conf$lackqual)
maxqual<-max(conf$lackqual)
meandist<-mean(conf$EuclDist2)
mindist<-min(conf$EuclDist2)
maxdist<-max(conf$EuclDist2)

maxprobit = myprobit$coef[1] + myprobit$coef[2]*maxqual +  myprobit$coef[3]*meandist + myprobit$coef[4]*0 + myprobit$coef[5]*0
maxprobit; pnorm(maxprobit)
meanprobit = myprobit$coef[1] + myprobit$coef[2]*meanqual + myprobit$coef[3]*meandist + myprobit$coef[4]*0 + myprobit$coef[5]*0
meanprobit; pnorm(meanprobit)
minprobit = myprobit$coef[1] + myprobit$coef[2]*minqual +  myprobit$coef[3]*meandist + myprobit$coef[4]*0 + myprobit$coef[5]*0
minprobit; pnorm(minprobit)

maxqualsub<-data.frame(lackqual=maxqual, EuclDist2=meandist, numstrngprs=0, sameprty=0)
predict(myprobit, maxqualsub, type="response", se.fit=TRUE)
meanqualsub<-data.frame(lackqual=meanqual, EuclDist2=meandist, numstrngprs=0, sameprty=0)
predict(myprobit, meanqualsub, type="response", se.fit=TRUE)
minqualsub<-data.frame(lackqual=minqual, EuclDist2=meandist, numstrngprs=0, sameprty=0)
predict(myprobit, minqualsub, type="response", se.fit=TRUE)

weakopp<-data.frame(lackqual=meanqual, EuclDist2=meandist, numstrngprs=0, sameprty=0)
predict(myprobit, weakopp, type="response")
weaksame<-data.frame(lackqual=meanqual, EuclDist2=meandist, numstrngprs=0, sameprty=1)
predict(myprobit, weaksame, type="response")
strongopp<-data.frame(lackqual=meanqual, EuclDist2=meandist, numstrngprs=1, sameprty=0)
predict(myprobit, strongopp, type="response")
strongsame<-data.frame(lackqual=meanqual, EuclDist2=meandist, numstrngprs=1, sameprty=1)
predict(myprobit, strongsame, type="response")

# logit
mylogit<- glm(numvote~lackqual+EuclDist2+numstrngprs+sameprty, family = binomial(link = "logit"), data = conf)
summary(mylogit)

weakdiff<-mylogit$coef[1] + mylogit$coef[2]*meanqual +  mylogit$coef[3]*meandist + mylogit$coef[4]*0 + mylogit$coef[5]*0
weakdiff; (1+exp(-weakdiff))^-1
strongdiff<-mylogit$coef[1] + mylogit$coef[2]*meanqual +  mylogit$coef[3]*meandist + mylogit$coef[4]*1 + mylogit$coef[5]*0
strongdiff; (1+exp(-strongdiff))^-1
weaksame<-mylogit$coef[1] + mylogit$coef[2]*meanqual +  mylogit$coef[3]*meandist + mylogit$coef[4]*0 + mylogit$coef[5]*1
weaksame; (1+exp(-weaksame))^-1
strongsame<-mylogit$coef[1] + mylogit$coef[2]*meanqual +  mylogit$coef[3]*meandist + mylogit$coef[4]*1 + mylogit$coef[5]*1
strongsame; (1+exp(-strongsame))^-1

# linear probability model
lpm<-lm(numvote~lackqual+EuclDist2+numstrngprs+sameprty, data = conf)
summary(lpm)

lpm$coef[1] + lpm$coef[2]*meanqual +  lpm$coef[3]*meandist + lpm$coef[4]*0 + lpm$coef[5]*0
lpm$coef[1] + lpm$coef[2]*meanqual +  lpm$coef[3]*meandist + lpm$coef[4]*1 + lpm$coef[5]*0
lpm$coef[1] + lpm$coef[2]*meanqual +  lpm$coef[3]*meandist + lpm$coef[4]*0 + lpm$coef[5]*1
lpm$coef[1] + lpm$coef[2]*meanqual +  lpm$coef[3]*meandist + lpm$coef[4]*1 + lpm$coef[5]*1

summary(lpm$fitted)
length(lpm$fitted[lpm$fitted>1])
white.test(lpm)
plot(conf$EuclDist2,lpm$residuals); abline(h=0, col="red")
plot(conf$lackqual,lpm$residuals); abline(h=0, col="red")

##Correct classification statistics
fitted<-lpm$fitted
clas.lpm <-recode(fitted, ".5:1.5=1; 0:.49999=0")
correct.lpm <-table(clas.lpm,conf$vote); correct.lpm

phat.l <- predict(mylogit, type="response"); clas.l <- ifelse(phat.l >= .5, c(1),c(0))
correct.l <- table(clas.l, conf$vote); correct.l

phat.p <- predict(myprobit, type="response"); clas.p <- ifelse(phat.p >= .5, c(1),c(0))
correct.p <- table(clas.p, conf$vote); correct.p

logit.base <- glm(numvote ~ 1, family = binomial(link = "logit"), data = conf); summary(logit.base)

phat.l.base <- predict(logit.base, type="response")
clas.l.base <- ifelse(phat.l.base >= .5, c(1),c(0))
table(clas.l.base, conf$vote)

probit.base <- glm(numvote ~ 1, family = binomial(link = "probit"), data = conf); summary(probit.base)
phat.p.base <- predict(probit.base, type="response"); clas.p.base <- ifelse(phat.p.base >= .5, c(1),c(0))
table(clas.p.base, conf$vote)

stargazer(lpm, mylogit, myprobit, type="text")

rm(list=ls()) 