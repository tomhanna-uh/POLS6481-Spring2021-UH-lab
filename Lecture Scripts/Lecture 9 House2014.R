rm(list=ls())
options(scipen=999)
helection <- read.csv("C:/USHouseElections2014.csv")

cor(helection[,c(5,11:13)], use="complete.obs", method="pearson") 
vcm <- cov(helection[,c(5,11:13)], use="complete.obs", method="pearson") 
round(vcm,digits=2)
occupied <- subset(helection, helection$inc<2)
occupied$ratio = log(occupied$dexp/(occupied$dexp+occupied$rexp))
complete <- subset(occupied, occupied$dv!="NA" & occupied$ratio!="NA" & occupied$ratio!="-Inf")
rm(occupied); rm(helection)

round(cor(complete[,c(5,14,13)], use="complete.obs", method="pearson"),digits=3)
simpleA <- lm(dv ~ ratio, data=complete) # regression of Dem vote on log spending ratio
summary(simpleA)
simpleB <- lm(dv ~ dpres, data=complete) # regression of Dem vote on normal vote
summary(simpleB)

model <- lm(dv ~ ratio + dpres, data=complete)  # regression of Dem vote on log spending ratio and normal vote
summary(model)

library(car); vif(model)

# Partialling analysis
partial <- lm(ratio ~ dpres, data=complete)
summary(partial)
complete$rhat = partial$residuals
round(cor(complete[, c(13:15)], use="complete.obs", method="pearson"), digits=3)
remodel <- lm(dv ~ rhat + dpres, data=complete)
summary(remodel)
vif(remodel)

#install.packages("stargazer")
library(stargazer)
stargazer(simpleA, model, remodel, simpleB, type="text", title="", single.row=TRUE, omit.stat=c("f", "adj.rsq"))

# Mediation analysis
# A. Successful
install.packages("knitr"); install.packages("mediation")
library(knitr); library(mediation)
?mediate
summary(mediate(partial, model, treat='dpres', mediator='ratio', boot=FALSE))

results <- mediate(partial, model, treat='dpres', mediator='ratio', boot=TRUE, sims=500)
summary(results)

#B. Long way; a*b (numerator) should equal C-C' (CminusCprime)
# z = a*b/sqrt(b^2*var(sa) + a^2*var(sb) + var(sa)*var(sb))
CminusCprime = simpleB$coef[2] - model$coef[3]
numerator = partial$coef[2]*model$coef[2]
sqdenomL = (model$coef[2]^2)*(vcov(partial)[2,2])
sqdenomR = (partial$coef[2]^2)*(vcov(model)[2,2])
sqdenomM = (vcov(partial)[2,2])*(vcov(model)[2,2])
denominator = sqrt(sqdenomL + sqdenomM + sqdenomR)
z = numerator/denominator; z

#C. Unsuccessful
install.packages("bstats"); library(bstats)
attach(complete)
detach(complete)
mv = complete$ratio
iv = complete$dpres
dv = complete$dv
mediation.test(ratio, dpres, dvp)
