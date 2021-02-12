rm(list=ls())

library(foreign)
Mortality<-read.dta("C:/Mortality.dta")
attach(Mortality)

# Plot with linear trend
pchval <- rep(21, length = length(northsouth))
pchval[northsouth == 1] <- 19
plot(calcium, mortal, pch = pchval); abline(lm(mortal~calcium), col="black")

# Correlation and Var-Covar Matrices
cor(Mortality, use="complete.obs", method="pearson") 
vcm <- cov(Mortality, use="complete.obs", method="pearson") 
round(vcm[1:3,1:3], digits=2)
b2tilde = vcm[4,1]/vcm[4,4] # simple regression coefficient for south
b1tilde = vcm[2,1]/vcm[2,2] # simple regression coefficient for calcium

# Simple regression of mortality on water hardness
reg1 <- lm(mortal~ calcium); summary(reg1)

# Examine residuals from Simple Regression
mean(reg1$residuals)
stem(reg1$residuals)
plot(Mortality$calcium, reg1$residuals, ylab="Residuals", xlab="Calcium", pch=pchval); abline(h=0, col="red")

cor(reg1$residuals,south)
residmodel <- lm(reg1$residuals ~ Mortality$south)
rn = residmodel$coefficients[1]
rs = residmodel$coefficients[1]+residmodel$coefficients[2]
plot(calcium, reg1$residuals, pch = pchval); abline(h=rn, col="black"); abline(h=rs, col="gray60")

# Plot data with group means for x
South <- subset(Mortality, south==1, select = c(mortal, calcium))
North <- subset(Mortality, south==0, select = c(mortal, calcium))
plot(calcium, mortal, pch = pchval); abline(v = mean(South$calcium)); abline(v = mean(North$calcium))

# Add group means for y
regd <- lm(mortal~south)
mun = regd$coefficients[1]
mus = regd$coefficients[1]+regd$coefficients[2]
abline(h=mun, col="black"); abline(h=mus, col="gray60")

# Simple regression of mortality on region (south = 1)
summary(regd)

# Multiple regression
reg2 <- lm(mortal ~ calcium + south)
reg2$coefficients
summary(reg2)[8]

#plot(calcium, mortal, pch = pchval)
abline(a=reg2$coefficients[1], b=reg2$coefficients[2], col="black")
abline(a=reg2$coefficients[1]+reg2$coefficients[3], b=reg2$coefficients[2], col="grey60")

# Examine residuals from Multiple Regression
summary(reg2$residuals)
stem(reg2$residuals)
plot(Mortality$calcium, reg2$residuals, ylab="Residuals", xlab="South", pch=pchval) 
abline(h=0, col="red")
