#here library allows relative paths to be portable
#from computer to computer using any R Studio 
#Project Directory as root
#install.packages("here")
library(here)
library(stargazer)

#read the data
#"data" is a subdirectory by that name in the 
#open R Studio Project directory 
suicidedata <- read.csv(here("data", "suicide.csv"))
View(suicidedata)

#create objects with means, variance, standard deviation
#of dependent variable (suicides) and explanatory variable
#unemployment
means <- mean(suicidedata$suicides); means
vars <- var(suicidedata$suicides); vars
sds <- sd(suicidedata$suicides); sds


meanu <- mean(suicidedata$unemployment); meanu
varu <- var(suicidedata$unemployment); varu
sdu <- sd(suicidedata$unemployment); sdu

#create a vector with unemployment observed values to use later
obsunemployment <- suicidedata$unemployment

#create an object with covariance of x and y
covus <- cov(suicidedata$unemployment,suicidedata$suicides); covus

#create an object with correlation of x and y
corruss <- covus/(sdu*sds); corruss

#find beta1/slope from covariance and variance
beta1 <- covus/varu;beta1

#find alpha/beta0/intercept
beta0 <- means - meanu*beta1;beta0

#verify slope and intercept with linear model
model1 <- lm(suicidedata$suicides ~ suicidedata$unemployment);model1

#verify that the regression line passes through the point
#containing the mean of x and y (s and u)
cc <- beta0 + beta1*5.745455; cc; means; meanu

#generate fitted values
fit <- fitted(model1); fit

#generate residuals
res <- residuals(model1);res

#product of residuals and observed values
ux <- res*obsunemployment; ux

#verify sum of residuals and sum of res*x = 0
sum(res)
sum(ux)

#load inequality data
inequalitydata <- read.csv(here("data","inequality and representation.csv"))
View(inequalitydata)
names(inequalitydata)

#create regression model of representation on inequality
#show summary of model
ineq.model1 <- lm(Representation ~ Inequality, 
                  data = inequalitydata); summary(ineq.model1)

#drop NAs
inequalitydata <- inequalitydata[complete.cases(inequalitydata[ , 1:3]),]

#calculate means, variances and assign to objects
meanr <- mean(inequalitydata$Representation); meanr
meani <- mean(inequalitydata$Inequality); meani
varr <- var(inequalitydata$Representation); varr
vari <- var(inequalitydata$Inequality); vari
covir <- cov(inequalitydata$Inequality,inequalitydata$Representation)
covir

#Computer SSX
dx2 <- (inequalitydata$Representation - meanr)^2; dr2
SSX <- sum(dx2); SSX

#Comput SSY
dy2 <- (inequalitydata$Inequality - meani)^2; dy2
SSY <- sum(dy2); SSY

#Compute beta1/slope
b1 <- covir/vari; b1

#Compute beta0/alpha/intercept
b0 <- meanr - b1*meani; b0

#scatterplot with regression line
#added text labels using row names to
#make finding outlier easier
with(inequalitydata,plot(Representation ~ Inequality))
abline(ineq.model1)
text(Representation ~ Inequality, labels=rownames(inequalitydata), data = inequalitydata, cex=0.9, font=2)

#hat values
ineq.hv <- hatvalues(ineq.model1); ineq.hv

#studentized residuals
library(MASS)
ineq.sr <- stdres(ineq.model1); ineq.sr

#DFFITS
ineq.dffits <- dffits(ineq.model1); ineq.dffits

#dfbeta
ineq.dfbeta <- dfbeta(ineq.model1); ineq.dfbeta

#Create dummy
inequalitydata$dummy<-ifelse(inequalitydata$Country=="Japan",c(1),c(0)) 
ineq.model2 <- lm(Representation ~ Inequality + dummy, 
                  data = inequalitydata); summary(ineq.model2)


#Cook's distance using chart

#fitted values for model 1
inequalitydata$fitted1 <- fitted(ineq.model1); inequalitydata$fitted1

#fitted values for model 2 - chart column 1
inequalitydata$chart1 <- fitted(ineq.model2);inequalitydata$chart1

#chart column 2
inequalitydata$chart2 <- inequalitydata$chart1 - inequalitydata$fitted1; inequalitydata$chart2

#chart column 3 (squared differences)
inequalitydata$chart3 <- inequalitydata$chart2^2; inequalitydata$chart3

#sum of column 3 (sum of squared differences)
sumdiffs <- sum(inequalitydata$chart3); sumdiffs
                
#sum/n
sumdiffs/20
                
#Sum minus Japan
Japan <- inequalitydata[1,8]; Japan
sumlessJapan <- sumdiffs - Japan; sumlessJapan

sig <- summary(ineq.model1)$sigma
sig2 <- sig^2

cooksd1 <- sumlessJapan/(3*sig2)
cooksd1

#Cooks distance

cooksd2 <- cooks.distance(ineq.model1); cooksd2
