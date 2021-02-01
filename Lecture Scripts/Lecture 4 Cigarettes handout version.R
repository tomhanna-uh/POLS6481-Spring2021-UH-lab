library(foreign)
lung <- read.dta("C:/TufteTable3-3.dta")

col1 = lung$cigarettes - mean(lung$cigarettes)
col1
col2 = col1^2
col2
col3 = lung$deaths - mean(lung$deaths)
round(col3, digits=1)
col4 = col3^2
round(col4, digits=1)
col5 = col1*col3
round(col5, digits=1)
col6 = (1/11)+col2/sum(col2)
round(col6, digits=4)
col7 = sqrt(1-col6)
round(col7, digits=3)
col8 = sqrt(col6/(1-col6))
round(col8, digits=3)

Model11 <- lm(deaths ~ cigarettes, data=lung)
summary(Model11)
round(Model11$fitted.values, digits=1)
round(Model11$residuals, digits=1)
round(Model11$residuals^2, digits=1)

# t = Model11$residuals/(col7*summary(Model11)$sigma) # internally studentized

lung$dummy <- ifelse(lung$Country=="United States",c(1),c(0))
Model11D <- lm(deaths ~ cigarettes + dummy, data=lung)
summary(Model11D)

t = Model11$residuals/(col7*summary(Model11D)$sigma) # externally studentized
round(t, digits=2)
dff = t*col8
round(dff, digits=3)

# to generate Cook's distance *just for USA*
round(Model11D$fitted.values, digits=1)
round(Model11$fitted.values - Model11D$fitted.values, digits=1)
numr = Model11$fitted.values - Model11D$fitted.values
numerator = sum(numr^2)
denominator = 3*((sum(Model11$residuals^2))/Model11$df.residual)
d = numerator/denominator; d

remove = -c(11)
Model10<- lm (deaths ~ cigarettes, data=lung, subset=remove)
summary(Model10)


influence <- lm.influence(Model11)
leverage <- influence$hat; round(leverage, digits=4)[11]
discrep <- rstudent(Model11); round(discrep, digits=3)[11]
cooks <- cooks.distance(Model11); round(cooks, digits=4)[11]
dffits <- dffits(Model11); round(dffits, digits=3)[11]

dfbeta <- dfbeta(Model11); round(dfbeta, digits=4)[11,]
Model11$coefficients; Model10$coefficients; Model11D$coefficients
round(coef(Model11)[1]-coef(Model10)[1], digits=4)
round(coef(Model11)[2]-coef(Model10)[2], digits=4)
# signs reveal: dfbeta indicates how coefficients changed with case included

mydiagnostics <- data.frame(lung$Country, leverage, discrep, cooks, dffits, dfbeta)
options("scipen"=100, "digits"=3)
View(mydiagnostics)

plot(leverage,discrep); abline(h=2, lty=2); abline(h=-2, lty=2); abline(v=4/11, lty=3)
install.packages("car")
library(car)
influencePlot (Model11, main="Influence Plot")

plot(lung$cigarettes, Model11$residuals, pch = 18)
text(lung$cigarettes, Model11$residuals, labels=lung$Country, pos=3, cex=.6, xpd=TRUE)
abline(lm(Model11$residuals ~ lung$cigarettes), col="blue")
abline(lm(Model11$residuals ~ lung$cigarettes, subset=remove), col="red")