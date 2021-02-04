# Unrest <- read.table("c:\Unrest.txt", header=TRUE, quote="\""); Unrest

library(here)
Unrest <- read.table(here("Lecture Data","Unrest.txt")); Unrest

attach(Unrest)

plot(Unemp, Riots, pch = 16, ylim = c(0,40))
pos.lab = c(1,3,1,1,3,3,3,3,1,1)
text(Unemp, Riots, labels=Year, pos=pos.lab, cex=.6, xpd=TRUE)
abline(h = mean(Riots))
abline(v = mean(Unemp))
abline(lm(Riots~Unemp, data=Unrest), col="red")
points(Unemp, fitted.values(lm(Riots~Unemp)), pch = 16, col="red")

model <- lm(Riots~Unemp, data=Unrest)
summary(model)$coef
summary(model)$sigma

round(model$fitted, digits=1)
round(model$residuals, digits=1)
round(sum(model$residuals), digits=4)
round(cor(model$residuals,Unrest$Unemp), digits=4)