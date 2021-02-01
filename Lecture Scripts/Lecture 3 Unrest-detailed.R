Unrest <- read.table("C:/Unrest.txt", header=TRUE, quote="\"")
attach(Unrest)
sum(Unemp)  # bottom of column 2
mean(Unemp) # bottom of column 2
sum(Riots)  # bottom of column 3
mean(Riots) # bottom of column 3

devx = Unemp-mean(Unemp) # entries in column 4
devxsq = devx^2          # entries in column 5
devy = Riots-mean(Riots) # entries in column 6
devysq = devy^2          # entries in column 7
codevxy = devx*devy      # entries in column 8
# varx = var(Unemp); varx
# vary = var(Riots); vary
# covxy = cov(Unrest[2:3], use = "pairwise.complete.obs"); covxy

myDF1 <- data.frame(Year, Unemp, Riots, devx, devxsq, devy, devysq, codevxy)
myDF1

SSTx = sum(devxsq)    # bottom of column 5
SSTy = sum(devysq)    # bottom of column 7
ncovxy = sum(codevxy) # bottom of column 8
b1 = ncovxy/SSTx
b0 = mean(Riots) - b1*mean(Unemp)

model <- lm(Riots~Unemp)
summary(model)$coefficients

yhat = model$fitted    # entries in table 2, column 2
sum(yhat)
uhat = model$residuals # entries in table 2, column 3
sum(uhat)
uhatsq = uhat^2        # entries in table 2, column 4
sum(uhatsq)
anova(model)
sqrt(sum(model$residuals^2)/(model$df.residual))
summary(model)$sigma
checkbias = model$residuals*(Unemp-mean(Unemp))
round(sum(checkbias), digits=4)

myDF2 <- data.frame(Year, Riots, yhat, uhat, uhatsq, checkbias)
round(myDF2, digits=2)