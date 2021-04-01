#install.packages("Sleuth3", repos="http://R-Forge.R-project.org")
#require(Sleuth3)
#str(case2201)
#attach(case2201)
#elephants=data.frame(Age,Matings)
#write.csv(elephants, file = "H:/elephants.csv",row.names=FALSE)
#detach(case2201)

data=read.csv("C:/elephants.csv"); attach(data)

## EXPLORATION 
plot(Matings ~ Age, pch=19, cex=.75)
abline(lm(Matings ~ Age), col="blue")
plot(Matings+1 ~ Age, pch=19, cex=.75, log="y")

## POISSON MODEL
myGlm2 <- glm(Matings ~ Age, poisson)
summary(myGlm2)
confint(myGlm2)

# Coefficients
beta  <- myGlm2$coef
exp(beta[2])
exp(beta[2])-1
# Interpretation: each 1 year increase in age is associated with a 7% increase in the mean number of matings.
exp(confint(myGlm2,2))
exp(confint(myGlm2,2))-1
# Interpretation: 95% confidence interval is a 4% to 10% increase

## GRAPHICAL DISPLAY FOR PRESENTATION AND COMPARISON
plot(Matings ~ Age, ylab="Number of Successful Matings",
     xlab="Age of Male Elephant (Years)",
     main="Age and Number of Successful Matings for 41 African Elephants",
     pch=19, bg="green", cex=.75, lwd=2)
dummyAge <- seq(min(Age),max(Age), length=101)
lp <- beta[1] + beta[2]*dummyAge
curve <- exp(lp)
lines(curve ~ dummyAge,lwd=2) 

abline(lm(Matings~Age),lwd=1.25,col="blue")

## LINEAR REGRESSION MODEL FOR COMPARISON
mylm <- lm(Matings~Age); summary(mylm)$r.squared
print=data.frame(Age,Matings,pred=myGlm2$fitted)
fits=lm(Matings~pred, print); summary(fits)$r.squared

## PREDICTED MU's
hat=cbind(Age, Matings, myGlm2$linear.predictors, exp(myGlm2$linear.predictors))
colnames(hat)=c("Age","Matings","xbeta","mu(x)")
round(hat, digits=3)

## PREDICTED PROBABILITIES FOR 39 YEAR OLD ELEPHANT
myGlm2$linear.predictors[29]
exp(myGlm2$linear.predictors)[29]
myGlm2$fitted[29]

# density
dpois(c(0:5), 2.995)
round(100*dpois(c(0:5), 2.995), digits=1)

# cumulative proportion (default is lower tail)
ppois(c(0:5), 2.995, lower.tail = T)

# THERE ARE NO 40 YEAR OLD ELEPHANTS IN THE SAMPLE
# TO SIMULATE ONE USE THE FOLLOWING TWO LINES
xbeta40 = myGlm2$coef[1]+myGlm2$coef[2]*40
mu40 = exp(xbeta40)

dpois(c(0:5), mu40)
round(100*dpois(c(0:5), mu40), digits=1)

# easy ways to check over-/under- dispersion

myGlm2$deviance/myGlm2$df.residual # desire value close to 1

myGlm2$deviance                    # desire value less than ...
qchisq(p=.05, df=myGlm2$df.residual, lower.tail=F)

pchisq(myGlm2$deviance, myGlm2$df.residual, lower.tail=FALSE)

# check dispersion / create dispersion parameter for quasi-poisson model
E2 <- resid(myGlm2, type="pearson")
N <- nrow(data)
p <- length(coef(myGlm2))
sum(E2^2)/(N-p)                    # desire value close to 1

## QUASI-POISSON MODEL
myGlm3 <- glm(Matings ~ Age, quasipoisson)
summary(myGlm3)

# Robust Standard Errors
# install.packages("sandwich")
library(sandwich)
robvcm <- vcovHC(myGlm2, type="HC0")
robse <- sqrt(diag(robvcm))
r.est <- cbind(Estimate= coef(myGlm2), "Robust SE" = robse, "Pr(>|z|)" = 2*pnorm(abs(coef(myGlm2)/robse), lower.tail=FALSE),
               LL = coef(myGlm2) - 1.96*robse, UL = coef(myGlm2) + 1.96*robse) 
options(scipen=999); round(r.est, digits=5)

# Possibly Unnecessary Zero-Inflated Poisson
data$Matings
require(ggplot2); ggplot(data, aes(Matings)) + geom_histogram(binwidth=.5)
summary(data$Matings)
round(41*dpois(c(0:9), mean(data$Matings)), digits=1)
# install.packages("pscl")
library(pscl)
myZIP <- zeroinfl(Matings ~ Age, dist="poisson", data)
myZIP
summary(myZIP)[1]
vuong(myGlm2, myZIP)
