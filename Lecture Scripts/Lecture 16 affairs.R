install.packages("AER")
data( "Affairs", package = "AER" )
write.csv(Affairs, file = "H:/Affairs.csv",row.names=FALSE)
rm(Affairs)

Affairs <- read.csv("C:/Affairs.csv")
attach(Affairs)
#hist(affairs)
require(ggplot2); ggplot(Affairs, aes(affairs)) + geom_histogram(bins=25)

plot(jitter(Affairs$rating,factor=(Affairs$rating/2)), jitter(Affairs$affairs, factor=1), pch=19, cex=.75)

reg.B <- lm(affairs ~ age + yearsmarried + religiousness + occupation + rating)
pois.B <- glm(affairs ~ age + yearsmarried + religiousness + occupation + rating, family=poisson)
library(stargazer); stargazer(reg.B, pois.B, type="text", title="", single.row=FALSE, omit.stat=c("f", "ser"))

beta  <- pois.B$coef
exp(beta[2])
exp(beta[2])-1
exp(confint(pois.B,2))-1

exp(beta[6])
exp(beta[6])-1
exp(confint(pois.B,6))-1

# overdispersion or underdispersion?
pois.B$deviance/pois.B$df.residual
library(AER); dispersiontest(pois.B)

# calculate dispesion parameter, then run quasi-poisson model and compare to poisson model
E2 <- resid(pois.B, type = "pearson"); N <- nrow(Affairs); p <- length(coef(pois.B))
sum(E2^2)/(N-p)
qpois.B <- glm(affairs ~ age + yearsmarried + religiousness + occupation + rating, family=quasipoisson)
summary(qpois.B)

stargazer(pois.B, qpois.B, type="text", title="", single.row=FALSE, omit.stat=c("f", "ser"))

# absolutely necessary zero-inflated model
install.packages("pscl")
library(pscl)
zip.B <- zeroinfl(affairs ~ age + yearsmarried + religiousness + occupation + rating | age + yearsmarried + religiousness + occupation + rating, family="poisson")
summary(zip.B)
vuong(pois.B, zip.B)

Affairs$dummydv <- ifelse(Affairs$affairs>0, c(1), c(0))
logit <- glm(dummydv ~ age + yearsmarried + religiousness + occupation + rating, Affairs, family=binomial(link="logit"))
summary(logit)