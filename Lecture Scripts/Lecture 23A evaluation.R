# install.packages("car")
library(car)
dataset <- read.csv("C:/eval.csv")

prof_eval <- dataset$prof_eval
female <- ifelse(dataset$gender=="female", c(1), c(0))
attract <- dataset$bty_avg

single <- lm(prof_eval ~ attract)
summary(single)

tenured <- ifelse(dataset$rank=="tenured", c(1), c(0))
tenuretrack <- ifelse(dataset$rank=="tenure track", c(1), c(0))

triple <- lm(prof_eval ~ tenured + tenuretrack + attract)
summary(triple)
linearHypothesis(triple, c("tenured = tenuretrack"))
linearHypothesis(triple, c("tenured=0", "tenuretrack=0"))
anova(single,triple)

numerator = triple$coef[2]-triple$coef[3]
vce = vcov(triple)
round(vce[2:4,2:4], digits=5)
varb1 = vce[2,2]
varb2 = vce[3,3]
covb1b2 = vce[2,3]
denominator = sqrt(varb1 + varb2 - 2*covb1b2)
tstat = numerator/denominator
tstat; tstat^2

ignorant = sqrt(summary(triple)$coef[2,2]^2 + summary(triple)$coef[3,2]^2)
ignorant; denominator