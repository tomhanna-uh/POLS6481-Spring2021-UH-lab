library(here)
install.packages("readstata13"); library(readstata13)
library(car)
options(scipen=999)
tickets <- read.dta13(here("data","speeding_tickets_text.dta"))
complete <- subset(tickets, tickets$Amount!="NA")

basic <-lm(Amount ~ Age + MPHover, data=complete); summary(basic)
vector.b <- basic$coefficients; round(vector.b, digits=4)
matrix.b <- vcov(basic); round(matrix.b, digits=5)
sqrt(matrix.b[2,2])
t_Age = vector.b[2]/sqrt(matrix.b[2,2]); t_Age
t_MPHover = vector.b[3]/sqrt(matrix.b[3,3]); t_MPHover
qt(.975,basic$df.residual)
confint(basic)

dummies <-lm(Amount ~ MPHover + Black + Hispanic + Female + OutTown + OutState + StatePol, data=complete)
summary(dummies)
vector.d <- dummies$coefficients; round(vector.d, digits=4)
matrix.d <- vcov(dummies); round(matrix.d, digits=4)
num = (vector.d[7] - vector.d[6])
denom = sqrt(matrix.d[7,7] + matrix.d[6,6] - 2*matrix.d[7,6])
t_eq = num/denom; t_eq
confint(dummies)

##F Test of Gender and Racial Variables' Equality##
linearHypothesis(dummies, c("Black=Female"))

summary(dummies)[7]
dfr<-summary(dummies)$df[2]
summary(dummies)[10]
summary(dummies)[8:9]

anovatable = anova(dummies); anovatable[2]
ssr<-sum(dummies$residuals^2) # residual sum of squares
sst<-sum((complete$Amount-mean(complete$Amount))^2) # total sum of squares
sse<-sum((predict(dummies) - mean(complete$Amount))^2) # explained sum of squares
linearHypothesis(dummies, c("MPHover=0", "Black=0", "Hispanic=0", "Female=0", "OutTown=0", "OutState=0", "StatePol=0"))

