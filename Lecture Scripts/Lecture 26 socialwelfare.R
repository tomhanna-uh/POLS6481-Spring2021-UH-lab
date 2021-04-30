library(foreign)
socwelf <- read.dta("C:/socialwelfare.dta")

table3 <- lm(socwel ~ female*Republican, socwelf)
summary(table3)

library(car)
linearHypothesis(table3, c("female=0"))
sqrt(linearHypothesis(table3, c("female=0"))[5])
linearHypothesis(table3, c("female + female:Republican=0"))
sqrt(linearHypothesis(table3, c("female + female:Republican=0"))[5])

socwelf$Democrat = 1-Republican
rebase.p <- lm(socwel ~ female*Democrat, socwelf)
summary(rebase.p)
linearHypothesis(rebase.p, c("female=0"))
sqrt(linearHypothesis(rebase.p, c("female=0"))[5])

linearHypothesis(table3, c("Republican=0"))
sqrt(linearHypothesis(table3, c("Republican=0"))[5])

socwelf$male = 1-female
rebase.g <- lm(socwel ~ male*Republican, socwelf)
summary(rebase.g)
linearHypothesis(rebase.g, c("Republican=0"))
sqrt(linearHypothesis(rebase.g, c("Republican=0"))[5])

table26col1 = table3
table26col2 = lm(socwel ~ Republican, socwelf[ which(socwelf$female==0), ])
summary(table26col2)
table26col3 = lm(socwel ~ Republican, socwelf[ which(socwelf$female==1), ])
summary(table26col3)
