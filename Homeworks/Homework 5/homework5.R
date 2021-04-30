library(here)

# Part 1

library(haven)
sleepdata <- read_dta(here("data","SLEEP75.DTA"))
View(sleepdata)
names(sleepdata)

#question 1
sleepdata$age2 <- (sleepdata$age)^2
q1 <- lm(sleep ~ totwrk + age + age2, data = sleepdata)
summary(q1)


#question 2
#brute force method

p25 <- -0.1466*2112 - 8.1020*25 + 0.1328*25 + 3674.9502
#etc...

#question 3


#question 3.5

#question 4




# Part 2
library(haven)
discrimdata <- read_dta(here("data","discrim.dta"))
View(discrimdata)