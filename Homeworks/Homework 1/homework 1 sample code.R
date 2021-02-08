library(here)

suicidedata <- read.csv(here("data", "suicide.csv"))
View(suicidedata)

means <- mean(suicidedata$suicides); means
vars <- var(suicidedata$suicides); vars
sds <- sd(suicidedata$suicides); sds

meanu <- mean(suicidedata$unemployment); meanu
varu <- var(suicidedata$unemployment); varu
sdu <- sd(suicidedata$unemployment); sdu

covus <- cov(suicidedata$unemployment,suicidedata$suicides); covus

corruss <- covus/(sdu*sds); corruss




inequalitydata <- read.csv(here("data","inequality and representation.csv"))
View(inequalitydata)