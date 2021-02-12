heightwage <- read.csv("H:/Teaching/POLS 6481 Spring 2021/Homework Assignments/weeks 03+04/HeightWage_MenWomenUS_HW.csv")
myvars <- c("male", "esteem80", "wage96", "height81", "height85", "clubnum", "athlets")
slimdata <- heightwage[myvars]
shortdata <- na.omit(slimdata)
newdata <- shortdata[ which(shortdata$male==1), ]
rm(heightwage); rm(slimdata); rm(shortdata)