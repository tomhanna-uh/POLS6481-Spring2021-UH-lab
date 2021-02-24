heightwage <- read.csv("H:/Teaching/POLS 6481 Spring 2020/Homework Assignments/HeightWage_MenWomenUS_HW.csv")
# to create new dataset without *any* missing data
# newdata <- na.omit(heightwage)
# unfortunately that command deletes too many cases, so intead, make the dataset smaller first
myvars <- c("male", "esteem80", "wage96", "height81", "height85", "clubnum", "athlets")
slimdata <- heightwage[myvars]
shortdata <- na.omit(slimdata)
newdata <- shortdata[ which(shortdata$male==1), ]
rm(heightwage); rm(slimdata); rm(shortdata)

summary(lm(wage96 ~ height85, newdata))

summary(lm(wage96 ~ height85 + height81, newdata))

pairs(newdata[,c(3:5)], pch=19)

VCM <- cov(newdata[,c(2:7)])
round(VCM, digits=3)
round(cor(VCM), digits=3)

partial <- lm(height85 ~ height81, newdata); summary(partial)
cor(newdata$height81,newdata$height85)

q3a <- lm(wage96 ~ height85 + height81 + clubnum + athlets, newdata)
summary(q3a)
newdata$growth = newdata$height85 - newdata$height81
summary(lm(wage96 ~ growth + height81 + clubnum + athlets, newdata))
# newdata$rhat <- partial$resid
# summary(lm(wage96 ~ rhat + height81 + clubnum + athlets, newdata))

q3b <- lm(wage96 ~ height81 + clubnum + athlets, newdata)
summary(q3b)

cor(newdata$clubnum,newdata$athlets)

library(car)
vif(q3a)
vif(q3b)

q4a <- lm(esteem80 ~ height81, newdata)
summary(q4a)
q4b <- lm(esteem80 ~ height81 + clubnum + athlets, newdata)
summary(q4b)

vif(q4b)

# extra: suppose you wanted standardized coefficients (betas)
# install.packages("QuantPsyc")
library(QuantPsyc)
lm.beta(lm(wage96 ~ height81 + clubnum + athlets, newdata))
lm.beta(lm(esteem80 ~ height81 + clubnum + athlets, newdata))