library(here)
library(ggplot2)
library(stargazer)

##Part 1
library(readr)
Uganda <- read_csv(here("data","Uganda_ITN.csv"))
View(Uganda)
names(Uganda)

#question 1

q1 <- glm(itn ~ education + age + altitude, data = Uganda, family=binomial(link="probit"))
summary(q1)
stargazer(q1)


#question 2
# library(sjPlot)
# plot_model(q1)

summary(Uganda$age)

newdata <- with(Uganda, data.frame(age = 15:49,
           education=mean(education, na.rm=TRUE), altitude=mean(altitude, na.rm=TRUE)))

preds <- predict(q1, newdata)

plot(preds)

#question 3

q3 <- update(q1, ~ . - education + rural + pregnant + infant)
summary(q3)
stargazer(q3)

#question 4

newdata2 <- with(Uganda, data.frame(age = 15:49,
                education=mean(education, na.rm=TRUE), altitude=mean(altitude, na.rm=TRUE),
                pregnant=mean(pregnant, na.rm=TRUE), infant=mean(infant, na.rm=TRUE), rural = 0))

newdata3 <- with(Uganda, data.frame(age = 15:49,
                  education=mean(education, na.rm=TRUE), altitude=mean(altitude, na.rm=TRUE),
                  pregnant=mean(pregnant, na.rm=TRUE), infant=mean(infant, na.rm=TRUE), rural = 1))

preds2 <- predict(q3, newdata2)

plot(preds2)

preds3 <- predict(q3, newdata3)
plot(preds2)
lines(preds3)

#question 5

Uganda$atrisk = 1 - (1-Uganda$pregnant)*(1-Uganda$infant)
summary(Uganda$atrisk)

q5 <- glm(itn ~ age + altitude + rural + atrisk, data = Uganda, family=binomial(link="probit"))
summary(q5)
stargazer(q5)

###Part 2

library(haven)
fert2 <- read_dta(here("data","FERTIL2.DTA"))
View(fert2)

#question 7

ruraldf <- fert2[ which(fert2$urban == 0),]
urbandf <- fert2[ which(fert2$urban == 1),]

meanchildren <- mean(fert2$children)
meanruralchildren <- mean(ruraldf$children)
meanurbanchildren <- mean(urbandf$children)

electricdf <- fert2[ which(fert2$electric == 1),]
noelectricdf <- fert2[ which(fert2$electric == 0),]

meanelectricchildren <- mean(electricdf$children)
meannoelectricchildren <- mean (noelectricdf$children)


#question 8 

q8 <- lm(children ~ educ + urban + electric + catholic + protest + spirit, data = fert2)
summary(q8)
stargazer(q8)

#question 9

summary(ruraldf$educ)

newruraldf <- with(ruraldf, data.frame(educ = 0:19, electric = 0, 
                 catholic = 0, protest = 0, spirit = 0, urban = 0))

summary(urbandf$educ)
newurbandf <- with(urbandf, data.frame(educ = 0:20, electric = 0, 
                catholic = 0, protest = 0, spirit = 0, urban = 1))

predsrural <- predict(q8,newruraldf)
predsurban <- predict(q8,newurbandf)

plot(predsurban)
lines(predsrural)

#10 
fert2 <- fert2[ which(is.na(electric) == FALSE), ]
q10 <- glm(children ~ educ + urban + electric + catholic + protest + 
               spirit, data = fert2, family = "poisson")
summary(q10)

print=data.frame(fert2,pred=q10$fitted)
print
fits=lm(children~pred, print); summary(fits)$r.squared
par(mfrow = c(2,2))
plot(fits)

pred=q10$fitted

# # Mean Effects
# pdf(file="meffect.pdf",height=6,width=6)
# ggplot(meffects,aes(x=x,y=meaneffects))+
#     geom_point(,color="red")+
#     theme_bw()+
#     labs(x="Tenure",y="Partial Change in E(y|x)")  
# dev.off()  

