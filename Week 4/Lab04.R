library(here)

library(foreign) # For loading Stata data in
library(car) # For VIF

##Uncomment the following line if needed
# install.packages("stargazer")


library(stargazer)
cadta<-read.dta(here("data","caschool.dta"))
options(scipen = 999)
options(digits=3)
cor(cadta[c(15, 8, 9, 16, 12, 13, 14)])

pairs(cadta[c(15, 8, 9, 16)], pch=19, cex=.5)
pairs(cadta[c(12, 13, 14)])

base<-lm(testscr ~ avginc + el_pct, cadta); summary(base)
mean(base$residuals)
cor(cadta$el_pct, base$residuals); cor(cadta$avginc, base$residuals)
cor(cadta$calw_pct, base$residuals); cor(cadta$meal_pct, base$residuals)

baseplus1<-lm(testscr~avginc+el_pct+calw_pct, cadta); summary(baseplus1)

baseplus2<-lm(testscr~avginc+el_pct+calw_pct+meal_pct, cadta); summary(baseplus2)

cor(cadta[c(11, 15, 8, 9, 16)])

vif(baseplus2)

check <- lm(meal_pct~avginc+el_pct+calw_pct, cadta); summary(check)[8]

final <- lm(testscr ~ avginc + el_pct + meal_pct, cadta); summary(final)
vif(final)

sum(final$residuals^2)/df.residual(final)
sqrt(sum(final$residuals^2)/df.residual(final))

vce <- vcov(final)
vce
sqrt(vce[2,2])

summary(final$residuals)
sd(final$residuals)
hist(final$residuals) 

# Model Fit
final.res<-resid(final)
resf<-final.res^2
ssr<-sum(resf); ssr

summary(cadta$testscr); ybar <- mean(cadta$testscr)
devy <-(cadta$testscr-ybar)
devysq<-devy^2
sst<-sum(devysq); sst

# Model Specification in a Small Sample
mytable<-table(cadta$county,cadta$observat)
margin.table(mytable,1)
newdata <- subset(cadta, county=="Los Angeles" | county=="Orange" | county=="Ventura") 

smalln<-lm(testscr~avginc+el_pct+meal_pct, data=newdata); summary(smalln)
stargazer(final, smalln, type="text", title="", single.row=TRUE, omit.stat=c("f", "ser"))
vif(smalln)
cor(newdata[c(11, 15, 8, 9, 16)])
pairs(newdata[c(15, 8, 9, 16)], pch=19, cex=.5)

stargazer(final, smalln, title="", single.row=TRUE, omit.stat=c("f", "ser"))