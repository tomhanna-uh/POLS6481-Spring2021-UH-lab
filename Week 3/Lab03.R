install.packages("here")
library(here)
##example of format for use of here
## CEOdta<-read.dta(here("Week 2","CEOSAL1.DTA"))


library(foreign) ##For loading Stata data##
library(Hmisc) ##For labeling data##
library(car) ##For avPlots##
CAdta<-read.dta(here("data","caschool.dta"))

# Dropping Variables
myvars <- names(CAdta) %in% c("district", "str", "testscr", "avginc", "el_pct", "expn_stu") 
newdata <- CAdta[myvars]

# Variances and covariances
mean(newdata$testscr); var(newdata$testscr)
mean(newdata$expn_stu); var(newdata$expn_stu)
cov(newdata$expn_stu,newdata$testscr)
reg1<-lm(newdata$testscr~newdata$expn_stu)
summary(reg1)

mean(newdata$avginc); var(newdata$avginc)
cov(newdata$avginc,newdata$testscr)
reg2<-lm(newdata$testscr~newdata$avginc)
summary(reg2)

cor(newdata[2:6], use="complete.obs", method="pearson") 
VCM <- cov(newdata[,c(2,4:6)])
round(VCM, digits=2)
mean(newdata$el_pct)

# Baseline Model
base<-lm(newdata$testscr~newdata$avginc+newdata$el_pct)
summary(base)
avPlots(base)

summary(base$residuals)
round(cor(base$residuals, newdata$el_pct), digits=3)
round(cor(base$residuals, newdata$avginc), digits=3)
round(cor(base$residuals, newdata$str), digits=3)
round(cor(base$residuals, newdata$expn_stu), digits=3)

# Baseline Model Plus a Policy Variable
baseplus<-lm(newdata$testscr~newdata$avginc+newdata$el_pct+newdata$expn_stu)
summary(baseplus)
avPlots(baseplus)

# Baseline Model Minus a Demogaphic Variable
proxy<-lm(newdata$testscr~newdata$el_pct+newdata$expn_stu)
summary(proxy)
residuals2<-resid(proxy)
summary(residuals2)
round(cor(residuals2, newdata$el_pct), digits=3)
round(cor(residuals2, newdata$expn_stu), digits=3)
round(cor(residuals2, newdata$avginc), digits=3)

# Quick focus on standard errors
r = cor(newdata$testscr, newdata$avginc); r; r^2
summary(reg2)[8]

summary(reg2)$sigma

reg2.res<-resid(reg2)
res2<-reg2.res^2
ssr<-sum(res2); ssr
mse<-ssr/(length(res2)-2)
rmse<-sqrt(mse); rmse

summary(reg2)$coefficients[,1:3]

denominator = (length(newdata$avginc)-1)*var(newdata$avginc)
ratio = mse/denominator; ratio
se = sqrt(ratio); se

