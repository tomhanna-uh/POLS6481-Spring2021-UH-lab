library(foreign)
library(car)
library(compactr)

candidates <- read.dta("C:/candidates.dta")
candta <- candidates[-c(8, 26, 53), ]

candta$intxn<-candta$groups*candta$runoff
mod<-lm(candidates~groups+runoff+intxn, candta); summary(mod)
linearHypothesis(mod, c("runoff", "intxn"), c(0,0)) ## result should equal Chow.p
linearHypothesis(mod, c("intxn"), c(0)) ## result should equal Chow.pp

mod$coefficients[2]+mod$coefficients[4]*0
mod$coefficients[2]+mod$coefficients[4]*1

round(vcov(mod)[2:4,2:4], digits=4)
varbg<-vcov(mod)[2,2]
varbr<-vcov(mod)[3,3]
varbi<-vcov(mod)[4,4]
covbgbi<-vcov(mod)[2,4]
covbrbi<-vcov(mod)[3,4]
dummy <- c(0,1)
dydg <- mod$coefficients[2]+mod$coefficients[4]*dummy
sedydg <- sqrt(varbg+(dummy^2)*varbi + 2*dummy*covbgbi)
t.dydg <- dydg/sedydg
p.negalt <- pt(t.dydg, df=mod$df.residual, lower=FALSE)
p.posalt <- pt(t.dydg, df=mod$df.residual, lower=TRUE)
p.2tail = pt(-abs(t.dydg), df=mod$df.residual)
ci90.l <- dydg - (qt(.95,mod$df.residual)*sedydg)
ci90.u <- dydg + (qt(.95,mod$df.residual)*sedydg)

dd<-data.frame(cbind(dummy,dydg,sedydg,t.dydg,p.negalt,p.posalt))
colnames(dd) <- c("runoff", "dydg", "sedydg", "t", "p neg", "p pos")
print(dd) # replicate Table 14 (without last two columns

groups2 <- seq(min(1), max(candta$groups), length.out = 1000)
dydr <- mod$coefficients[3]+mod$coefficients[4]*groups2
sedydr <- sqrt(varbr + (groups2^2)*varbi + 2*groups2*covbrbi)
t.star <-qt(.95,mod$df.residual)
upr <- dydr + t.star*sedydr
lwr <- dydr - t.star*sedydr
plot(groups2, dydr, pch="_", xlim = c(1,3), ylim = c(-2, 6))
lines(groups2, lwr, lty = 3)
lines(groups2, upr, lty = 3)
abline(h=0, col="red")

candta$dydr<-mod$coefficients[3]+mod$coefficients[4]*candta$groups
candta$sedydr<-sqrt(varbr + (candta$groups^2)*varbi + 2*candta$groups*covbrbi)

newtab <- data.frame(groups=c(1,1.5,2,2.5,3,1,1.5,2,2.5,3), runoff=c(0,0,0,0,0,1,1,1,1,1), intxn=c(0,0,0,0,0,1,1.5,2,2.5,3))
#predict(mod, newdata=newtab, interval='prediction')
#predict(mod, newdata=newtab, se.fit = T)
predict(mod, newdata=newtab[1:5,], interval='none')
predict(mod, newdata=newtab[6:10,], interval='none')

yhat = mod$coef[1] + newtab$groups*mod$coef[2] + newtab$runoff*mod$coef[3] + newtab$intxn*mod$coef[4]
dydrunoff = mod$coef[3] + newtab$groups*mod$coef[4]
vce.mod = vcov(mod); vce.mod[2:4,2:4]
sedydrunoff = sqrt(vce.mod[3,3] + (newtab$groups)^2*vce.mod[4,4] + 2*newtab$groups*vce.mod[4,3])
t.runoff = dydrunoff/sedydrunoff
table15 = cbind(yhat,dydrunoff,sedydrunoff,t.runoff)
round(table15[1:5,2:4], digits=3)

# Conditional Coefficients
candta$grps1.0 <-candta$groups-1
candta$intx1.0<-candta$grps1.0*candta$runoff
mod1.0<-lm(candidates~grps1.0+runoff+intx1.0, candta); round(summary(mod1.0)$coef[3,], digits=4)

candta$grps1.5 <-candta$groups-1.5
candta$intx1.5<-candta$grps1.5*candta$runoff
mod1.5<-lm(candidates~grps1.5+runoff+intx1.5, candta); round(summary(mod1.5)$coef[3,], digits=4)

candta$grps2.0 <-candta$groups-2
candta$intx2.0<-candta$grps2.0*candta$runoff
mod2.0<-lm(candidates~grps2.0+runoff+intx2.0, candta); round(summary(mod2.0)$coef[3,], digits=4)

candta$grps2.5 <-candta$groups-2.5
candta$intx2.5<-candta$grps2.5*candta$runoff
mod2.5<-lm(candidates~grps2.5+runoff+intx2.5, candta); round(summary(mod2.5)$coef[3,], digits=4)

candta$grps3.0 <-candta$groups-3
candta$intx3.0<-candta$grps3.0*candta$runoff
mod3.0<-lm(candidates~grps3.0+runoff+intx3.0, candta); round(summary(mod3.0)$coef[3,], digits=4)

round(summary(mod1.0)$coef[3,1:3], digits=3)
round(summary(mod1.5)$coef[3,1:3], digits=3)
round(summary(mod2.0)$coef[3,1:3], digits=3)
round(summary(mod2.5)$coef[3,1:3], digits=3)
round(summary(mod3.0)$coef[3,1:3], digits=3)

# Chow tests
# estimate separate regressions, and save sums of squared residuals and residual df's
n.reg <- lm(candidates~groups, candta[ which(candta$runoff==0), ]); summary(n.reg)
n.ss <- anova(n.reg)[2,2]; n.df <- anova(n.reg)[2,1]

y.reg <- lm(candidates~groups, candta[ which(candta$runoff==1), ]); summary(y.reg)
y.ss <- anova(y.reg)[2,2]; y.df <- anova(y.reg)[2,1]

# estimate combined samples regressions, without and with intercept shifts
pooled <- lm(candidates~groups, data=candta); summary(pooled)
p.ss <- anova(pooled)[2,2]; p.df <- anova(pooled)[2,1]

parallel <- lm(candidates~groups+runoff, data=candta); summary(parallel)
pp.ss <- anova(parallel)[3,2]; pp.df <- anova(parallel)[3,1]

# Chow test without intercept shift
Chow.p = ((p.ss-n.ss-y.ss)/2)/((n.ss+y.ss)/(n.df+y.df)); Chow.p
qf(.05, 2, 12, lower.tail=F)

#Chow test with intercept shift
Chow.pp = ((pp.ss-n.ss-y.ss)/1)/((n.ss+y.ss)/(n.df+y.df)); Chow.pp
qf(.05, 1, 12, lower.tail=F)