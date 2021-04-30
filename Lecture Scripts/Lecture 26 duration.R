library(foreign)
install.packages("compactr")
library(compactr)
govdta <- read.dta("C:/govduration.dta")
govdta$NPPS<-govdta$NP*govdta$PS
# for the regression results in Table 5 (p. 31) shown on slide 4
intmod <- lm(govdur~NP+PS+NPPS+PD, govdta); summary(intmod)

# for the first row of predicted values in Table 6 (p. 32) shown on slide 5
intmod$coefficients[1]+intmod$coefficients[3]*40+intmod$coefficients[2]*1+intmod$coefficients[4]*1*40+intmod$coefficients[5]
intmod$coefficients[1]+intmod$coefficients[3]*40+intmod$coefficients[2]*2+intmod$coefficients[4]*2*40+intmod$coefficients[5]
intmod$coefficients[1]+intmod$coefficients[3]*40+intmod$coefficients[2]*3+intmod$coefficients[4]*3*40+intmod$coefficients[5]
intmod$coefficients[1]+intmod$coefficients[3]*40+intmod$coefficients[2]*4+intmod$coefficients[4]*4*40+intmod$coefficients[5]
# alternative method of finding first *column* of Table 6 (p. 32)  on slide 5... with confidence intervals
table.data <- data.frame(NP=c(1,1,1,1,1), PS=c(40,50,60,70,80), NPPS=c(40,50,60,70,80), PD=c(1,1,1,1,1))
predict(intmod, newdata=table.data, interval='prediction')
#predict(intmod, newdata=table.data, se.fit = T)

vce=vcov(intmod)
round(vce[2:5,2:5], digits=3) # Estimated VCE Matrix (Table 19, p. 56) on slide 7
varbnp<-vcov(intmod)[2,2]
varbps<-vcov(intmod)[3,3]
varbnpps<-vcov(intmod)[4,4]
covbnpbnpps<-vcov(intmod)[4,2]
covbpsbnpps<-vcov(intmod)[4,3]

# First three columns of Table 20 (p. 56) shown on slide 7
attach(table.data)
mfx.np = intmod$coef[2] + PS*intmod$coef[4]
se.mfx.np = sqrt(varbnp + (PS^2)*varbnpps + 2*PS*covbnpbnpps)
t.np = mfx.np/se.mfx.np
detach(table.data)
table20 = cbind(table.data$PS, mfx.np, se.mfx.np,t.np)
round(table20, digits=3)

# Figure 8 (p. 69) shown on slide 7
PSx<-seq(min(govdta$PS), max(govdta$PS), length.out = 1000)
dydnp<-intmod$coefficients[2]+intmod$coefficients[4]*PSx
sedydnp<-sqrt(varbnp + (PSx^2)*varbnpps + 2*PSx*covbnpbnpps)
t.star  = qt(.95,intmod$df.residual)
upperci <- dydnp + t.star*sedydnp
lowerci <- dydnp - t.star*sedydnp
eplot(xlim = mm(c(40,80)), ylim = mm(c(-20, 20)))
lines(PSx, dydnp, lwd=2)
lines(PSx, upperci, lty=2, lwd=2)
lines(PSx, lowerci, lty=2, lwd=2)
abline(h=0)

# first row of table 20 (p. 56) shown on slide 7 using "rescaling"
govdta$PS40<-govdta$PS-40
govdta$NPPS40<-govdta$NP*govdta$PS40
govmod40<-lm(govdur~NP+PS40+NPPS40+PD, govdta)
round(summary(govmod40)$coef[2,], digits=3)

# middle row of table 20 (p. 56)  shown on slide 7 using "rescaling"
govdta$PS60<-govdta$PS-60
govdta$NPPS60<-govdta$NP*govdta$PS60
govmod60<-lm(govdur~NP+PS60+NPPS60+PD, govdta)
round(summary(govmod60)$coef[2,], digits=3)

# last  row of table 20 (p. 56) shown on slide 7 using "rescaling"
govdta$PS80<-govdta$PS-80
govdta$NPPS80<-govdta$NP*govdta$PS80
govmod80<-lm(govdur~NP+PS80+NPPS80+PD, govdta)
round(summary(govmod80)$coef[2,], digits=3)

# Table 21 (p. 57) and Figure 9 (p. 70) shown on slide 8
govdta$dydps<-intmod$coefficients[3]+intmod$coefficients[4]*govdta$NP
govdta$sedydps<-sqrt(varbps + (govdta$NP^2)*varbnpps + 2*govdta$NP*covbpsbnpps)
# Use specific countries to calculate values in first three columns of Table 21
govdta$dydps[5]; govdta$sedydps[5]; govdta$dydps[5]/govdta$sedydps[5] # NP=1
govdta$dydps[14]; govdta$sedydps[14]; govdta$dydps[14]/govdta$sedydps[14] # NP=2
govdta$dydps[8]; govdta$sedydps[8]; govdta$dydps[8]/govdta$sedydps[8] # NP=3
# Plot marginal effects and confidence interval
NPx<-seq(min(govdta$NP), max(govdta$NP), length.out = 1000)
dydps<-intmod$coefficients[3]+intmod$coefficients[4]*NPx
sedydps<-sqrt(varbps + (NPx^2)*varbnpps + 2*NPx*covbpsbnpps)
upperci.ps <- dydps + t.star*sedydps
lowerci.ps <- dydps - t.star*sedydps
eplot(xlim = mm(c(1,4)), ylim = mm(c(-1, 2)))
lines(NPx, dydps, lwd=2)
lines(NPx, upperci.ps, lty=2, lwd=2)
lines(NPx, lowerci.ps, lty=2, lwd=2)
abline(h=0)
