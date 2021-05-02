library(here)
library(ggplot2)
library(compactr)
library(foreign)
dataset <- read.dta(here("data","alexseev.dta"))
alexseev <- subset(dataset, region!="NA")
qqnorm(alexseev$e03ld); qqline(alexseev$e03ld, col = 3)
hist(alexseev$e03ld, freq = FALSE, breaks=19)

model <- lm(e03ld ~ slav89 + nonslav8 + sl89nsl8 + inc9903 + eduhi02 + unemp02 + apt9200 + vsall03 + brdcont, alexseev)
summary(model)

summary(alexseev$slav89); s.bar = mean(alexseev$slav89)
summary(alexseev$nonslav8); ns.bar = mean(alexseev$nonslav8)
summary(alexseev$sl89nsl8); sns.bar=mean(alexseev$sl89nsl8)
int.bar = s.bar*ns.bar

model$coef[2] + model$coef[4]*ns.bar
model$coef[3] + model$coef[4]*s.bar

attach(alexseev); x5=mean(inc9903); x6=mean(eduhi02); x7=mean(unemp02); x8=mean(apt9200); x9=mean(vsall03); x10=mean(brdcont); detach(alexseev) 
baseline = model$coef[1] + model$coef[5]*x5 + model$coef[6]*x6 + model$coef[7]*x7 + model$coef[8]*x8 + model$coef[9]*x9 + model$coef[10]*x10
allmeans = baseline + model$coef[2]*s.bar + model$coef[3]*ns.bar + model$coef[4]*sns.bar

sums = summary(alexseev$slav89); slav89 = c(sums[2], sums[3], sums[5], sums[2], sums[3], sums[5], sums[2], sums[3], sums[5])
sumns = summary(alexseev$nonslav8); nonslav8 = c(sumns[2], sumns[2], sumns[2], sumns[3], sumns[3], sumns[3], sumns[5], sumns[5], sumns[5])
new.data <- data.frame(cbind(slav89, nonslav8))
new.data$sl89nsl8 <- new.data$slav89*new.data$nonslav8
new.data$yhat <- baseline + model$coef[2]*new.data$slav89 + model$coef[3]*new.data$nonslav8 + model$coef[4]*new.data$sl89nsl8

index<-seq(min(alexseev$slav89), max(alexseev$slav89), length.out = 101)
yhat.q1 <- baseline + model$coef[2]*index + model$coef[3]*sumns[2] + model$coef[4]*index*sumns[2]
yhat.q2 <- baseline + model$coef[2]*index + model$coef[3]*sumns[3] + model$coef[4]*index*sumns[3]
yhat.q3 <- baseline + model$coef[2]*index + model$coef[3]*sumns[4] + model$coef[4]*index*sumns[4]
eplot(xlim = mm(c(25,100)), ylim = mm(c(10, 14)))
lines(index, yhat.q2); lines(index, yhat.q1, col="red"); lines(index, yhat.q3, col="blue")

smooth<-seq(min(alexseev$nonslav8), max(alexseev$nonslav8), length.out = 101)
dydsl89<-model$coef[2]+model$coef[4]*smooth
plot(smooth, dydsl89, pch="-", 
  main = "Marginal Effects Plot for Slavic Population in 1989",
  xlab = "Change in Percent Ethnic Minorities",
  ylab = "Estimated Coefficient for Percent Ethnic Majority", 
  ylim = c(-.02,.16))
abline(h=0, col=2)

vce=vcov(model)
varsl89<-vce[2,2]
varsl89nsl8<-vce[4,4]
cova<-vce[2,4]
sedydsl89<-sqrt(varsl89 + (smooth^2)*varsl89nsl8 + 2*smooth*cova)
plot(smooth, sedydsl89, ylim=c(0,.07))

t.star = qt(.95,model$df.residual)
upperci <- dydsl89 + t.star*sedydsl89
lowerci <- dydsl89 - t.star*sedydsl89
plot(smooth, dydsl89, pch="-", ylim=c(-.02,.16)); lines(smooth, upperci); lines(smooth, lowerci); abline(h=0, col="red")

install.packages("interplot")
library(interplot)
model2 <- lm(e03ld ~ slav89*nonslav8 + inc9903 + eduhi02 + unemp02 + apt9200 + vsall03 + brdcont, alexseev)
interplot(m = model2, var1 = "slav89", var2 = "nonslav8") +  # Can add "point=T" for points instead of line if desired; AND "hist=T" to include histogram
  xlab("Change in Percent Ethnic Minorities") +
  ylab("Estimated Coefficient for Percent Ethnic Majority") +
  theme_bw() +   # Change the background
  ggtitle("Marginal Effect of Majority Population on Zhirinovsky Bloc Support \nby Change in Minority Population") +
  theme(plot.title = element_text(face="bold")) +
  geom_hline(yintercept = 0, linetype = "dashed") +
  theme(axis.text.x  = element_text(angle=90))

interplot(m = model2, var1 = "slav89", var2 = "nonslav8", hist=T) +
  xlab("Change in Percent Ethnic Minorities") +
  ylab("Estimated Coefficient for Percent Ethnic Majority") +
  theme_bw() +   # Change the background
  ggtitle("Marginal Effect of Majority Population on Zhirinovsky Bloc Support \nby Change in Minority Population") +
  theme(plot.title = element_text(face="bold")) +
  geom_hline(yintercept = 0, linetype = "dashed") +
  theme(axis.text.x  = element_text(angle=90))