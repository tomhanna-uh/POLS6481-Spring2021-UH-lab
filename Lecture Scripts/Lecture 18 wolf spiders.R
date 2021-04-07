cannibal <- read.csv("C:/wolfspiders.csv")
attach(cannibal)

byhand=c(1,1,1,1,1,1,2,2,2,2,2,2,2,2,3,3,3,3,3,3,3,3,3,3,3,3,2,2,2,2,.5,2.5,2.5,2.5,2.5,2.5,.5,1,1,1.5,1.5,1.5,1,1,1,1,.5,.5,.5,.5,.5)
plot(Size.Difference, Cannibalism, pch=19, cex=byhand)

xcontinuous = seq(min(Size.Difference), max(Size.Difference), length=501)

lpm <- lm(Cannibalism ~ Size.Difference); summary(lpm)$coef[,1:3]
summary(predict(lpm))
hist(predict(lpm), breaks=10); abline(v=0, col="red", lwd=3)
linear <- lpm$coef[1] + xcontinuous*lpm$coef[2]
#plot(Size.Difference, Cannibalism, pch=19, cex=byhand)
plot(Size.Difference, Cannibalism, pch=19, cex=byhand, ylim=c(-.2,1.2))
lines (xcontinuous, linear, col="darkgreen", lwd=2)

logit <- glm(Cannibalism ~ Size.Difference, binomial); summary(logit)$coef[,1:3]
summary(predict(logit))
summary(predict(logit, type = "response"))
hist(predict(logit, type = "response"), breaks=10)
straight <- logit$coef[1] + xcontinuous*logit$coef[2]
curved <- 1/(1+exp(-1*straight))
plot(Size.Difference, Cannibalism, pch=19, cex=byhand)
lines(xcontinuous, curved, col="blue", lwd=2)
abline(h=.5, col="black", lty=2)
