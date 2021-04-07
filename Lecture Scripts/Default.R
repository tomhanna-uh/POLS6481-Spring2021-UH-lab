# Default <- read.csv("http://courses.atlas.illinois.edu/spring2016/STAT/STAT200/RProgramming/data/Default.csv")
# write.csv(Default, file = "H:/default.csv")
# http://courses.atlas.illinois.edu/spring2016/STAT/STAT200/RProgramming/Maximum_Likelihood.html
# 'data' comes from ISLR package (book: Introduction to Statistical Learning)
# 10,000 cases simulated 
Default <- read.csv("C:/default.csv")
y <- as.integer(Default$default=="Yes")
mean(y)

# Query 1. Do students default more often?
tapply(y, Default$student, mean)
t.test(y ~ Default$student, var.equal=TRUE)

options(scipen=999)
# Query 2. Do people with larger balances default more often?
plot(balance ~ default, data=Default, las=1)
# can divide into 10 intervals of equal length ...
balance_cut1 <- cut(Default$balance, breaks=10)
levels(balance_cut1)
table(balance_cut1)
# ... or can divide into 10 groups of equal sizes
quantiles <- quantile(Default$balance, probs=seq(0,1,length=11))
balance_cut2 = cut(Default$balance, breaks=quantiles, include.lowest=TRUE)
levels(balance_cut2)
table(balance_cut2)
p <- tapply(y, balance_cut2, mean)
p
# display data
balance_cut2_midpoints = rep(NA,10) # initialize a vector of length 10 
for (i in 1:10) {
  balance_cut2_midpoints[i] = (quantiles[i] + quantiles[i+1])/2
}
plot(y ~ Default$balance, las=1, pch=20, xlab="Balance")
points(balance_cut2_midpoints, p, col="red", pch=20)
abline(v=quantiles, col="blue")

# focus on last 10% (divide it into 5 groups each representing 2%)
quan_last <- quantile(Default$balance, probs=c(0.92,0.94,0.96,0.98,1))
quan_combined <- c(quantiles[1:10], quan_last)
quan_combined
balance_cut3 <- cut(Default$balance, breaks=quan_combined, include.lowest=TRUE)
table(balance_cut3)
p <- tapply(y, balance_cut3, mean)
p

nintervals = length(levels(balance_cut3))
balance_cut3_midpoints <- rep(NA,nintervals)
for (i in 1:nintervals) {
  balance_cut3_midpoints[i] <- (quan_combined[i] + quan_combined[i+1])/2
}
plot(y ~ Default$balance, las=1, pch=20, xlab="Balance")
points(balance_cut3_midpoints, p, col="red", pch=20)
abline(v=quan_combined, col="blue")

# logit 
fit <- glm(y ~ Default$balance, family=binomial)
summary(fit)
#plot(y ~ Default$balance, las=1, pch=20, xlab="Balance")
#points(balance_cut3_midpoints, p, col="red", pch=20)
#abline(v=quan_combined, col="blue")
logit <- function(x,beta0,beta1) {
  1/(1+exp(-beta0 - beta1*x))
}
curve(logit(x,fit$coefficients[1],fit$coefficients[2]), col="green", add=TRUE)

scobit <- function(x,beta0,beta1,alpha) {
  1/((1+exp(-beta0 - beta1*x))^alpha)
}
curve(scobit(x,-10.63,0.0056,0.85775), col="purple", lwd=2, add=TRUE)