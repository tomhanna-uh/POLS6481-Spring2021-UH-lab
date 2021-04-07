# http://courses.atlas.illinois.edu/spring2016/STAT/STAT200/RProgramming/Maximum_Likelihood.html

# Define the logistic function
logit <- function(x,beta0,beta1) {
  (1+exp(-(beta0 + beta1*x)))^-1
}

# Plot the logistic function with beta1=1 and 3 different values of beta0.
curve(logit(x,0,1),xlim=c(-10,10), lwd=2,ylab="Logistic Function", las=1)
curve(logit(x,-2,1),xlim=c(-10,10), lwd=2, lty=2, col="red", add=TRUE)
curve(logit(x,2,1),xlim=c(-10,10), lwd=2, lty=2, col="blue", add=TRUE)
legend(-10,1,c(expression(paste(beta[0]," = 0")),expression(paste(beta[0]," = -2")),expression(paste(beta[0]," = 2"))), lwd=2, lty=1:3, col=c("black","red","blue"))

# Plot the logistic function with beta0=0 and 3 different +ve values of beta1.
curve(logit(x,0,1),xlim=c(-10,10), lwd=2,ylab="Logistic Function", las=1)
curve(logit(x,0,2),xlim=c(-10,10), lwd=2, lty=2, col="red", add=TRUE)
curve(logit(x,0,0.5),xlim=c(-10,10), lwd=2, lty=3, col="blue", add=TRUE)
legend(-10,0.9,c(expression(paste(beta[1]," = 1")),expression(paste(beta[1]," = 2")),expression(paste(beta[1]," = 0.5"))), lwd=2, lty=1:3, col=c("black","red","blue"))

# Plot the logistic function with beta0=0 and 3 different -ve values of beta1.
curve(logit(x,0,-1),xlim=c(-10,10), lwd=2,ylab="Logistic Function", las=1)
curve(logit(x,0,-2),xlim=c(-10,10), lwd=2, lty=2, col="red", add=TRUE)
curve(logit(x,0,-0.5),xlim=c(-10,10), lwd=2, lty=3, col="blue", add=TRUE)
legend(5,0.9,c(expression(paste(beta[1]," = -1")),expression(paste(beta[1]," = -2")),expression(paste(beta[1]," = 0.5"))), lwd=2, lty=1:3, col=c("black","red","blue"))

# Define the scobit function
scobit <- function(x,beta0,beta1,alpha) {
  (1+exp(-(beta0 + beta1*x)))^-alpha
}
# Plot the scobit function with beta0=0 and beta1=1 and 3 different values of alpha.
curve(scobit(x,0,1,1),xlim=c(-10,10), lwd=2,ylab="Logistic Function", las=1)
curve(scobit(x,0,1,.67),xlim=c(-10,10), lwd=2, lty=2, col="red", add=TRUE)
curve(scobit(x,0,1,.33),xlim=c(-10,10), lwd=2, lty=3, col="blue", add=TRUE)
legend(-10,0.9,c(expression(paste(alpha," = 1")),expression(paste(alpha," = .67")),expression(paste(alpha," = 0.33"))), lwd=2, lty=1:3, col=c("black","red","blue"))

