library(foreign)
govdta <- read.dta("C:/Users/Scott/govduration.dta")

# Quadratic Model
govdta$PSSQ <- govdta$PS^2 
quadmod <- lm(govdur~PS+PSSQ, govdta); summary(quadmod)
vcov(quadmod)
varb1<-vcov(quadmod)[2,2]
varb2<-vcov(quadmod)[3,3]
covar<-vcov(quadmod)[2,3]

# Marginal effects plot with actual values
govdta$conb<-quadmod$coefficient[2] + 2*quadmod$coefficient[3]*govdta$PS
govdta$conse<-sqrt(varb1 + 4*govdta$PSSQ*varb2 + 4*govdta$PS*covar)
govdta$upperq<-govdta$conb + 2.093*govdta$conse
govdta$lowerq<-govdta$conb - 2.093*govdta$conse
plot(govdta$PS, govdta$conb, xlim=range(40, 80), ylim=range(-3, 3), pch=19); points(govdta$PS, govdta$upperq, pch=6); points(govdta$PS, govdta$lowerq, pch=2); abline(h=0)

# Marginal effects plot with sequence
PSx<-seq(min(govdta$PS), max(govdta$PS), length.out = 1000)
dydps<-quadmod$coefficients[2]+2*PSx*quadmod$coefficients[3]
sedydps<-sqrt(varb1 + 4*(PSx^2)*varb2 + 4*PSx*covar); plot(PSx, sedydps, pch=".", ylim=c(-3,3))
upperci <- dydps + qt(.975,quadmod$df.residual)*sedydps
lowerci <- dydps - qt(.975,quadmod$df.residual)*sedydps
plot(PSx, dydps, pch=".", ylim=c(-3,3)); lines(PSx, upperci); lines(PSx, lowerci); abline(h=0, col="red")

