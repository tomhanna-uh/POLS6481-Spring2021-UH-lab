#install.packages("faux")
library(faux)

set.seed(11032020)
dat <- rnorm_multi(n = 1000,
                   mu = c(0, 0, 0),
                   sd = c(1, 2, 4),
                   r = c(0.5, 0, 0),
                   varnames = c("x", "w", "v"),
                   empirical = FALSE)
attach(dat)
plot(x, type="l"); hist(x)
plot(w, type="l"); hist(w)
plot(v, type="l"); hist(v) 
plot(x, w, pch=16); cor(x, w, use="complete.obs") # != 0.5 if empirical = FALSE
plot(v, w, pch=16)
plot(x, v, pch=16)
u <- runif(1000, min=-3, max=3); 
mean(u); sd(u)
plot(u, type="l"); hist(u)
y <- x + w + v + u; mean(y); sd(y)
detach(dat)
dat$i = seq(1,1000)
dat$u = u; rm(u)
dat$y = y; rm(y)
hist(dat$u + dat$v)

sd(dat$v)
summary(lm(y ~ x + w + u + i, dat))

prf.v <- lm(y ~ x + w + u, dat); summary(prf.v)

sd(dat$u + dat$v)
summary(lm(y ~ x + w, dat))

#data <- as.data.frame(cbind(y, x, u))
N <- nrow(dat)
S <- 25

# 1. only v omitted
n <- seq(1, 100, length=100)
beta0 <- numeric(length(n))
beta1 <- numeric(length(n))
beta2 <- numeric(length(n))
sigma <- numeric(length(n))
for(i in 1:length(n)) {
  subsample <- dat[sample(1:N, size=S, replace=T), ]
  model <- lm(y ~ x + w + u, data=subsample)
    beta0[i] <- model$coef[1]
    beta1[i] <- model$coef[2]
    beta2[i] <- model$coef[3]
    sigma[i] <- summary(model)$sigma
}

# look just at the most recent model
summary(model)
confint(model)

# 100 samples drawn, with sample size of 25, therefore:
# Theorem 3.3 tells us average sigma should equal roughly 4 (sd(dat$v))
mean(sigma); sd(sigma)
# Theorem 3.1 tells us average beta1 should equal roughly 1
mean(beta1)
# Theorem 3.2 tells us s.d. of beta1 should equal roughly 4/(5*1*(1-.25))
sd(beta1)
# Theorem 4.X tells us the beta1 shoudl be approx Normal . . . 
hist(beta1, breaks=9); stem(beta1-1)
# 95% of beta1 should be within 2.06*1 units of 1
# 95% of (beta1-1) should be between -2.06 and +2.06

# Theorem 3.1 tells us average beta1 should equal roughly 1
mean(beta2)
# Theorem 3.2 tells us s.d. of beta2 should equal roughly 4/(5*2*(1-.25))
sd(beta2)
# Theorem 4.X tells us the beta2 should be approx Normal . . . 
hist(beta2, breaks=9); stem(beta2-1)
# 95% of beta2 should be within 2.06*.5 units of 1
# 95% of (beta1-1) should be between -1.03 and +1.03

# 2. both v and w omitted -- should bias beta1 upward
beta0ovb <- numeric(length(n))
beta1ovb <- numeric(length(n))
sigmaovb <- numeric(length(n))
for(i in 1:length(n)) {
  subsample <- dat[sample(1:N, size=S, replace=T), ]
  model <- lm(y ~ x + u, data=subsample)
  beta0ovb[i] <- model$coef[1]
  beta1ovb[i] <- model$coef[2]
  sigmaovb[i] <- summary(model)$sigma
}

mean(beta1ovb); sd(beta1ovb)
hist(beta1ovb)
mean(sigmaovb); sd(sigmaovb)

# 3. change sample sizes?
# 4. violate assumptions?