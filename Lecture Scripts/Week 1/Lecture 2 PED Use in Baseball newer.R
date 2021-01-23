x <- c(48, 63, 17, 9, 53, 18, 26, 56, 16)
y <- c(1.2, 0.7, 2.5, 3.9, 1.6, 3.2, 2.4, 1.0, 4.2)
origin <- c("Australia", "Canada", "Colombia", "Dominican", "JapanTaiwan", "Mexico", "PuertoRico", "USA", "Venezuela")
plot(x, y, pch=16, xlim = c(0,70), ylim = c(0,4.5))
text(x, y, labels=origin, pos=1, xpd=TRUE)
col1 <- x-mean(x)
col2 <- col1^2
col3 <- y-mean(y)
col4 <- col3^2
col5 <- col1*col3

slide7<-as.data.frame(cbind(col1, col2, col3, col4, col5))
slide7

covarxy = mean(col5)
varx = mean(col2)
vary = mean(col4)

model <- lm(y ~ x)
abline(model, col="black")
summary(model)$coef
summary(model)$sigma

round(model$fitted, digits=1)
round(model$residuals, digits=1)
round(sum(model$residuals), digits=4)
round(cor(model$residuals,x), digits=4)

options(scipen=999)
sum(model$residuals)
sum(model$residuals*x)

secondtable <-as.data.frame(cbind(model$fitted, model$residuals, model$residuals^2, x*model$residuals))
round(secondtable,digits=2)