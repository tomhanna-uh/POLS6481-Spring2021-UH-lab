options(scipen=999)
data <- read.csv("C:/suicide.csv")
attach(data)
mean(suicides); var(suicides); sd(suicides) #1a left column
mean(unemployment); var(unemployment); sd(unemployment) # 1a right column
cov(unemployment,suicides); cor(unemployment,suicides) #1b covariance and correlation
cov(unemployment,suicides)/var(unemployment) #1c slope
mean(suicides) - (mean(unemployment)*cov(unemployment,suicides)/var(unemployment)) #1c intercept
simple <- lm(suicides~unemployment); summary(simple) #1d
simple$coef[1] + (mean(unemployment)*simple$coef[2]) #1e
round(simple$fitted.values,digits=3) #1f first column
sum(simple$fitted.values); mean(simple$fitted.values) #1f first column sum & average
round(simple$residuals,digits=3) #1f second column
sum(simple$residuals); mean(simple$residuals) #1f second column sum & average
products <- unemployment*simple$residuals; round(products, digits=3) #1f third column
sum(products); mean(products) #1f third column sum & average