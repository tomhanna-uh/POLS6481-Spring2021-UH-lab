data <- read.csv("C:/ex1123.csv"); names(data)
cov(data$Mort, data$SO2)
var(data$SO2)
cov(data$Mort, data$SO2)/var(data$SO2)
simple <- lm(Mort ~ SO2, data)
summary(simple)$coef[2]

cov(data[c(2,5,7)])
M1 = cov(data[c(2,5,7)])
numerator = (M1[3,1]/M1[3,3]) - (M1[2,1]/M1[2,2])*(M1[3,2]/M1[3,3])
denominator = 1 - ((M1[3,2]*M1[3,2])/(M1[2,2]*M1[3,3]))
numerator/denominator
multiple <- lm(Mort ~ SO2 + NonWhite, data)
summary(multiple)$coef[2]

cov(data[c(2,3,7)])
multiple <- lm(Mort ~ SO2 + Precip, data)
summary(multiple)$coef[2]
