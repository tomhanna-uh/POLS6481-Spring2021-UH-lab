data <- read.csv("C:/inequality and representation.csv")
complete <- data[1:20,1:3]
attach(complete)

simple <- lm(Representation ~ Inequality); summary(simple) #2a
round(coef(simple)[1]+coef(simple)[2], digits=4)           #2b

mean(Inequality); var(Inequality)                          #2c 1st and 2nd lines
mean(Representation); var(Representation)                  #2c 3rd and 4th lines
cov(Inequality, Representation)                            #2c 5th line

col1 = Inequality - mean(Inequality)
col2 = col1^2
col3 = Representation - mean(Representation)
col4 = col3^2
col5 = col1*col3
sum(col2); sum(col4); sum(col5)
sum(col2)/19; sum(col4)/19; sum(col5)/19

plot(Inequality, Representation, pch = 16)                 #2f
abline(lm(Representation ~ Inequality))                    #2f "if possible"
text(Inequality, Representation, labels=Country, pos=3, cex=.6, xpd=TRUE)

yhat = round(simple$fitted.values, digits=1)
uhat = round(simple$residuals, digits=1) 
uhatsq = round(simple$residuals^2, digits=2)

page5 = data.frame(Country, col1, col2, col3, col4, col5, yhat, uhat, uhatsq)
df <-page5[order(page4$Country),]
df

sum(uhatsq)                                                #2e SSR
sum(uhatsq)/simple$df.residual                             #2e MSE (sigma-hat squared)
sqrt(sum(uhatsq)/simple$df.residual)                       #2e root MSE (sigma)
summary(simple)$sigma                                      #2e shortcut to sigma

influence <- lm.influence(simple)
leverage <- influence$hat; round(leverage, digits=4)[1]    #2g hat value for Japan
discrep <- rstudent(simple); round(discrep, digits=4)[1]   #2g studentized residual for Japan
dffits <- dffits(simple); round(dffits, digits=4)[1]       #2g dffits for Japan

dfbeta <- dfbeta(simple); round(dfbeta, digits=3)[1]       #2h dfbetas for Japan

detach(complete)
complete$dummy <- ifelse(complete$Country=="Japan",c(1),c(0)) # three lines for #2i
dummied <- lm(Representation ~ Inequality + dummy, data=complete)
summary(dummied)$coef                                         #2i complete

yhatd = round(dummied$fitted.values, digits=1)[1:20]
uhatd = round(dummied$residuals, digits=1)[1:20]
uhatdsq = round(dummied$residuals^2, digits=2)[1:20]
sum(uhatdsq)

page5 = data.frame(Country[1:20], yhatd, yhat-yhatd, (yhat-yhatd)^2)
display <-page5[order(page5$Country),]
display
sum((yhat-yhatd)^2)
(sum((yhat-yhatd)^2))/(3*(summary(simple)$sigma^2))

cooks <- cooks.distance(simple); round(cooks, digits=4)[1]
