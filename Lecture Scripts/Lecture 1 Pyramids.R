
width <- c(230,220,215,188,102)
height <- c(147,105,143,105,65)
pyramids <- c("Khafu", "Red", "Khafre", "Bent", "Menkaure")
horiz <- c(0,250)
vert <- c(0,150)
plot(width, height, pch=16, xlim = horiz, ylim = vert)
text(width, height, labels=pyramids, pos=1, xpd=TRUE)

egypt <- cbind(pyramids,width,height)
pyr <- data.frame(egypt)

myModel = lm(height ~ width)
summary(myModel)

round(cor(myModel$residuals, width), digits=5)
round(sum(myModel$residuals), digits=5)