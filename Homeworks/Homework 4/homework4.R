library(here)
library(ggplot2)




###Part 2

library(haven)
fert2 <- read_dta(here("data","FERTIL2.DTA"))
View(fert2)




#10 
fert2 <- fert2[ which(is.na(electric) == FALSE), ]
q10 <- glm(children ~ educ + urban + electric + catholic + protest + 
               spirit, data = fert2, family = "poisson")
summary(q10)

print=data.frame(fert2,pred=q10$fitted)
print
fits=lm(children~pred, print); summary(fits)$r.squared
par(mfrow = c(2,2))
plot(fits)

pred=q10$fitted

# # Mean Effects
# pdf(file="meffect.pdf",height=6,width=6)
# ggplot(meffects,aes(x=x,y=meaneffects))+
#     geom_point(,color="red")+
#     theme_bw()+
#     labs(x="Tenure",y="Partial Change in E(y|x)")  
# dev.off()  

