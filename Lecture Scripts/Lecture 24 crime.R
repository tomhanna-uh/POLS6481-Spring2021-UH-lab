library(foreign)
crime <- read.dta("C:/CRIME2.DTA")

wave2 <- crime[crime$year==87, ]
slide6a <- lm(crmrte ~ unem, wave2); summary(slide6a)
slide6b <- lm(crmrte ~ unem + d87, crime); summary(slide6b)

slide7 <- lm(ccrmrte ~ cunem, crime); summary(slide7)
# note "46 observations deleted due to missingness"; could use wave2 instead
# jump to line 25 or 26 (depending on whether plm loaded in another R script)
# note dataset already includes change variables; to generate these fresh, use:
crime$d.crmrte<-c()
for (i in 1:92){
  crime$d.crmrte[i]<-ifelse(crime$year[i]>crime$year[i-1], 
                          crime$crmrte[i]-crime$crmrte[i-1], NA)
}
crime$d.unem<-c()
for (i in 1:92){
  crime$d.unem[i]<-ifelse(crime$year[i]>crime$year[i-1], 
                          crime$unem[i]-crime$unem[i-1], NA)
}
# note data needs to be sorted correctly (group cross-sectional units together, ordered by year)
slide7too <- lm(d.crmrte ~ d.unem, crime); summary(slide7too)

install.packages("plm"); library(plm)
pooled <- plm(crmrte ~ unem + d87, crime, index=c("area", "year"), model="pooling")
summary(pooled)

fixed <- plm(crmrte ~ unem + d87, crime, index=c("area", "year"), model="within")
summary(fixed)

random <- plm(crmrte ~ unem + d87, crime, index=c("area", "year"), model="random")
summary(random)
phtest(fixed, random)

# check whether current law expenditures depend on crime, controlling for past expenditures
crime$l.crmrte<-c()
for (i in 1:92){
  crime$l.crmrte[i]<-ifelse(crime$year[i]>crime$year[i-1], crime$crmrte[i-1], NA)
}
crime$l.llawexpc<-c()
for (i in 1:92){
  crime$l.llawexpc[i]<-ifelse(crime$year[i]>crime$year[i-1], crime$llawexpc[i-1], NA)
}
budget <- lm(llawexpc ~ l.crmrte + l.llawexpc, crime); summary(budget)