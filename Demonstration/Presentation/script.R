library(here)
library(foreign)
library(tseries) #For lagging
library(lmtest) #For test for joint significance
library(plm) #For plm command  --- if needed install.packages("plm")
library(pglm)
library(pcse) #For panel corrected standard errors -- if needed install.packages("pcse")

#This is using data on non-democratic nations involvement in Militarized Interstate Disputes (MIDS), the data is from the Correlates of War dataset. Additionally, 
#there are variables from the Varieties of Democracy (VDEM) project and from Jeff Colgan's Revolutionary Leader's database. This is unbalanced panel data, so
#I won't be using the PLM package. 

library(readr)
NDC <- read_csv(here("data","nondemocraciesconflict.csv"))
View(NDC)
names(NDC) #I like to do this because it makes organizing and using variables easier

## The following produces an error because there are duplicates - some countries (COWcodes) 
## appear in multiple disputes in the same year. This is okay, because at this point I am 
## just demonstrating how to copy the console output to LaTeX.

panelNDC <- pdata.frame(NDC, c("COWcode","year"))
pdim(panelNDC)

# Plotting one of the explanatory variables I use against a common control variable for conflict
# Multicollinearity rears its ugly head
#The year I'm using is the year before Mikhail Gorbachev became Secretary General of the 
#Communist Party of the Soviet Union

plot(panelNDC$transformativeideology[panelNDC$year==1984], panelNDC$peaceyears[panelNDC$year==1984],
     xlab = "Regime Transformative Ideology", ylab = "Years of Peace")
bimod.1984 <- lm(panelNDC$peaceyears[panelNDC$year==1984] ~
                     panelNDC$transformativeideology[panelNDC$year==1984])
abline(bimod.1984, col="red")

#I built the plot above using the lab code as an example
# plot(pdata$beertax[pdata$year==1982], pdata$vfrall[pdata$year==1982],
#      xlab="Tax on Case of Beer", ylab="vfrall", xlim=c(0,3), ylim=c(1,4))
# bimod.82<-lm(pdata$vfrall[pdata$year==1982]~pdata$beertax[pdata$year==1982])

#I'm going to compare this to a year after the end of the Cold War, right after Gorbachev left office
plot(panelNDC$transformativeideology[panelNDC$year==1992], panelNDC$peaceyears[panelNDC$year==1992],
     xlab = "Regime Transformative Ideology", ylab = "Years of Peace")
bimod.1992 <- lm(panelNDC$peaceyears[panelNDC$year==1992] ~
                     panelNDC$transformativeideology[panelNDC$year==1992])
abline(bimod.1992, col="red")

#Now I'm going to build three alternative models and build tables
#I'm using GLM models because my outcome variable is binary

model1 <- glm(nonterritorial ~ transformativeideology + cinc2 + coldwar
              + peaceyears, data = NDC, family = binomial(link = "logit"))
summary(model1)

modelfixed <- glm(nonterritorial ~ transformativeideology + cinc2 + coldwar
                  + peaceyears + as.factor(COWcode), data = NDC, 
                  family = binomial(link = "logit"))
summary(modelfixed)

modelrandom <- pglm(nonterritorial ~ transformativeideology + cinc2 + coldwar
                    + peaceyears, data = panelNDC, model = "random", 
                    family = binomial(link = "logit"))
summary(modelrandom)

library(stargazer)
#I included this just to show you something I talked about before - stargazer sometimes
#doesn't play well with other packages
stargazer(modelrandom)

##So, I'm going to make a table with the base and fixed effects models only
stargazer(model1,modelfixed)