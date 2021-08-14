setwd('C:\data')
ad <- read.csv('ADmission.csv')
str(ad)

sum(is.na(ad))

ad2 <- ad[,-7]
str(ad2)

cor(ad2)
install.packages('corrplot')
install.packages('corrgram')
library(corrplot)
library(corrgram)

plot(ad2)
corrplot(cor(ad2),method='circle')
corrgram(cor(ad2),type = 'corr',upper.panel = panel.conf)

rg <- lm(Chance_of_Admit~., data=ad)         
summary(rg)

reb <- step(rg,direction = 'both')

rg2 <- lm(Chance_of_Admit ~ GRE + TOEFL + LOR + CGPA + Research,data=ad)

par(mfrow=c(2,2))
plot(rg2)

install.packages('GA')

