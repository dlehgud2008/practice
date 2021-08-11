setwd('C:\\data')
fifa <- read.csv('fifa.csv')
fifa
head(fifa)
str(fifa)
fifa$Height
class(fifa$Height)

f2 <- fifa$Height
f3 <- as.numeric(f2)
f3
f2
# 1
library(data.table)
Height_cm<- fread(paste(sub('"', "", fifa$Height), collapse="\n"), sep="'")[, 
                                                          as.matrix(.SD) %*% c(30.48, 2.5)][,1]
fifa$Height_cm <- Height_cm
str(fifa)
# 2
install.packages(gsubfn)
library(gsubfn)
as.numeric(gsubfn("(\\d)'(\\d+)", ~ as.numeric(x) * 30.48 + 
                    as.numeric(y) * 2.54, sub('"', '', fifa$Height)))

#3
install.packages("tidyverse")
library(tidyverse)
fifa %>% 
  separate(Height, into = c("H1", "H2"), convert = TRUE) %>%
  transmute(Height = H1 * 30.48 + H2 * 2.54)

po <- fifa$Position
str(po)

str(fifa)
fifa<-within(fifa,
            {Position_Class = character(0) 
            Position_Class[ Position %in% c("LS", "ST", "RS", "RW",'LW','LF','CF','RF') ]="Forward"
            Position_Class[ Position %in% c("LAM", "CAM", "RAM", "RM",'LM','CM','LCM','RCM') ]="Midfielder"
            Position_Class[ Position %in% c('LWB','LDM','CDM','RDM','RWB','LB','LCB','CB','RCB','RB')]='Defender'
            Position_Class[ Position %in% 'GK']='Goalkeeper'} )

str(fifa)

fifa$Position_Class <- as.factor(fifa$Position_Class) 
fifa$Position_Class


sum(is.na(fifa$Position_Class))

library(car)
leveneTest(Value~Position_Class,data=fifa) # 등분산성 위배 되는거 아님?
out <- aov(Value~Position_Class, data=fifa)
summary(out)

TukeyHSD(out)

#welch 등분산성 위배될때
m1 <- oneway.test(Value~Position_Class,data=fifa,var.equal = F)
m1

#등분산성 위배됨
leveneTest(Value~Position_Class*Preferred_Foot, data = fifa)

out2 <- aov(Value~Position_Class*Preferred_Foot, data=fifa)
summary(out2)
TukeyHSD(out2)



#상호작용 효과
interaction.plot(x.factor = fifa$Position_Class, trace.factor = fifa$Preferred_Foot,
                 response = fifa$Value,legend=T,fun = mean)

interaction.plot(x.factor = fifa$Preferred_Foot, trace.factor = fifa$Position_Class,
                 response = fifa$Value,legend=TRUE)


#회귀분석
out3 <- lm(Value~Age+Overall+Wage+Height_cm+Weight_lb,data=fifa)
vif(out3)
summary(out3)

out4 <- step(out3,direction = 'both')
out41 <- step(out3, scope=list(lower=~1,upper=~Age+Overall+Wage+Height_cm+Weight_lb),
              direction = 'both')

out5 <- lm(Value~Age+Overall+Wage+Height_cm,data=fifa)
summary(out5)


#wordcloud

install.packages('KoNLP')


package_version(R.version)

install.packages('multilinguer')
install.packages('rJava')
library(multilinguer)
install_jdk()
install.packages(c("hash", "tau", "Sejong", "RSQLite", "devtools", "bit", "rex", "lazyeval", "htmlwidgets", "crosstalk", "promises", "later", "sessioninfo", "xopen", "bit64", "blob", "DBI", "memoise", "plogr", "covr", "DT", "rcmdcheck", "rversions"), type = "binary")
install.packages("remotes")
remotes::install_github('haven-jeon/KoNLP', upgrade = "never", INSTALL_opts=c("--no-multiarch"))

install.packages("remotes")
#remotes::install_github("mrchypark/multilinguer")

install.packages('tm')
library(tm)
library(KoNLP)
useSejongDic()
text <- readLines('영화 기생충_review.txt')
text

dic <- readLines('영화 기생충_사전.txt')

buildDictionary(ext_dic = 'woorimalsam',user_dic = data.frame(readLines('영화 기생충_사전.txt'),'ncn'),replace_usr_dic=T)

CORPUS = Corpus(VectorSource(text)) # 코퍼스 생성
CORPUS_TM = tm_map(CORPUS,removePunctuation) # 특수문자 제거 
CORPUS_TM = tm_map(CORPUS_TM, removeNumbers) # 숫자 제거 
CORPUS_TM = tm_map(CORPUS_TM, tolower) # 알파벳 모두 소문자로 바꾸기

TDM=DocumentTermMatrix(CORPUS_TM) # 문서행렬 생성
inspect(TDM)

CORPUS_TM[1:10]
library(rJava)
install.packages('wordcloud')
library(wordcloud)
library(plyr)
clean = tm_map(CORPUS,removePunctuation) # 특수문자 제거 
clean = tm_map(CORPUS_TM, removeNumbers) # 숫자 제거 
clean = tm_map(CORPUS_TM, tolower) # 알파벳 모두 소문자로 바꾸기
clean[1:4]

text[1:10]

text2 <- tm_map(text,removePunctuation)
t
