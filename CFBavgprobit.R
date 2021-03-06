CFB2019 <- read.csv("~/OneDrive - SectorShield Inc/2020 Summer/CFB2019-v2.csv")
View(CFB2019) 
attach(CFB2019)

Vars18 <- c("Pass.Yards.Per.Game.Allowed","Penalty.Yards.Per.Game","Avg.Yards.Per.Punt.Return","Avg.Yards.Allowed.per.Punt.Return","Rushing.Yards.per.Game")
Data18 <- CFB2019[Vars18]
names(Data18)[1:5]=c("x1","x2","x3","x4","x5")


probitmodel1<-glm(as.factor(WR) ~ .*.*.,family=binomial(link='probit'),data=Data18)
summary(probitmodel1)
drop1(probitmodel1,test = "LRT")
probitmodel2<-update(probitmodel1, .~. -x2:x3:x4)
summary(probitmodel2)
drop1(probitmodel2,test = "LRT")
probitmodel3<-update(probitmodel2, .~. -x1:x3:x5)
summary(probitmodel3)
drop1(probitmodel3,test = "LRT")
probitmodel4<-update(probitmodel3, .~. -x2:x3:x5)
summary(probitmodel4)
drop1(probitmodel4,test = "LRT")
probitmodel5<-update(probitmodel4, .~. -x1:x2:x4)
summary(probitmodel5)
drop1(probitmodel5,test = "LRT")
probitmodel6<-update(probitmodel5, .~. -x1:x2:x3)
summary(probitmodel6)
drop1(probitmodel6,test = "LRT")
probitmodel7<-update(probitmodel6, .~. -x2:x3)
summary(probitmodel7)
drop1(probitmodel7,test = "LRT") #AIC 170.10
probitmodel8<-update(probitmodel7, .~. -x1:x2:x5) #AIC 170.63
summary(probitmodel8)
drop1(probitmodel8,test = "LRT")
probitmodel9<-update(probitmodel8, .~. -x1:x2)
summary(probitmodel9)
drop1(probitmodel9,test = "LRT")
probitmodel10<-update(probitmodel9, .~. -x1:x3:x4)
summary(probitmodel10)
drop1(probitmodel10,test = "LRT")
probitmodel11<-update(probitmodel10, .~. -x1:x3)
summary(probitmodel11)
drop1(probitmodel11,test = "LRT")
probitmodel12<-update(probitmodel11, .~. -x2:x4:x5)
summary(probitmodel12)
drop1(probitmodel12,test = "LRT")
probitmodel13<-update(probitmodel12, .~. -x2:x5)
summary(probitmodel13)
drop1(probitmodel13,test = "LRT")
probitmodel14<-update(probitmodel13, .~. -x3:x4:x5)
summary(probitmodel14)
drop1(probitmodel14,test = "LRT")
probitmodel15<-update(probitmodel14, .~. -x3:x5)
summary(probitmodel15)
drop1(probitmodel15,test = "LRT")
probitmodel16<-update(probitmodel15, .~. -x2:x4)
summary(probitmodel16)
drop1(probitmodel16,test = "LRT")
probitmodel17<-update(probitmodel16, .~. -x2)
summary(probitmodel17)
drop1(probitmodel17,test = "LRT")
probitmodel18<-update(probitmodel17, .~. -x3:x4)
summary(probitmodel18)
drop1(probitmodel18,test = "LRT")
probitmodel19<-update(probitmodel18, .~. -x3)
summary(probitmodel19) #AIC 163.93
drop1(probitmodel19,test = "LRT")
probitmodel20<-update(probitmodel19, .~. -x1:x4:x5)
summary(probitmodel20) #AIC 166.37
drop1(probitmodel20,test = "LRT")
probitmodel21<-update(probitmodel20, .~. -x4:x5)
summary(probitmodel21) #AIC 164.37
drop1(probitmodel21,test = "LRT")
probitmodel22<-update(probitmodel21, .~. -x1:x5)
summary(probitmodel22) #AIC 162.92
drop1(probitmodel22,test = "LRT")
library(ROCR)

