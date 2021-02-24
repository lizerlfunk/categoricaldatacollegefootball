library(VGAM)
CFB2019 <- read.csv("~/OneDrive - SectorShield Inc/2020 Summer/CFB2019-v2.csv")
View(CFB2019) 
attach(CFB2019)

Wins<-factor(CFB2019$Wins)
Vars18 <- c("Pass.Yards.Per.Game.Allowed","Penalty.Yards.Per.Game","Avg.Yards.Per.Punt.Return","Avg.Yards.Allowed.per.Punt.Return","Rushing.Yards.per.Game")
Data18 <- CFB2019[Vars18]

CFB2019$WinCategory[CFB2019$Wins==10|CFB2019$Wins==11|CFB2019$Wins==12|CFB2019$Wins==13|CFB2019$Wins==14|CFB2019$Wins==15]="10 or More"
CFB2019$WinCategory[CFB2019$Wins==6|CFB2019$Wins==7|CFB2019$Wins==8|CFB2019$Wins==9]= "6 to 9"
CFB2019$WinCategory[CFB2019$Wins==0|CFB2019$Wins==1|CFB2019$Wins==2|CFB2019$Wins==3|CFB2019$Wins==4|CFB2019$Wins==5]="5 or Less"
class(CFB2019$WinCategory)

################
#Example R code#
################
Data18$Categories = CFB2019$WinCategory
head(Data18)
dim(Data18)
#[1] 130  21
names(Data18)[1:5]=c("x1","x2","x3","x4","x5")

mult.fit1 = vglm(as.factor(Data18$Categories) ~ x1*x2*x3*x4*x5, family=multinomial, data = Data18)

summary(mult.fit1)
drop1(mult.fit1, test = "LRT")

mult.fit2 = vglm(as.factor(Data18$Categories) ~ x2*x3*x4*x5+x1*x3*x4*x5+x1*x2*x4*x5+x1*x2*x3*x5+x1*x2*x3*x4, family=multinomial, data = Data18)
  
summary(mult.fit2)
drop1(mult.fit2, test = "LRT")

mult.fit3 <- update(mult.fit2, .~. -x2:x3:x4:x1)
summary(mult.fit3)

drop1(mult.fit3, test = "LRT")

mult.fit4 <- update(mult.fit3, .~. -x2:x3:x5:x1)
summary(mult.fit4)

drop1(mult.fit4, test = "LRT")

mult.fit5 <- update(mult.fit4, .~. -x2:x3:x1)
summary(mult.fit5)

drop1(mult.fit5, test = "LRT")

mult.fit6 <- update(mult.fit5, .~. -x2:x3:x4:x5)
summary(mult.fit6)

drop1(mult.fit6, test = "LRT")

mult.fit7 <- update(mult.fit6, .~. -x2:x3:x5)
summary(mult.fit7)

drop1(mult.fit7, test = "LRT")

mult.fit8 <- update(mult.fit7, .~. -x2:x3:x4)
summary(mult.fit8)

drop1(mult.fit8, test = "LRT")

mult.fit9 <- update(mult.fit8, .~. -x3:x4:x5:x1)
summary(mult.fit9)

drop1(mult.fit9, test = "LRT")

mult.fit10 <- update(mult.fit9, .~. -x3:x5:x1)
summary(mult.fit10)

drop1(mult.fit10, test = "LRT")

mult.fit11 <- update(mult.fit10, .~. -x3:x4:x1)
summary(mult.fit11)

drop1(mult.fit11, test = "LRT")

mult.fit12 <- update(mult.fit11, .~. -x3:x1)
summary(mult.fit12)

drop1(mult.fit12, test = "LRT")

mult.fit13 <- update(mult.fit12, .~. -x2:x3)
summary(mult.fit13)

drop1(mult.fit13, test = "LRT")

mult.fit14 <- update(mult.fit13, .~. -x3:x4:x5)
summary(mult.fit14)

drop1(mult.fit14, test = "LRT")

mult.fit15 <- update(mult.fit14, .~. -x3:x4)
summary(mult.fit15)

drop1(mult.fit15, test = "LRT")

mult.fit16 <- update(mult.fit15, .~. -x2:x4:x5:x1)
summary(mult.fit16)

drop1(mult.fit16, test = "LRT")

mult.fit17 <- update(mult.fit16, .~. -x2:x4:x1)
summary(mult.fit17)

drop1(mult.fit17, test = "LRT")

mult.fit18 <- update(mult.fit17, .~. -x2:x4:x5)
summary(mult.fit18) #AIC 260.35

drop1(mult.fit18, test = "LRT")

mult.fit19 <- update(mult.fit18, .~. -x3:x5)
summary(mult.fit19) 

drop1(mult.fit19, test = "LRT")

mult.fit20 <- update(mult.fit19, .~. -x3)
summary(mult.fit20) 

drop1(mult.fit20, test = "LRT")

############
#question 2

library("ordinal")
ord.fit1 = clm(as.factor(Categories) ~ x1*x2*x3*x4*x5, family=multinomial, data = Data18)
summary(ord.fit1)
drop1(ord.fit1,test = "Chisq")

ord.fit2 = clm(as.factor(Categories) ~ x2*x3*x4*x5 + x1*x3*x4*x5 + x1*x2*x4*x5 + x1*x2*x3*x5 + x1*x2*x3*x4, family=multinomial, data = Data18)
summary(ord.fit2)
drop1(ord.fit2,test = "Chisq")

ord.fit3 <- update(ord.fit2, .~. -x2:x4:x5:x1)
summary(ord.fit3)
drop1(ord.fit3,test = "Chisq")

ord.fit4 <- update(ord.fit3, .~. -x2:x3:x5:x1)
summary(ord.fit4)
drop1(ord.fit4,test = "Chisq")

ord.fit5 <- update(ord.fit4, .~. -x2:x5:x1)
summary(ord.fit5)
drop1(ord.fit5,test = "Chisq")

ord.fit6 <- update(ord.fit5, .~. -x2:x3:x4:x5)
summary(ord.fit6)
drop1(ord.fit6,test = "Chisq")

ord.fit7 <- update(ord.fit6, .~. -x2:x3:x4:x1)
summary(ord.fit7)
drop1(ord.fit7,test = "Chisq")

ord.fit8 <- update(ord.fit7, .~. -x2:x4:x1)
summary(ord.fit8)
drop1(ord.fit8,test = "Chisq")

ord.fit9 <- update(ord.fit8, .~. -x2:x3:x1)
summary(ord.fit9)
drop1(ord.fit9,test = "Chisq")

ord.fit10 <- update(ord.fit9, .~. -x2:x1)
summary(ord.fit10)
drop1(ord.fit10,test = "Chisq")

ord.fit11 <- update(ord.fit10, .~. -x2:x3:x5)
summary(ord.fit11)
drop1(ord.fit11,test = "Chisq")

ord.fit12 <- update(ord.fit11, .~. -x2:x4:x5)
summary(ord.fit12)
drop1(ord.fit12,test = "Chisq")

ord.fit13 <- update(ord.fit12, .~. -x2:x3:x4)
summary(ord.fit13)
drop1(ord.fit13,test = "Chisq")

ord.fit14 <- update(ord.fit13, .~. -x2:x5)
summary(ord.fit14)
drop1(ord.fit14,test = "Chisq") #AIC 282.37

ord.fit15 <- update(ord.fit14, .~. -x3:x4:x5:x1)
summary(ord.fit15)
drop1(ord.fit15,test = "Chisq")

ord.fit16 <- update(ord.fit15, .~. -x4:x5:x1)
summary(ord.fit16)
drop1(ord.fit16,test = "Chisq")

ord.fit17 <- update(ord.fit16, .~. -x3:x4:x1)
summary(ord.fit17)
drop1(ord.fit17,test = "Chisq")

ord.fit18 <- update(ord.fit17, .~. -x3:x5:x1)
summary(ord.fit18)
drop1(ord.fit18,test = "Chisq")

ord.fit19 <- update(ord.fit18, .~. -x3:x1)
summary(ord.fit19)
drop1(ord.fit19,test = "Chisq")

ord.fit20 <- update(ord.fit19, .~. -x3:x4:x5)
summary(ord.fit20)
drop1(ord.fit20,test = "Chisq")

ord.fit21 <- update(ord.fit20, .~. -x3:x4)
summary(ord.fit21)
drop1(ord.fit21,test = "Chisq")

ord.fit22 <- update(ord.fit21, .~. -x4:x5)
summary(ord.fit22)
drop1(ord.fit22,test = "Chisq")

ord.fit23 <- update(ord.fit22, .~. -x4:x1)
summary(ord.fit23) #AIC 276.90
drop1(ord.fit23,test = "Chisq")

ord.fit24 <- update(ord.fit23, .~. -x3:x5)
summary(ord.fit24)
drop1(ord.fit24,test = "Chisq")

ord.fit25 <- update(ord.fit24, .~. -x5:x1)
summary(ord.fit25)
drop1(ord.fit25,test = "Chisq")

ord.fit26 <- update(ord.fit25, .~. -x1)
summary(ord.fit26)
drop1(ord.fit26,test = "Chisq")

ord.fit27 <- update(ord.fit26, .~. -x5)
summary(ord.fit27)
drop1(ord.fit27,test = "Chisq")


