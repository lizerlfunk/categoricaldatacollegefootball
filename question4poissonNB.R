library(VGAM)
CFB2019 <- read.csv("~/OneDrive - SectorShield Inc/2020 Summer/CFB2019-v2.csv")
View(CFB2019) 
attach(CFB2019)

Vars18 <- c("Pass.Yards.Per.Game.Allowed","Penalty.Yards.Per.Game","Avg.Yards.Per.Punt.Return","Avg.Yards.Allowed.per.Punt.Return","Rushing.Yards.per.Game")
Data18 <- CFB2019[Vars18]
Data18$Wins = CFB2019$Wins
names(Data18)[1:5]=c("x1","x2","x3","x4","x5")
CFBpoissonwins.log = glm(Wins ~ x1*x2*x3*x4*x5, family=poisson(link="log"), data = Data18)
summary(CFBpoissonwins.log)

hist(Data18$Wins,
     main="Histogram of Number of Wins",
     xlab="Number of Wins",
     xlim=c(0,16),
     breaks=c(0,2,4,6,8,10,12,14,16)
)

drop1(CFBpoissonwins.log, test = "LRT")

CFBpoissonwins.log2 = glm(Wins ~ x1*x2*x3*x4 + x1*x2*x3*x5 + x1*x2*x4*x5 + x1*x3*x4*x5 + x2*x3*x4*x5, family=poisson(link="log"), data = Data18)
summary(CFBpoissonwins.log2)

drop1(CFBpoissonwins.log2, test = "LRT")

CFBpoissonwins.log3 <- update(CFBpoissonwins.log2, .~. -x1:x2:x3:x5)
summary(CFBpoissonwins.log3)
drop1(CFBpoissonwins.log3, test = "LRT")

CFBpoissonwins.log4 <- update(CFBpoissonwins.log3, .~. -x1:x3:x4:x5)
summary(CFBpoissonwins.log4)
drop1(CFBpoissonwins.log4, test = "LRT")

CFBpoissonwins.log5 <- update(CFBpoissonwins.log4, .~. -x2:x3:x4:x5)
summary(CFBpoissonwins.log5)
drop1(CFBpoissonwins.log5, test = "LRT")

CFBpoissonwins.log6 <- update(CFBpoissonwins.log5, .~. -x2:x3:x5)
summary(CFBpoissonwins.log6)
drop1(CFBpoissonwins.log6, test = "LRT")

CFBpoissonwins.log7 <- update(CFBpoissonwins.log6, .~. -x3:x4:x5)
summary(CFBpoissonwins.log7)
drop1(CFBpoissonwins.log7, test = "LRT")

CFBpoissonwins.log8 <- update(CFBpoissonwins.log7, .~. -x1:x3:x5)
summary(CFBpoissonwins.log8)
drop1(CFBpoissonwins.log8, test = "LRT")

CFBpoissonwins.log9 <- update(CFBpoissonwins.log8, .~. -x3:x5)
summary(CFBpoissonwins.log9)
drop1(CFBpoissonwins.log9, test = "LRT")

CFBpoissonwins.log10 <- update(CFBpoissonwins.log9, .~. -x1:x2:x3:x4)
summary(CFBpoissonwins.log10)
drop1(CFBpoissonwins.log10, test = "LRT")

CFBpoissonwins.log11 <- update(CFBpoissonwins.log10, .~. -x1:x3:x4)
summary(CFBpoissonwins.log11)
drop1(CFBpoissonwins.log11, test = "LRT")

CFBpoissonwins.log12 <- update(CFBpoissonwins.log11, .~. -x2:x3:x4)
summary(CFBpoissonwins.log12)
drop1(CFBpoissonwins.log12, test = "LRT")

CFBpoissonwins.log13 <- update(CFBpoissonwins.log12, .~. -x1:x2:x3)
summary(CFBpoissonwins.log13)
drop1(CFBpoissonwins.log13, test = "LRT")

CFBpoissonwins.log14 <- update(CFBpoissonwins.log13, .~. -x1:x3)
summary(CFBpoissonwins.log14)
drop1(CFBpoissonwins.log14, test = "LRT")

CFBpoissonwins.log15 <- update(CFBpoissonwins.log14, .~. -x2:x3)
summary(CFBpoissonwins.log15)
drop1(CFBpoissonwins.log15, test = "LRT")

CFBpoissonwins.log16 <- update(CFBpoissonwins.log15, .~. -x1:x2:x4:x5)
summary(CFBpoissonwins.log16)
drop1(CFBpoissonwins.log16, test = "LRT")

CFBpoissonwins.log17 <- update(CFBpoissonwins.log16, .~. -x1:x4:x5)
summary(CFBpoissonwins.log17)
drop1(CFBpoissonwins.log17, test = "LRT")

CFBpoissonwins.log18 <- update(CFBpoissonwins.log17, .~. -x1:x2:x4)
summary(CFBpoissonwins.log18)
drop1(CFBpoissonwins.log18, test = "LRT")

CFBpoissonwins.log19 <- update(CFBpoissonwins.log18, .~. -x1:x4)
summary(CFBpoissonwins.log19)
drop1(CFBpoissonwins.log19, test = "LRT")

CFBpoissonwins.log20 <- update(CFBpoissonwins.log19, .~. -x2:x4:x5)
summary(CFBpoissonwins.log20)
drop1(CFBpoissonwins.log20, test = "LRT")

CFBpoissonwins.log21 <- update(CFBpoissonwins.log20, .~. -x4:x5)
summary(CFBpoissonwins.log21)
drop1(CFBpoissonwins.log21, test = "LRT")

CFBpoissonwins.log22 <- update(CFBpoissonwins.log21, .~. -x2:x4)
summary(CFBpoissonwins.log22)
drop1(CFBpoissonwins.log22, test = "LRT")

CFBpoissonwins.log23 <- update(CFBpoissonwins.log22, .~. -x1:x2:x5)
summary(CFBpoissonwins.log23)
drop1(CFBpoissonwins.log23, test = "LRT")

CFBpoissonwins.log24 <- update(CFBpoissonwins.log23, .~. -x1:x5)
summary(CFBpoissonwins.log24)
drop1(CFBpoissonwins.log24, test = "LRT")

CFBpoissonwins.log25 <- update(CFBpoissonwins.log24, .~. -x1:x2)
summary(CFBpoissonwins.log25)
drop1(CFBpoissonwins.log25, test = "LRT")

CFBpoissonwins.log26 <- update(CFBpoissonwins.log25, .~. -x2:x5)
summary(CFBpoissonwins.log26)
drop1(CFBpoissonwins.log26, test = "LRT")

CFBpoissonwins.log27 <- update(CFBpoissonwins.log26, .~. -x2)
summary(CFBpoissonwins.log27)
drop1(CFBpoissonwins.log27, test = "LRT")

deviance(CFBpoissonwins.log27)
df.residual(CFBpoissonwins.log27)
pchisq(151.568, df = 124, lower.tail = FALSE)

#################
#Negative binomial regression

library(MASS)

CFBnbwins = glm.nb(Wins ~ .*.*., data = Data18)
summary(CFBnbwins)
drop1(CFBnbwins, test = "LRT")

CFBnbwins2 = update(CFBnbwins, .~. - x2:x3:x5)
summary(CFBnbwins2)
drop1(CFBnbwins2, test = "LRT")

CFBnbwins3 = update(CFBnbwins2, .~. - x2:x3:x4)
summary(CFBnbwins3)
drop1(CFBnbwins3, test = "LRT")

CFBnbwins4 = update(CFBnbwins3, .~. - x1:x3:x4)
summary(CFBnbwins4)
drop1(CFBnbwins4, test = "LRT")

CFBnbwins5 = update(CFBnbwins4, .~. - x1:x2:x4)
summary(CFBnbwins5)
drop1(CFBnbwins5, test = "LRT")

CFBnbwins6 = update(CFBnbwins5, .~. - x1:x3:x5)
summary(CFBnbwins6)
drop1(CFBnbwins6, test = "LRT")

CFBnbwins7 = update(CFBnbwins6, .~. - 1:x2:x3)
summary(CFBnbwins7)
drop1(CFBnbwins7, test = "LRT")

CFBnbwins8 = update(CFBnbwins7, .~. - x1:x2:x5)
summary(CFBnbwins8)
drop1(CFBnbwins8, test = "LRT")

CFBnbwins9 = update(CFBnbwins8, .~. - x1:x4:x5)
summary(CFBnbwins9)
drop1(CFBnbwins9, test = "LRT")

CFBnbwins10 = update(CFBnbwins9, .~. - x1:x5)
summary(CFBnbwins10)
drop1(CFBnbwins10, test = "LRT")

CFBnbwins11 = update(CFBnbwins10, .~. - x3:x4:x5)
summary(CFBnbwins11)
drop1(CFBnbwins11, test = "LRT")

CFBnbwins12 = update(CFBnbwins11, .~. - x3:x5)
summary(CFBnbwins12)
drop1(CFBnbwins12, test = "LRT")

CFBnbwins13 = update(CFBnbwins12, .~. - x1:x4)
summary(CFBnbwins13)
drop1(CFBnbwins13, test = "LRT")

CFBnbwins14 = update(CFBnbwins13, .~. - x2:x4:x5)
summary(CFBnbwins14)
drop1(CFBnbwins14, test = "LRT")

CFBnbwins15 = update(CFBnbwins14, .~. - x2:x4)
summary(CFBnbwins15)
drop1(CFBnbwins15, test = "LRT")

CFBnbwins16 = update(CFBnbwins15, .~. - x4:x5)
summary(CFBnbwins16)
drop1(CFBnbwins16, test = "LRT")

CFBnbwins17 = update(CFBnbwins16, .~. - x2:x5)
summary(CFBnbwins17)
drop1(CFBnbwins17, test = "LRT")

deviance(CFBnbwins17)
df.residual(CFBnbwins17)
pchisq(145.3922, df=120, lower.tail = FALSE)
