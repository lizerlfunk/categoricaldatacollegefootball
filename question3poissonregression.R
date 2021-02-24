library(VGAM)
CFB2019 <- read.csv("~/OneDrive - SectorShield Inc/2020 Summer/CFB2019-v2.csv")
View(CFB2019) 
attach(CFB2019)

Vars18 <- c("Pass.Yards.Per.Game.Allowed","Penalty.Yards.Per.Game","Avg.Yards.Per.Punt.Return","Avg.Yards.Allowed.per.Punt.Return","Rushing.Yards.per.Game")
Data18 <- CFB2019[Vars18]
Data18$WR = CFB2019$Winning.Record

summary(Data18)

Data18$x1.cat = cut(Pass.Yards.Per.Game.Allowed, breaks = c(155,226.9,321.4))
table(Data18$x1.cat)

Data18$x2.cat = cut(Penalty.Yards.Per.Game, breaks = c(27, 52.42, 79.08))
table(Data18$x2.cat)

Data18$x3.cat = cut(Avg.Yards.Per.Punt.Return, breaks = c(0, 8.0, 25))
table(Data18$x3.cat)

Data18$x4.cat = cut(Avg.Yards.Allowed.per.Punt.Return, breaks = c(-1, 7.530, 17.290))
table(Data18$x4.cat)

Data18$x5.cat = cut(Rushing.Yards.per.Game, breaks = c(47, 161.6, 360.5))
table(Data18$x5.cat)

Data18 <- as.data.frame(Data18)

library(plyr)
Data18$x1.cat <- revalue(Data18$x1.cat, c("(155,227]"="low", "(227,321]"="high"))

Data18$x2.cat <- revalue(Data18$x2.cat, c("(27,52.4]"="low", "(52.4,79.1]"="high"))

Data18$x3.cat <- revalue(Data18$x3.cat, c("(0,8]"="low","(8,25]"="high"))

Data18$x4.cat <- revalue(Data18$x4.cat, c("(-1,7.53]"="low","(7.53,17.3]"="high"))

Data18$x5.cat <- revalue(Data18$x5.cat, c("(47,162]"="low","(162,360]"="high"))

Data18.table <- table(Data18$x1.cat, Data18$x2.cat, Data18$x3.cat, Data18$x4.cat, Data18$x5.cat, Data18$WR, dnn=c("x1","x2","x3","x4","x5","WR"))
Data18.table
ftable(Data18.table, row.vars = c("x1", "x2","x3","x4","x5","WR"))

Data18.table <- as.table(Data18.table)
Data18.df <- as.data.frame(Data18.table)



CFBpoisson.indep <- glm(Freq ~ WR + x1 + x2 + x3 + x4 +x5, family = poisson, data = Data18.df)
summary(CFBpoisson.indep)

CFBpoisson.homog <- glm(Freq ~ .*., family = poisson, data = Data18.df)
summary(CFBpoisson.homog)

CFBpoisson.3way <- glm(Freq ~ .*.*., family = poisson, data = Data18.df)
summary(CFBpoisson.3way)
drop1(CFBpoisson.3way, test = "LRT")

CFBpoisson.3way2 <- update(CFBpoisson.3way, .~. -x1:x2:WR)
summary(CFBpoisson.3way2)
drop1(CFBpoisson.3way2, test = "LRT")

CFBpoisson.3way3 <- update(CFBpoisson.3way2, .~. -x2:x4:x5)
summary(CFBpoisson.3way3)
drop1(CFBpoisson.3way3, test = "LRT")

CFBpoisson.3way4 <- update(CFBpoisson.3way3, .~. -x2:x4:WR)
summary(CFBpoisson.3way4)
drop1(CFBpoisson.3way4, test = "LRT")

CFBpoisson.3way5 <- update(CFBpoisson.3way4, .~. -x2:x3:WR)
summary(CFBpoisson.3way5)
drop1(CFBpoisson.3way5, test = "LRT")

CFBpoisson.3way6 <- update(CFBpoisson.3way5, .~. -x1:x5:WR)
summary(CFBpoisson.3way6)
drop1(CFBpoisson.3way6, test = "LRT")

CFBpoisson.3way7 <- update(CFBpoisson.3way6, .~. -x2:x3:x4)
summary(CFBpoisson.3way7)
drop1(CFBpoisson.3way7, test = "LRT")

CFBpoisson.3way8 <- update(CFBpoisson.3way7, .~. -x1:x2:x4)
summary(CFBpoisson.3way8)
drop1(CFBpoisson.3way8, test = "LRT")

CFBpoisson.3way9 <- update(CFBpoisson.3way8, .~. -x1:x4:x5)
summary(CFBpoisson.3way9)
drop1(CFBpoisson.3way9, test = "LRT")

CFBpoisson.3way10 <- update(CFBpoisson.3way9, .~. -x2:x5:WR)
summary(CFBpoisson.3way10)
drop1(CFBpoisson.3way10, test = "LRT")

CFBpoisson.3way11 <- update(CFBpoisson.3way10, .~. -x1:x4:WR)
summary(CFBpoisson.3way11)
drop1(CFBpoisson.3way11, test = "LRT")

CFBpoisson.3way12 <- update(CFBpoisson.3way11, .~. -x3:x4:WR)
summary(CFBpoisson.3way12)
drop1(CFBpoisson.3way12, test = "LRT")

CFBpoisson.3way13 <- update(CFBpoisson.3way12, .~. -x2:x3:x5)
summary(CFBpoisson.3way13)
drop1(CFBpoisson.3way13, test = "LRT")

CFBpoisson.3way14 <- update(CFBpoisson.3way13, .~. -x2:WR)
summary(CFBpoisson.3way14)
drop1(CFBpoisson.3way14, test = "LRT")

CFBpoisson.3way15 <- update(CFBpoisson.3way14, .~. -x1:x2:x5)
summary(CFBpoisson.3way15)
drop1(CFBpoisson.3way15, test = "LRT")

CFBpoisson.3way16 <- update(CFBpoisson.3way15, .~. -x2:x4)
summary(CFBpoisson.3way16)
drop1(CFBpoisson.3way16, test = "LRT")

CFBpoisson.3way17 <- update(CFBpoisson.3way16, .~. -x4:x5:WR)
summary(CFBpoisson.3way17)
drop1(CFBpoisson.3way17, test = "LRT")

CFBpoisson.3way18 <- update(CFBpoisson.3way17, .~. -x1:x2:x3)
summary(CFBpoisson.3way18)
drop1(CFBpoisson.3way18, test = "LRT")

CFBpoisson.3way19 <- update(CFBpoisson.3way18, .~. -x1:x2)
summary(CFBpoisson.3way19)
drop1(CFBpoisson.3way19, test = "LRT")

CFBpoisson.3way20 <- update(CFBpoisson.3way19, .~. -x2:x3)
summary(CFBpoisson.3way20)
drop1(CFBpoisson.3way20, test = "LRT")

CFBpoisson.3way21 <- update(CFBpoisson.3way20, .~. -x2:x5)
summary(CFBpoisson.3way21)
drop1(CFBpoisson.3way21, test = "LRT")

CFBpoisson.3way22 <- update(CFBpoisson.3way21, .~. -x2)
summary(CFBpoisson.3way22)
drop1(CFBpoisson.3way22, test = "LRT")

CFBpoisson.3way23 <- update(CFBpoisson.3way22, .~. -x3:x5:WR)
summary(CFBpoisson.3way23)
drop1(CFBpoisson.3way23, test = "LRT")

CFBpoisson.3way24 <- update(CFBpoisson.3way23, .~. -x4:WR)
summary(CFBpoisson.3way24)
drop1(CFBpoisson.3way24, test = "LRT")

CFBpoisson.3way25 <- update(CFBpoisson.3way24, .~. -x1:x3:WR)
summary(CFBpoisson.3way25)
drop1(CFBpoisson.3way25, test = "LRT")

CFBpoisson.3way26 <- update(CFBpoisson.3way25, .~. -x3:WR)
summary(CFBpoisson.3way26)
drop1(CFBpoisson.3way26, test = "LRT")

deviance(CFBpoisson.3way26)
df.residual(CFBpoisson.3way26)
pchisq(39.70012, df=47)
