library(VGAM)
CFB2019 <- read.csv("~/OneDrive - SectorShield Inc/2020 Summer/CFB2019-v2.csv")
View(CFB2019) 
attach(CFB2019)

Wins<-factor(CFB2019$Wins)
AvgVars2 <- c("Average.Sacks.per.Game","Avg.Points.per.Game.Allowed","Avg.Turnover.Margin.per.Game","Avg.Yard.per.Kickoff.Return","Avg.Yards.Allowed.per.Punt.Return","Avg.Yards.per.Kickoff.Return.Allowed", "Avg.Yards.Per.Punt.Return","Pass.Yards.Attempt","Pass.Yards.Per.Game","Pass.Yards.Per.Game.Allowed","Penalty.Yards.Per.Game","Points.Per.Game","Redzone.Scores","Rushing.Yards.per.Game","Tackle.For.Loss.Per.Game","X3rd.Percent","X4th.Percent","Yards.Attempt.Allowed","Yards.Completion.Allowed","Yards.Rush")
AvgData2 <- CFB2019[AvgVars2]

CFB2019$WinCategory[CFB2019$Wins==10|CFB2019$Wins==11|CFB2019$Wins==12|CFB2019$Wins==13|CFB2019$Wins==14|CFB2019$Wins==15]=3
CFB2019$WinCategory[CFB2019$Wins==6|CFB2019$Wins==7|CFB2019$Wins==8|CFB2019$Wins==9]= 2
CFB2019$WinCategory[CFB2019$Wins==0|CFB2019$Wins==1|CFB2019$Wins==2|CFB2019$Wins==3|CFB2019$Wins==4|CFB2019$Wins==5]=1
class(CFB2019$WinCategory)

as.numeric(CFB2019$WinCategory)

mult.fit1 = vglm(CFB2019$WinCategory ~ Avg.Points.per.Game.Allowed+Avg.Turnover.Margin.per.Game+Avg.Yard.per.Kickoff.Return, family=multinomial, data = AvgData2)
summary(mult.fit1)
lrtest(mult.fit1)


drop1(mult.fit1, test = "LRT")


library("ordinal")
mult.fit2 = clm(as.factor(CFB2019$WinCategory) ~ ., family=multinomial, data = AvgData2)
summary(mult.fit2)
drop1(mult.fit2, test = "LRT")
