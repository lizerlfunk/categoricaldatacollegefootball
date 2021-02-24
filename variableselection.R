CFB2019 <- read.csv("~/OneDrive - SectorShield Inc/2020 Summer/CFB2019-v2.csv")
View(CFB2019) 
attach(CFB2019)

library(Hmisc)

CFB2019.df <- as.data.frame(CFB2019)

Vars1 <- c("Off.Yards.per.Game","Yards.Per.Game.Allowed","X4th.Percent","Opponent.4th.Percent","Avg.Yards.per.Kickoff.Return.Allowed","Avg.Yard.per.Kickoff.Return","Pass.Yards.Per.Game","Pass.Yards.Per.Game.Allowed","Penalty.Yards.Per.Game","Avg.Yards.Per.Punt.Return","Avg.Yards.Allowed.per.Punt.Return","Redzone.Points","Redzone.Points.Allowed","Rush.Yards.Per.Game.Allowed","Rushing.Yards.per.Game","Average.Sacks.per.Game","Avg.Points.per.Game.Allowed","Points.Per.Game","Tackle.For.Loss.Per.Game","X3rd.Percent","Avg.Turnover.Margin.per.Game")
Data1 <- CFB2019[Vars1]

write.csv(Vars1,'variablelist.csv')

head(Data1)

avgmatrix1<- cor(Data1, method = "pearson")
data1corr.df <- as.data.frame(avgmatrix1)
write.csv(data1corr.df, "datacorr1.csv")


cor.tests <- rcorr(as.matrix(Data1))
cortests.df <- as.data.frame(cor.tests$P)
write.csv(cortests.df, "cortests.csv")

Vars2 <- c("Off.Yards.per.Game","Yards.Per.Game.Allowed","X4th.Percent","Opponent.4th.Percent","Avg.Yards.per.Kickoff.Return.Allowed","Avg.Yard.per.Kickoff.Return","Pass.Yards.Per.Game","Pass.Yards.Per.Game.Allowed","Penalty.Yards.Per.Game","Avg.Yards.Per.Punt.Return","Avg.Yards.Allowed.per.Punt.Return","Redzone.Points","Redzone.Points.Allowed","Rush.Yards.Per.Game.Allowed","Rushing.Yards.per.Game","Average.Sacks.per.Game","Avg.Points.per.Game.Allowed","Tackle.For.Loss.Per.Game","X3rd.Percent","Avg.Turnover.Margin.per.Game")
Data2 <- CFB2019[Vars2]


avgmatrix2<- cor(Data2, method = "pearson")
data2corr.df <- as.data.frame(avgmatrix2)
write.csv(data2corr.df, "datacorr2.csv")


cor.tests2 <- rcorr(as.matrix(Data2))
cortests2.df <- as.data.frame(cor.tests2$P)
write.csv(cortests2.df, "cortests2.csv")

Vars3 <- c("Off.Yards.per.Game","X4th.Percent","Opponent.4th.Percent","Avg.Yards.per.Kickoff.Return.Allowed","Avg.Yard.per.Kickoff.Return","Pass.Yards.Per.Game","Pass.Yards.Per.Game.Allowed","Penalty.Yards.Per.Game","Avg.Yards.Per.Punt.Return","Avg.Yards.Allowed.per.Punt.Return","Redzone.Points","Redzone.Points.Allowed","Rush.Yards.Per.Game.Allowed","Rushing.Yards.per.Game","Average.Sacks.per.Game","Avg.Points.per.Game.Allowed","Tackle.For.Loss.Per.Game","X3rd.Percent","Avg.Turnover.Margin.per.Game")
Data3 <- CFB2019[Vars3]


avgmatrix3<- cor(Data3, method = "pearson")
data3corr.df <- as.data.frame(avgmatrix3)
write.csv(data3corr.df, "datacorr3.csv")


cor.tests3 <- rcorr(as.matrix(Data3))
cortests3.df <- as.data.frame(cor.tests3$P)
write.csv(cortests3.df, "cortests3.csv")

Vars4 <- c("Off.Yards.per.Game","X4th.Percent","Opponent.4th.Percent","Avg.Yards.per.Kickoff.Return.Allowed","Avg.Yard.per.Kickoff.Return","Pass.Yards.Per.Game","Pass.Yards.Per.Game.Allowed","Penalty.Yards.Per.Game","Avg.Yards.Per.Punt.Return","Avg.Yards.Allowed.per.Punt.Return","Redzone.Points","Redzone.Points.Allowed","Rushing.Yards.per.Game","Average.Sacks.per.Game","Avg.Points.per.Game.Allowed","Tackle.For.Loss.Per.Game","X3rd.Percent","Avg.Turnover.Margin.per.Game")
Data4 <- CFB2019[Vars4]


avgmatrix4<- cor(Data4, method = "pearson")
data4corr.df <- as.data.frame(avgmatrix4)
write.csv(data4corr.df, "datacorr4.csv")


cor.tests4 <- rcorr(as.matrix(Data4))
cortests4.df <- as.data.frame(cor.tests4$P)
write.csv(cortests4.df, "cortests4.csv")

Vars5 <- c("Off.Yards.per.Game","X4th.Percent","Opponent.4th.Percent","Avg.Yards.per.Kickoff.Return.Allowed","Avg.Yard.per.Kickoff.Return","Pass.Yards.Per.Game","Pass.Yards.Per.Game.Allowed","Penalty.Yards.Per.Game","Avg.Yards.Per.Punt.Return","Avg.Yards.Allowed.per.Punt.Return","Redzone.Points","Redzone.Points.Allowed","Rushing.Yards.per.Game","Average.Sacks.per.Game","Avg.Points.per.Game.Allowed","Tackle.For.Loss.Per.Game","X3rd.Percent","Avg.Turnover.Margin.per.Game")
Data5 <- CFB2019[Vars5]


avgmatrix5<- cor(Data5, method = "pearson")
data5corr.df <- as.data.frame(avgmatrix5)
write.csv(data5corr.df, "datacorr5.csv")


cor.tests5 <- rcorr(as.matrix(Data5))
cortests5.df <- as.data.frame(cor.tests5$P)
write.csv(cortests5.df, "cortests5.csv")

Vars6 <- c("Off.Yards.per.Game","X4th.Percent","Opponent.4th.Percent","Avg.Yards.per.Kickoff.Return.Allowed","Avg.Yard.per.Kickoff.Return","Pass.Yards.Per.Game","Pass.Yards.Per.Game.Allowed","Penalty.Yards.Per.Game","Avg.Yards.Per.Punt.Return","Avg.Yards.Allowed.per.Punt.Return","Redzone.Points","Redzone.Points.Allowed","Rushing.Yards.per.Game","Average.Sacks.per.Game","Tackle.For.Loss.Per.Game","X3rd.Percent","Avg.Turnover.Margin.per.Game")
Data6 <- CFB2019[Vars6]


avgmatrix6<- cor(Data6, method = "pearson")
data6corr.df <- as.data.frame(avgmatrix6)
write.csv(data6corr.df, "datacorr6.csv")


cor.tests6 <- rcorr(as.matrix(Data6))
cortests6.df <- as.data.frame(cor.tests6$P)
write.csv(cortests6.df, "cortests6.csv")

Vars7 <- c("X4th.Percent","Opponent.4th.Percent","Avg.Yards.per.Kickoff.Return.Allowed","Avg.Yard.per.Kickoff.Return","Pass.Yards.Per.Game","Pass.Yards.Per.Game.Allowed","Penalty.Yards.Per.Game","Avg.Yards.Per.Punt.Return","Avg.Yards.Allowed.per.Punt.Return","Redzone.Points","Redzone.Points.Allowed","Rushing.Yards.per.Game","Average.Sacks.per.Game","Tackle.For.Loss.Per.Game","X3rd.Percent","Avg.Turnover.Margin.per.Game")
Data7 <- CFB2019[Vars7]


avgmatrix7<- cor(Data7, method = "pearson")
data7corr.df <- as.data.frame(avgmatrix7)
write.csv(data7corr.df, "datacorr7.csv")


cor.tests7 <- rcorr(as.matrix(Data7))
cortests7.df <- as.data.frame(cor.tests7$P)
write.csv(cortests7.df, "cortests7.csv")

Vars8 <- c("X4th.Percent","Opponent.4th.Percent","Avg.Yards.per.Kickoff.Return.Allowed","Avg.Yard.per.Kickoff.Return","Pass.Yards.Per.Game","Pass.Yards.Per.Game.Allowed","Penalty.Yards.Per.Game","Avg.Yards.Per.Punt.Return","Avg.Yards.Allowed.per.Punt.Return","Redzone.Points","Redzone.Points.Allowed","Rushing.Yards.per.Game","Average.Sacks.per.Game","Tackle.For.Loss.Per.Game","Avg.Turnover.Margin.per.Game")
Data8 <- CFB2019[Vars8]


avgmatrix8<- cor(Data8, method = "pearson")
data8corr.df <- as.data.frame(avgmatrix8)
write.csv(data8corr.df, "datacorr8.csv")


cor.tests8 <- rcorr(as.matrix(Data8))
cortests8.df <- as.data.frame(cor.tests8$P)
write.csv(cortests8.df, "cortests8.csv")

Vars9 <- c("X4th.Percent","Opponent.4th.Percent","Avg.Yards.per.Kickoff.Return.Allowed","Avg.Yard.per.Kickoff.Return","Pass.Yards.Per.Game","Pass.Yards.Per.Game.Allowed","Penalty.Yards.Per.Game","Avg.Yards.Per.Punt.Return","Avg.Yards.Allowed.per.Punt.Return","Redzone.Points.Allowed","Rushing.Yards.per.Game","Average.Sacks.per.Game","Tackle.For.Loss.Per.Game","Avg.Turnover.Margin.per.Game")
Data9 <- CFB2019[Vars9]


avgmatrix9<- cor(Data9, method = "pearson")
data9corr.df <- as.data.frame(avgmatrix9)
write.csv(data9corr.df, "datacorr9.csv")


cor.tests9 <- rcorr(as.matrix(Data9))
cortests9.df <- as.data.frame(cor.tests9$P)
write.csv(cortests9.df, "cortests9.csv")

Vars10 <- c("X4th.Percent","Opponent.4th.Percent","Avg.Yards.per.Kickoff.Return.Allowed","Avg.Yard.per.Kickoff.Return","Pass.Yards.Per.Game.Allowed","Penalty.Yards.Per.Game","Avg.Yards.Per.Punt.Return","Avg.Yards.Allowed.per.Punt.Return","Redzone.Points.Allowed","Rushing.Yards.per.Game","Average.Sacks.per.Game","Tackle.For.Loss.Per.Game","Avg.Turnover.Margin.per.Game")
Data10 <- CFB2019[Vars10]


avgmatrix10<- cor(Data10, method = "pearson")
data10corr.df <- as.data.frame(avgmatrix10)
write.csv(data10corr.df, "datacorr10.csv")


cor.tests10 <- rcorr(as.matrix(Data10))
cortests10.df <- as.data.frame(cor.tests10$P)
write.csv(cortests10.df, "cortests10.csv")

Vars11 <- c("X4th.Percent","Opponent.4th.Percent","Avg.Yards.per.Kickoff.Return.Allowed","Avg.Yard.per.Kickoff.Return","Pass.Yards.Per.Game.Allowed","Penalty.Yards.Per.Game","Avg.Yards.Per.Punt.Return","Avg.Yards.Allowed.per.Punt.Return","Rushing.Yards.per.Game","Average.Sacks.per.Game","Tackle.For.Loss.Per.Game","Avg.Turnover.Margin.per.Game")
Data11 <- CFB2019[Vars11]


avgmatrix11<- cor(Data11, method = "pearson")
data11corr.df <- as.data.frame(avgmatrix11)
write.csv(data11corr.df, "datacorr11.csv")


cor.tests11 <- rcorr(as.matrix(Data11))
cortests11.df <- as.data.frame(cor.tests11$P)
write.csv(cortests11.df, "cortests11.csv")

Vars12 <- c("Opponent.4th.Percent","Avg.Yards.per.Kickoff.Return.Allowed","Avg.Yard.per.Kickoff.Return","Pass.Yards.Per.Game.Allowed","Penalty.Yards.Per.Game","Avg.Yards.Per.Punt.Return","Avg.Yards.Allowed.per.Punt.Return","Rushing.Yards.per.Game","Average.Sacks.per.Game","Tackle.For.Loss.Per.Game","Avg.Turnover.Margin.per.Game")
Data12 <- CFB2019[Vars12]


avgmatrix12<- cor(Data12, method = "pearson")
data12corr.df <- as.data.frame(avgmatrix12)
write.csv(data12corr.df, "datacorr12.csv")


cor.tests12 <- rcorr(as.matrix(Data12))
cortests12.df <- as.data.frame(cor.tests12$P)
write.csv(cortests12.df, "cortests12.csv")

Vars13 <- c("Opponent.4th.Percent","Avg.Yards.per.Kickoff.Return.Allowed","Avg.Yard.per.Kickoff.Return","Pass.Yards.Per.Game.Allowed","Penalty.Yards.Per.Game","Avg.Yards.Per.Punt.Return","Avg.Yards.Allowed.per.Punt.Return","Rushing.Yards.per.Game","Tackle.For.Loss.Per.Game","Avg.Turnover.Margin.per.Game")
Data13 <- CFB2019[Vars13]


avgmatrix13<- cor(Data13, method = "pearson")
data13corr.df <- as.data.frame(avgmatrix13)
write.csv(data13corr.df, "datacorr13.csv")


cor.tests13 <- rcorr(as.matrix(Data13))
cortests13.df <- as.data.frame(cor.tests13$P)
write.csv(cortests13.df, "cortests13.csv")

Vars14 <- c("Opponent.4th.Percent","Avg.Yards.per.Kickoff.Return.Allowed","Avg.Yard.per.Kickoff.Return","Pass.Yards.Per.Game.Allowed","Penalty.Yards.Per.Game","Avg.Yards.Per.Punt.Return","Avg.Yards.Allowed.per.Punt.Return","Rushing.Yards.per.Game","Tackle.For.Loss.Per.Game")
Data14 <- CFB2019[Vars14]


avgmatrix14<- cor(Data14, method = "pearson")
data14corr.df <- as.data.frame(avgmatrix14)
write.csv(data14corr.df, "datacorr14.csv")


cor.tests14 <- rcorr(as.matrix(Data14))
cortests14.df <- as.data.frame(cor.tests14$P)
write.csv(cortests14.df, "cortests14.csv")

Vars15 <- c("Avg.Yards.per.Kickoff.Return.Allowed","Avg.Yard.per.Kickoff.Return","Pass.Yards.Per.Game.Allowed","Penalty.Yards.Per.Game","Avg.Yards.Per.Punt.Return","Avg.Yards.Allowed.per.Punt.Return","Rushing.Yards.per.Game","Tackle.For.Loss.Per.Game")
Data15 <- CFB2019[Vars15]


avgmatrix15<- cor(Data15, method = "pearson")
data15corr.df <- as.data.frame(avgmatrix15)
write.csv(data15corr.df, "datacorr15.csv")


cor.tests15 <- rcorr(as.matrix(Data15))
cortests15.df <- as.data.frame(cor.tests15$P)
write.csv(cortests15.df, "cortests15.csv")

Vars16 <- c("Avg.Yards.per.Kickoff.Return.Allowed","Avg.Yard.per.Kickoff.Return","Pass.Yards.Per.Game.Allowed","Penalty.Yards.Per.Game","Avg.Yards.Per.Punt.Return","Avg.Yards.Allowed.per.Punt.Return","Rushing.Yards.per.Game")
Data16 <- CFB2019[Vars16]


avgmatrix16<- cor(Data16, method = "pearson")
data16corr.df <- as.data.frame(avgmatrix16)
write.csv(data16corr.df, "datacorr16.csv")


cor.tests16 <- rcorr(as.matrix(Data16))
cortests16.df <- as.data.frame(cor.tests16$P)
write.csv(cortests16.df, "cortests16.csv")

Vars17 <- c("Avg.Yard.per.Kickoff.Return","Pass.Yards.Per.Game.Allowed","Penalty.Yards.Per.Game","Avg.Yards.Per.Punt.Return","Avg.Yards.Allowed.per.Punt.Return","Rushing.Yards.per.Game")
Data17 <- CFB2019[Vars17]


avgmatrix17<- cor(Data17, method = "pearson")
data17corr.df <- as.data.frame(avgmatrix17)
write.csv(data17corr.df, "datacorr17.csv")


cor.tests17 <- rcorr(as.matrix(Data17))
cortests17.df <- as.data.frame(cor.tests17$P)
write.csv(cortests17.df, "cortests17.csv")

Vars18 <- c("Pass.Yards.Per.Game.Allowed","Penalty.Yards.Per.Game","Avg.Yards.Per.Punt.Return","Avg.Yards.Allowed.per.Punt.Return","Rushing.Yards.per.Game")
Data18 <- CFB2019[Vars18]


avgmatrix18<- cor(Data18, method = "pearson")
data18corr.df <- as.data.frame(avgmatrix18)
write.csv(data18corr.df, "datacorr18.csv")


cor.tests18 <- rcorr(as.matrix(Data18))
cortests18.df <- as.data.frame(cor.tests18$P)
write.csv(cortests18.df, "cortests18.csv")

View(Vars18)
