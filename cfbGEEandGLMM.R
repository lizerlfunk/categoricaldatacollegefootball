CFB2019 <- read.csv("~/OneDrive - SectorShield Inc/2020 Summer/CFB2019-v2.csv")
View(CFB2019) 
attach(CFB2019)

CFB2019.df <- as.data.frame(CFB2019)

library(Hmisc)

Vars18 <- c("Pass.Yards.Per.Game.Allowed","Penalty.Yards.Per.Game","Avg.Yards.Per.Punt.Return","Avg.Yards.Allowed.per.Punt.Return","Rushing.Yards.per.Game")
Data18 <- CFB2019[Vars18]
names(Data18)[1:5]=c("x1","x2","x3","x4","x5")



View(Data18)

Data18$WR=CFB2019$Winning.Record
Data18$Conference = CFB2019$Conference
head(Data18)

ids = levels(as.factor(Data18$Conference))
#[1] "AAC" "ACC" "Big 12" "Big Ten" "C-USA"
#[6] "FBS Independent" "MAC" "Mountain West" "Pac-12" "SEC"
#[11] "Sun Belt"

conference_ids = Data18$Conference
for(i in 1:11){
  conference_ids[Data18$Conference==ids[i]]=i
}
Data18$Conference = as.numeric(conference_ids)
gee.fit = gee::gee((WR=="Yes") ~ .*., id=as.numeric(Conference), data=Data18,
                   corstr = "exchangeable", family=binomial)

summary(gee.fit)

2*pnorm(-abs(-2.17162525))
2*pnorm(-abs(-1.98412140))
2*pnorm(-abs(1.97895529))
2*pnorm(-abs(-1.38450121))

prob3=predict(gee.fit,type=c("response"))
CFB2019$prob3=prob3
library(pROC)
WR=CFB2019$Winning.Record
g3<-roc(WR~prob3,data=CFB2019)
plot(g3,print.auc=T)
