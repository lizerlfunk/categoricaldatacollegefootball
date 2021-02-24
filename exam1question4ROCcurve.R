CFB2019 <- read.csv("~/OneDrive - SectorShield Inc/2020 Summer/CFB2019-v2.csv")
View(CFB2019) 
attach(CFB2019)

prob1=predict(model1avg.22,type=c("response"))
CFB2019$prob1=prob1
library(pROC)
g<-roc(WR~prob1,data=CFB2019)
plot(g,print.auc=T)
prob2=predict(probitmodel22,type=c("response"))
CFB2019$prob2=prob2
g2<-roc(WR~prob2,data=CFB2019)
plot(g2,print.auc=T)

