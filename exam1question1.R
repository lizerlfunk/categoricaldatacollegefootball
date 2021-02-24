CFB2019 <- read.csv("~/OneDrive - SectorShield Inc/2020 Summer/CFB2019.csv")
View(CFB2019)
CFB2019$Conference<-factor(CFB2019$Conference, c("AAC", "ACC", "Big Ten", "Big 12", "C-USA", "FBS Independent", "MAC", "Mountain West","Pac-12", "SEC", "Sun Belt"))
class(CFB2019$Conference)
class(CFB2019$Wins)
CFB2019$Win.Percentage = CFB2019$Wins / CFB2019$Games
CFB2019$Winning.Record = cut(CFB2019$Win.Percentage,c(0.0,0.5,1.0))
levels(CFB2019$Winning.Record) = c("No","Yes")
CFB2019$Winning.Record = cut(CFB2019$Win.Percentage,c(-0.1,0.5,1.0))
levels(CFB2019$Winning.Record) = c("No","Yes")
class(CFB2019$Winning.Record)
WR<- CFB2019$Winning.Record
Bowl<-CFB2019$BowlQual
Ten<-CFB2019$Ten.Win
Conf<-CFB2019$Conference
Chi1<-chisq.test(WR,Conf)
Chi1
Chi1$observed
Chi2<-chisq.test(Bowl,Conf)
Chi2
Chi2$observed
Chi3<-chisq.test(Ten,Conf)
Chi3
Chi3$observed
CFB2019$P5[Conf == "ACC"|Conf == "SEC"|Conf == "Pac-12"|Conf == "Big 12"|Conf == "Big Ten"]="Yes"
CFB2019$P5[Conf == "AAC"|Conf == "Sun Belt"|Conf == "MAC"|Conf == "C-USA"|Conf == "Mountain West"|Conf == "FBS Independent"]="No"
P5<-CFB2019$P5
Chi4<-chisq.test(WR,P5)
Chi4
Chi4$observed
Chi4$expected
Chi5<-chisq.test(Bowl,P5)
Chi5
Chi5$observed
Chi6<-chisq.test(Ten,P5)
Chi6
Chi6$observed
count1 <- length(which(WR == "Yes"))
count1
count2 <- length(which(WR == "No"))
count2
CFB2019$BowlQual[CFB2019$Wins==6|CFB2019$Wins==7|CFB2019$Wins==8|CFB2019$Wins==9|CFB2019$Wins==10|CFB2019$Wins==11|CFB2019$Wins==12|CFB2019$Wins==13|CFB2019$Wins==14|CFB2019$Wins==15]="Yes"
CFB2019$BowlQual[CFB2019$Wins==0|CFB2019$Wins==1|CFB2019$Wins==2|CFB2019$Wins==3|CFB2019$Wins==4|CFB2019$Wins==5]="No"
CFB2019$TenWin[CFB2019$Wins==10|CFB2019$Wins==11|CFB2019$Wins==12|CFB2019$Wins==13|CFB2019$Wins==14|CFB2019$Wins==15]="Yes"
CFB2019$TenWin[CFB2019$Wins==0|CFB2019$Wins==1|CFB2019$Wins==2|CFB2019$Wins==3|CFB2019$Wins==4|CFB2019$Wins==5|CFB2019$Wins==6|CFB2019$Wins==7|CFB2019$Wins==8|CFB2019$Wins==9]="No"
Bowl<-CFB2019$BowlQual
Chi2<-chisq.test(Bowl,Conf)
Chi2
Chi2$observed
count3 <- length(which(Bowl == "Yes"))
count3
count4 <- length(which(Bowl == "No"))
count4
count5 <- length(which(Ten == "Yes"))
count5
count6 <- length(which(Ten == "No"))
count6
Chi5<-chisq.test(Bowl,P5)
Chi5
Chi5$observed
write.csv(CFB2019, 'CFB2019-v2.csv')
