#The original data for the X's only users:

#library needed:

library(data.table)
library(magrittr)

#Data:

Q_big5_20_100 <- fread("big5_domains_item_level.csv", header = T)
big5_100Q <- Q_big5_20_100[qlength=="100",]
cols100 <- paste('q',1:100, sep='')
big5.100Q<- subset(big5_100Q, 
                   select =c('userid',cols100, "E","A","O","N","C"))
big5.order<- na.omit(setorder(big5.100Q,col=userid))
allq<-as.matrix(big5.order[,q1:q100])
new5.<-allq%*%rotation.mat 
big5.order<- as.data.table(cbind(big5.order,new5.))
dim(big5.order)
all.dupli<-big5.order$userid[duplicated(big5.order$userid)]
dupli<-big5.order[userid%in%all.dupli]
dupli$one<- duplicated(dupli$userid)
first.time<- dupli[one==FALSE]
temp<-dupli[one==TRUE]
temp$two<-duplicated(temp$userid)
second.time<-temp[two==FALSE]

dim(first.time) 
dim(second.time)
head(second.time)
all.equal(first.time$userid,second.time$userid)
cor.new1<-cor(first.time$V1,second.time$V1)
cor.new2<-cor(first.time$V2,second.time$V2)
cor.new3<-cor(first.time$V3,second.time$V3)
cor.new4<-cor(first.time$V4,second.time$V4)
cor.new5<-cor(first.time$V5,second.time$V5)

cor.big1<-cor(first.time$E,second.time$E)
cor.big2<-cor(first.time$A,second.time$A)
cor.big3<-cor(first.time$O,second.time$O)
cor.big4<-cor(first.time$N,second.time$N)
cor.big5<-cor(first.time$C,second.time$C)
