
#Import Data and subset for the test set:

all.cesd <- fread("cesd_item_level.csv", header = T)
all.cesd<- unique(all.cesd, by='userid')
names(all.cesd)
cesd <- all.cesd[,-(2:7)]
cesd[cesd == -1] <- NA
cesd[cesd == 0] <- NA

cesd.noNA <- na.omit(cesd)
sapply(cesd.noNA[,q1:q20], function(x) table(is.na(x)))

cor(cesd.noNA[,-1])
reverse.cols<- c(5, 9, 13,17)
cesd.noNA[ ,reverse.cols] = 5 - cesd.noNA[ ,..reverse.cols]
cesd.noNA$score.cesd<- rowSums(cesd.noNA[,-1])

final.cesd<- cesd.noNA[,c("userid", "score.cesd")]
final.cesd<- unique(final.cesd, by= 'userid')
dim(final.cesd)

#Correlation between swl and cesd variable:

#cesd.swl<- merge(final.cesd, train.swl, by="userid")
#cor.cesd_swl<-cor(cesd.swl$score.cesd, cesd.swl$swl)

######---------------------------------------------------------########

cesd.big5<- unique(merge(big5.100Q,final.cesd, by="userid"))
dim(cesd.big5)

#Take 800 for the test set:

set.seed(1)
size_test <- 800
test_ind <- sample(seq_len(nrow(cesd.big5)),
                   size = size_test)
test.cesd <- data.table(cesd.big5[test_ind, ])
train.cesd <- data.table(cesd.big5[-test_ind, ])
dim(train.cesd)

cesd.train.100<- train.cesd[,-c("userid", "O","C","E","A","N")]

#Regression model:

cesd.model<- lm(score.cesd~.,cesd.train.100)

coef(cesd.model)
length( coef(cesd.model))
summary(cesd.model)
