
#Import Data and subset for the test set:

mfq <- fread("mfq.csv", header = T)
liberal<- mfq[,c("id", "liberalness")]
names(liberal)<- c("userid", "liberalness")
liberal[liberal == 0] <- NA

all.liberal<- na.omit(unique(liberal, by="userid"))
dim(all.liberal)
table(all.liberal$liberalness)

#merge with the big 5:

liberal.big5<-unique(merge(big5.100Q,all.liberal, by="userid"))
dim(liberal.big5)

#Take 800 for the test set:

set.seed(1)
size_test <- 800
test_ind <- sample(seq_len(nrow(liberal.big5)),
                   size = size_test)
test.liberal <- data.table(liberal.big5[test_ind, ])
train.liberal <- data.table(liberal.big5[-test_ind, ])
dim(train.liberal)

#Regression model to predict on the all NA data:

liberal.train.100<- train.liberal[,-c("userid", "O","C","E","A","N")]

liberal.model<- lm(liberalness~.,liberal.train.100)

coef(liberal.model)
length( coef(liberal.model))
summary(liberal.model)

