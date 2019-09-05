
#Import Data and subset for the test set:

delay <- fread("delay_discounting_data.csv", header = T)
risky<- delay[,c("userid", "drug_status")]
risky[risky == -1] <- NA
risky<- unique(na.omit(risky))
risky[ ,2] = 4 - risky[ ,2]
names(risky)<- c("userid", "risky")

#merge with the big 5:

risky.big5<- unique(merge(big5.100Q,risky, by="userid"))
dim(risky.big5)

#Take 800 for the test set:

set.seed(1)
size_test <- 800
test_ind <- sample(seq_len(nrow(risky.big5)),
                   size = size_test)
test.risky <- data.table(risky.big5[test_ind, ])
train.risky<- data.table(risky.big5[-test_ind, ])
dim(train.risky)

#Regression model to predict on the all NA data:
risky.train.100<- train.risky[,-c("userid", "O","C","E","A","N")]

risky.model<- lm(risky~.,risky.train.100)

coef(risky.model)
length( coef(risky.model))
summary(risky.model)

