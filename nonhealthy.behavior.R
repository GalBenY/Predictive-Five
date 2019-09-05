
#Import Data and subset for the test set:

delay <- fread("delay_discounting_data.csv", header = T)
nonhealthy<- delay[,c("userid", "smoking_status", "alcohol_status")]

nonhealthy[nonhealthy == -1] <- NA
nonhealthy<- unique( na.omit(nonhealthy))
nonhealthy$nonhealthy<- rowMeans(nonhealthy[,2:3])
nonhealthy[ ,4] = 4 - nonhealthy[ ,4]
nonhealthy.final<- nonhealthy[,c("userid", "nonhealthy")]
 
#merge with the big 5:

nonhealthy.big5<- unique(merge(big5.100Q,nonhealthy.final, by="userid"))
dim(nonhealthy.big5)

#Take 800 for the test set:

set.seed(1)
size_test <- 800
test_ind <- sample(seq_len(nrow(nonhealthy.big5)),
                   size = size_test)
test.nonhealthy <- data.table(nonhealthy.big5[test_ind, ])
train.nonhealthy<- data.table(nonhealthy.big5[-test_ind, ])
dim(train.nonhealthy)

#Regression model:

nonhealthy.train.100<- train.nonhealthy[,-c(
                      "userid", "O","C","E","A","N")]

nonhealthy.model<- lm(nonhealthy~.,nonhealthy.train.100)

coef(nonhealthy.model)
length( coef(nonhealthy.model))
summary(nonhealthy.model)
