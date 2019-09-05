
#Import Data and subset for the test set:

all.swl <- fread("swl.csv", header = T)
all.swl<- na.omit(unique(all.swl, by='userid'))

dim(all.swl)

swl.big5<- unique(merge(big5.100Q,all.swl, by="userid"))

#Take 800 for the test set:

set.seed(1)
size_test <- 800
test_ind <- sample(seq_len(nrow(swl.big5)),
                   size = size_test)
test.swl <- data.table(swl.big5[test_ind, ])
train.swl <- data.table(swl.big5[-test_ind, ])
dim(train.swl)
head(train.swl)

#Regression model to predict on the all NA data:

swl.train.100<- train.swl[,-c("userid", "O","C","E","A","N")]

swl.model<- lm(swl~.,swl.train.100)


coef(swl.model)
length( coef(swl.model))
summary(swl.model)


