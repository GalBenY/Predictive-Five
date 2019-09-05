
#library needed

library(data.table)
library(MASS)
library(glmnet)

#Import Data and subset for the test set:

all.iq <- fread("iq.csv", header = T)

iq.big5<- merge(big5.100Q, all.iq, by="userid")

#Take 800 for the test set:

set.seed(123)
size_test <- 800
test_ind <- sample(seq_len(nrow(iq.big5)),
                   size = size_test, 
                   replace = FALSE)
dim(test.iq <- iq.big5[test_ind, ])
dim(train.iq <- iq.big5[-test_ind, ])


#Regression model to predict on the all NA data:
iq.train.100<- train.iq[,-c("userid", "O","C","E","A","N")]

iq.model<-lm(iq~., data=iq.train.100)

summary(iq.model)
coef(iq.model)
length( coef(iq.model))



