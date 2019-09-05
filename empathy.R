
#Import Data and subset for the test set:

all.empathy <- fread("eqs.csv", header = T)
empathy<- all.empathy[,c("q1","q6","q19","q22","q25","q26","q35","q36",
                         "q37","q38","q41","q42","q43","q44","q52","q54",
                         "q55","q57","q58","q59","q60",
                         "q4","q8","q10","q11","q12","q14","q15","q18",
                         "q21","q27","q28","q29","q32","q34","q39","q46",
                         "q48","q49","q50","userid")]
empathy[empathy == -1] <- NA
empathy[empathy == 0] <- NA

empathy<- na.omit(empathy)
dim(empathy)

Fun1 <- function(x)  sum((x==4)*2)+sum((x==3))
Fun2 <- function(x)  sum((x==1)*2)+sum((x==2))

empathy$empathy <- 
  empathy[,apply(.SD,1,Fun1),.SDcols=q1:q60]+
empathy[,apply(.SD,1,Fun2),.SDcols=q4:q50]

final.empathy<- empathy[,c("userid", "empathy")]
final.empathy<- unique(final.empathy, by= 'userid')

dim(final.empathy)

empathy.big5<- unique(merge(big5.100Q,final.empathy, by="userid"))

#Take 800 for the test set:

set.seed(1)
size_test <- 800
test_ind <- sample(seq_len(nrow(empathy.big5)),
                   size = size_test)
test.empathy <- data.table(empathy.big5[test_ind, ])
train.empathy<- data.table(empathy.big5[-test_ind, ])
dim(train.empathy)

#Regression model:

empathy.train.100<- train.empathy[,-c("userid", "O","C","E","A","N")]

empathy.model<- lm(empathy~.,empathy.train.100)


coef(empathy.model)
length( coef(empathy.model))
summary(empathy.model)
