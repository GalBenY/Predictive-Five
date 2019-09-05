
#Import Data and subset for the test set:

all.svq <- fread("svs.csv", header = T)
svq<- all.svq[,-c("date_added", "time_completed", "completed",
                  "section_order","question_order","current_section")]
svq[svq == -1] <- NA
svq[svq == 0] <- NA

svq<- unique(na.omit(svq))

svq$self_transcendence<- rowMeans(svq[,c("q33","q45","q49","q52","q54",
                                         "q1","q17","q24","q26","q29",
                                         "q30","q35","q38")])

svq$self_enhancement<- rowMeans(svq[,c("q34","q39","q43","q55","q3",
                                       "q27","q12","q46")])

svq$openness<- rowMeans(svq[,c("q5","q16","q31","q41","q53","q9","q25",
                               "q37","q4","q50","q57")])

svq$conservation<- rowMeans(svq[,c("q11","q20","q40","q47","q18","q32",
                                   "q36","q44","q51","q8","q13","q15",
                                   "q22","q56")])

svq$sv.openness<- svq[,openness]-svq[,conservation]

svq$sv.trans<- svq[,self_transcendence]-svq[,self_enhancement]

final.svq<- svq[,c("id", "sv.openness", "sv.trans")]
names(final.svq)<- c("userid", "sv.openness", "sv.trans")
final.svq<- unique(final.svq, by= 'userid')

dim(final.svq)

svq.big5<- unique(merge(big5.100Q,final.svq, by="userid"))
dim(svq.big5)
apply(svq.big5[,], 2, function(x) sum(is.na(x)))

#Take 800 for the test set:

set.seed(123)
size_test <- 800
test_ind <- sample(seq_len(nrow(svq.big5)),
                   size = size_test)
test.svq <- svq.big5[test_ind, ]
train.svq<- svq.big5[-test_ind, ]

test.svq.openness<- test.svq[,-"sv.trans"]
test.svq.trans<- test.svq[,-"sv.openness"]

train.svq.openness<- train.svq[,-"sv.trans"]
train.svq.trans<- train.svq[,-"sv.openness"]
apply(train.svq.openness[,], 2, function(x) sum(is.na(x)))

#To examine the correlation between the degree
#of liberalism and the new schwartz variable:

svq.liberal<- merge(train.svq, train.liberal, by="userid")
cor.svq_liberal<-cor(svq.liberal$sv.openness ,svq.liberal$liberalness)

svq_2factors<- svq[,c("id","conservation", "openness")]
names(svq_2factors)<- c("userid","conservation", "openness")
liberal.svq<- merge(svq_2factors, train.liberal, by="userid")
cor.svq_big5<- cor(liberal.svq$openness, liberal.svq$O)

#Regression model to predict on the all NA data:

sv.openness.train.100<- train.svq.openness[,-c(
                      "userid", "O","C","E","A","N")]

openness.model<- lm(sv.openness~.,sv.openness.train.100)

coef(openness.model)
length( coef(openness.model))
summary(openness.model)


################################trans##########################################

sv.trans.train.100<- train.svq.trans[,-c(
                    "userid", "O","C","E","A","N")]

trans.model<- lm(sv.trans~.,sv.trans.train.100)

coef(trans.model)
length( coef(trans.model))
summary(trans.model)

