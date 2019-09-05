
#New Mturk Data:

new.data<- fread("new.mturk.reverse.csv", header = T)
names(new.data)
new.data.100<- na.omit (new.data)
apply(new.data.100,c(1,2), is.na)
mean(new.data.100$age)
dim(new.data.100)
table(new.data.100$race)
#Create a rotation matrix from the RRR model:
rotation.mat <- rrpack.model$coef.ls %*% rrpack.model$A

new.data.100$O<- rowMeans(new.data.100[,c("q1","q4","q7","q16","q21","q24",
                                  "q31","q34","q41","q44","q51","q54",
                                  "q61","q64","q71","q74","q81","q84",
                                  "q91","q94")])

new.data.100$C<- rowMeans(new.data.100[,c("q5","q8","q15","q20","q25","q28",
                                  "q35","q38","q45","q48","q55","q58",
                                  "q65","q68","q75","q78","q85","q88",
                                  "q95","q98")])
                               
new.data.100$E<- rowMeans(new.data.100[,c("q3","q10","q14","q18","q23","q29",
                                  "q33","q39","q43","q49","q53","q59",
                                  "q63","q69","q73","q79","q83","q89",
                                  "q93","q99")])
                               
new.data.100$A<- rowMeans(new.data.100[,c("q2","q6","q9","q13","q22","q26",
                                  "q32","q36","q42","q46","q52","q56",
                                  "q62","q66","q72","q76","q82","q86",
                                  "q92","q96")])

new.data.100$N<- rowMeans(new.data.100[,c("q11","q12","q17","q19","q27","q30",
                                  "q37","q40","q47","q50","q57","q60",
                                  "q67","q70","q77","q80","q87","q90",
                                  "q97","q100")])

#Create the five new factors using a rotation matrix,
#and add them to the original table:

new.data.100<- na.omit(new.data.100)
only.100<- as.matrix(new.data.100[,q1:q100])
new5<- only.100 %*% as.matrix(rotation.mat)
all.new<-  as.data.table(cbind(new.data.100, new5))
dim(all.new)


#Division for train and test set:

set.seed(26)
size_test <- 150
test_ind <- sample(seq_len(nrow(all.new)),
                   size = size_test)
test.new <- data.table(all.new[test_ind, ])
dim(train.new)

train.new <- data.table(all.new[-test_ind, ])
dim(train.new)
train.only100<- train.new[,q1:q100]
test.only100<- test.new[,q1:q100]

#####################################################################

#Linear models New5:

##################################DV1:################################

#-----------------------------New5-----------------------------------#

model.dv1.new5 <- lm(DV.1~ V1+V2+V3+V4+V5, train.new)
summary(model.dv1.new5)

pred.dv1.new5.train<- predict(model.dv1.new5, newdata =train.new )
r2.dv1.new5.train<- R2(train.new$DV.1, pred.dv1.new5.train)

pred.dv1.new5.test<- predict(model.dv1.new5, newdata =test.new )
r2.dv1.new5.test<- R2(test.new$DV.1, pred.dv1.new5.test)

#---------------------------------Big5-------------------------------#

model.dv1.big5 <- lm(DV.1~ O+C+E+A+N, train.new)
summary(model.dv1.big)

pred.dv1.big5.train<- predict(model.dv1.big5, newdata =train.new )
r2.dv1.big5.train<- R2(train.new$DV.1, pred.dv1.big5.train)

pred.dv1.big5.test<- predict(model.dv1.big5, newdata =test.new )
r2.dv1.big5.test<- R2(test.new$DV.1, pred.dv1.big5.test)

#------------------------------------------------------------------#

dv1.train.only100<- data.table(train.only100, DV.1= train.new$DV.1)
dv1.test.only100<- data.table(test.only100, DV.1= test.new$DV.1)

model.dv1.100 <- lm(DV.1~., dv1.train.only100)
summary(model.dv1.100)

pred.dv1.100.test<- predict(model.dv1.100, newdata =dv1.test.only100 )
r2.dv1.100.test<- R2(dv1.test.only100$DV.1, pred.dv1.100.test)

pred.dv1.100.train<- predict(model.dv1.100, newdata =dv1.train.only100 )
r2.dv1.100.train<- R2(dv1.train.only100$DV.1, pred.dv1.100.train)


##################################DV2:################################


#-----------------------------New5-----------------------------------#

model.dv2.new5 <- lm(DV.2~ V1+V2+V3+V4+V5, train.new)
summary(model.dv2.new5)

pred.dv2.new5.train<- predict(model.dv2.new5, newdata =train.new )
r2.dv2.new5.train<- R2(train.new$DV.2, pred.dv2.new5.train)

pred.dv2.new5.test<- predict(model.dv2.new5, newdata =test.new )
r2.dv2.new5.test<- R2(test.new$DV.2, pred.dv2.new5.test)

#---------------------------------Big5-------------------------------#

model.dv2.big5 <- lm(DV.2~ O+C+E+A+N, train.new)
summary(model.dv2.big)

pred.dv2.big5.train<- predict(model.dv2.big5, newdata =train.new )
r2.dv2.big5.train<- R2(train.new$DV.2, pred.dv2.big5.train)

pred.dv2.big5.test<- predict(model.dv2.big5, newdata =test.new )
r2.dv2.big5.test<- R2(test.new$DV.2, pred.dv2.big5.test)

#----------------------------------100Q--------------------------------#

dv2.train.only100<- data.table(train.only100, DV.2= train.new$DV.2)
dv2.test.only100<- data.table(test.only100, DV.2= test.new$DV.2)

model.dv2.100 <- lm(DV.2~., dv2.train.only100)
summary(model.dv2.100)

pred.dv2.100.test<- predict(model.dv2.100, newdata =dv2.test.only100 )
r2.dv2.100.test<- R2(dv2.test.only100$DV.2, pred.dv2.100.test)

pred.dv2.100.train<- predict(model.dv2.100, newdata =dv2.train.only100 )
r2.dv2.100.train<- R2(dv2.train.only100$DV.2, pred.dv2.100.train)

##################################DV3:################################

#-----------------------------New5-----------------------------------#

model.dv3.new5 <- lm(DV.3~ V1+V2+V3+V4+V5, train.new)
summary(model.dv3.new5)

pred.dv3.new5.train<- predict(model.dv3.new5, newdata =train.new )
r2.dv3.new5.train<- R2(train.new$DV.3, pred.dv3.new5.train)

pred.dv3.new5.test<- predict(model.dv3.new5, newdata =test.new )
r2.dv3.new5.test<- R2(test.new$DV.3, pred.dv3.new5.test)

#---------------------------------Big5-------------------------------#

model.dv3.big5 <- lm(DV.3~ O+C+E+A+N, train.new)
summary(model.dv3.big)

pred.dv3.big5.train<- predict(model.dv3.big5, newdata =train.new )
r2.dv3.big5.train<- R2(train.new$DV.3, pred.dv3.big5.train)

pred.dv3.big5.test<- predict(model.dv3.big5, newdata =test.new )
r2.dv3.big5.test<- R2(test.new$DV.3, pred.dv3.big5.test)

#----------------------------------100Q--------------------------------#

dv3.train.only100<- data.table(train.only100, DV.3= train.new$DV.3)
dv3.test.only100<- data.table(test.only100, DV.3= test.new$DV.3)

model.dv3.100 <- lm(DV.3~., dv3.train.only100)
summary(model.dv3.100)

pred.dv3.100.test<- predict(model.dv3.100, newdata =dv3.test.only100 )
r2.dv3.100.test<- R2(dv3.test.only100$DV.3, pred.dv3.100.test)

pred.dv3.100.train<- predict(model.dv3.100, newdata =dv3.train.only100 )
r2.dv3.100.train<- R2(dv3.train.only100$DV.3, pred.dv3.100.train)

##################################DV4:################################

#-----------------------------New5-----------------------------------#

model.dv4.new5 <- lm(DV.4~ V1+V2+V3+V4+V5, train.new)
summary(model.dv4.new5)

pred.dv4.new5.train<- predict(model.dv4.new5, newdata =train.new )
r2.dv4.new5.train<- R2(train.new$DV.4, pred.dv4.new5.train)

pred.dv4.new5.test<- predict(model.dv4.new5, newdata =test.new )
r2.dv4.new5.test<- R2(test.new$DV.4, pred.dv4.new5.test)

#---------------------------------Big5-------------------------------#

model.dv4.big5 <- lm(DV.4~ O+C+E+A+N, train.new)
summary(model.dv4.big)

pred.dv4.big5.train<- predict(model.dv4.big5, newdata =train.new )
r2.dv4.big5.train<- R2(train.new$DV.4, pred.dv4.big5.train)

pred.dv4.big5.test<- predict(model.dv4.big5, newdata =test.new )
r2.dv4.big5.test<- R2(test.new$DV.4, pred.dv4.big5.test)

#----------------------------------100Q--------------------------------#

dv4.train.only100<- data.table(train.only100, DV.4= train.new$DV.4)
dv4.test.only100<- data.table(test.only100, DV.4= test.new$DV.4)

model.dv4.100 <- lm(DV.4~., dv4.train.only100)
summary(model.dv4.100)

pred.dv4.100.test<- predict(model.dv4.100, newdata =dv4.test.only100 )
r2.dv4.100.test<- R2(dv4.test.only100$DV.4, pred.dv4.100.test)

pred.dv4.100.train<- predict(model.dv4.100, newdata =dv4.train.only100 )
r2.dv4.100.train<- R2(dv4.train.only100$DV.4, pred.dv4.100.train)

##################################DV5:################################

#-----------------------------New5-----------------------------------#

model.dv5.new5 <- lm(DV.5~ V1+V2+V3+V4+V5, train.new)
summary(model.dv5.new5)

pred.dv5.new5.train<- predict(model.dv5.new5, newdata =train.new )
r2.dv5.new5.train<- R2(train.new$DV.5, pred.dv5.new5.train)

pred.dv5.new5.test<- predict(model.dv5.new5, newdata =test.new )
r2.dv5.new5.test<- R2(test.new$DV.5, pred.dv5.new5.test)

#---------------------------------Big5-------------------------------#

model.dv5.big5 <- lm(DV.5~ O+C+E+A+N, train.new)
summary(model.dv5.big)

pred.dv5.big5.train<- predict(model.dv5.big5, newdata =train.new )
r2.dv5.big5.train<- R2(train.new$DV.5, pred.dv5.big5.train)

pred.dv5.big5.test<- predict(model.dv5.big5, newdata =test.new )
r2.dv5.big5.test<- R2(test.new$DV.5, pred.dv5.big5.test)

#----------------------------------100Q--------------------------------#

dv5.train.only100<- data.table(train.only100, DV.5= train.new$DV.5)
dv5.test.only100<- data.table(test.only100, DV.5= test.new$DV.5)

model.dv5.100 <- lm(DV.5~., dv5.train.only100)
summary(model.dv5.100)

pred.dv5.100.test<- predict(model.dv5.100, newdata =dv5.test.only100 )
r2.dv5.100.test<- R2(dv5.test.only100$DV.5, pred.dv5.100.test)

pred.dv5.100.train<- predict(model.dv5.100, newdata =dv5.train.only100 )
r2.dv5.100.train<- R2(dv5.train.only100$DV.5, pred.dv5.100.train)

##################################DV6:################################

#-----------------------------New5-----------------------------------#

model.dv6.new5 <- lm(DV.6~ V1+V2+V3+V4+V5, train.new)
summary(model.dv6.new5)

pred.dv6.new5.train<- predict(model.dv6.new5, newdata =train.new )
r2.dv6.new5.train<- R2(train.new$DV.6, pred.dv6.new5.train)

pred.dv6.new5.test<- predict(model.dv6.new5, newdata =test.new )
r2.dv6.new5.test<- R2(test.new$DV.6, pred.dv6.new5.test)

#---------------------------------Big5-------------------------------#

model.dv6.big5 <- lm(DV.6~ O+C+E+A+N, train.new)
summary(model.dv6.big)

pred.dv6.big5.train<- predict(model.dv6.big5, newdata =train.new )
r2.dv6.big5.train<- R2(train.new$DV.6, pred.dv6.big5.train)

pred.dv6.big5.test<- predict(model.dv6.big5, newdata =test.new )
r2.dv6.big5.test<- R2(test.new$DV.6, pred.dv6.big5.test)

#----------------------------------100Q--------------------------------#

dv6.train.only100<- data.table(train.only100, DV.6= train.new$DV.6)
dv6.test.only100<- data.table(test.only100, DV.6= test.new$DV.6)

model.dv6.100 <- lm(DV.6~., dv6.train.only100)
summary(model.dv6.100)

pred.dv6.100.test<- predict(model.dv6.100, newdata =dv6.test.only100 )
r2.dv6.100.test<- R2(dv6.test.only100$DV.6, pred.dv6.100.test)

pred.dv6.100.train<- predict(model.dv6.100, newdata =dv6.train.only100 )
r2.dv6.100.train<- R2(dv6.train.only100$DV.6, pred.dv6.100.train)

##################################DV7:################################

#-----------------------------New5-----------------------------------#

model.dv7.new5 <- lm(DV.7~ V1+V2+V3+V4+V5, train.new)
summary(model.dv7.new5)

pred.dv7.new5.train<- predict(model.dv7.new5, newdata =train.new )
r2.dv7.new5.train<- R2(train.new$DV.7, pred.dv7.new5.train)

pred.dv7.new5.test<- predict(model.dv7.new5, newdata =test.new )
r2.dv7.new5.test<- R2(test.new$DV.7, pred.dv7.new5.test)

#---------------------------------Big5-------------------------------#

model.dv7.big5 <- lm(DV.7~ O+C+E+A+N, train.new)
summary(model.dv7.big)

pred.dv7.big5.train<- predict(model.dv7.big5, newdata =train.new )
r2.dv7.big5.train<- R2(train.new$DV.7, pred.dv7.big5.train)

pred.dv7.big5.test<- predict(model.dv7.big5, newdata =test.new )
r2.dv7.big5.test<- R2(test.new$DV.7, pred.dv7.big5.test)

#----------------------------------100Q--------------------------------#

dv7.train.only100<- data.table(train.only100, DV.7= train.new$DV.7)
dv7.test.only100<- data.table(test.only100, DV.7= test.new$DV.7)

model.dv7.100 <- lm(DV.7~., dv7.train.only100)
summary(model.dv7.100)

pred.dv7.100.test<- predict(model.dv7.100, newdata =dv7.test.only100 )
r2.dv7.100.test<- R2(dv7.test.only100$DV.7, pred.dv7.100.test)

pred.dv7.100.train<- predict(model.dv7.100, newdata =dv7.train.only100 )
r2.dv7.100.train<- R2(dv7.train.only100$DV.7, pred.dv7.100.train)

##################################DV8:################################

#-----------------------------New5-----------------------------------#

model.dv8.new5 <- lm(DV.8~ V1+V2+V3+V4+V5, train.new)
summary(model.dv8.new5)

pred.dv8.new5.train<- predict(model.dv8.new5, newdata =train.new )
r2.dv8.new5.train<- R2(train.new$DV.8, pred.dv8.new5.train)

pred.dv8.new5.test<- predict(model.dv8.new5, newdata =test.new )
r2.dv8.new5.test<- R2(test.new$DV.8, pred.dv8.new5.test)

#---------------------------------Big5-------------------------------#

model.dv8.big5 <- lm(DV.8~ O+C+E+A+N, train.new)
summary(model.dv8.big)

pred.dv8.big5.train<- predict(model.dv8.big5, newdata =train.new )
r2.dv8.big5.train<- R2(train.new$DV.8, pred.dv8.big5.train)

pred.dv8.big5.test<- predict(model.dv8.big5, newdata =test.new )
r2.dv8.big5.test<- R2(test.new$DV.8, pred.dv8.big5.test)

#----------------------------------100Q--------------------------------#

dv8.train.only100<- data.table(train.only100, DV.8= train.new$DV.8)
dv8.test.only100<- data.table(test.only100, DV.8= test.new$DV.8)

model.dv8.100 <- lm(DV.8~., dv8.train.only100)
summary(model.dv8.100)

pred.dv8.100.test<- predict(model.dv8.100, newdata =dv8.test.only100 )
r2.dv8.100.test<- R2(dv8.test.only100$DV.8, pred.dv8.100.test)

pred.dv8.100.train<- predict(model.dv8.100, newdata =dv8.train.only100 )
r2.dv8.100.train<- R2(dv8.train.only100$DV.8, pred.dv8.100.train)


##################################DV9:################################

#-----------------------------New5-----------------------------------#

model.dv9.new5 <- lm(DV.9~ V1+V2+V3+V4+V5, train.new)
summary(model.dv9.new5)

pred.dv9.new5.train<- predict(model.dv9.new5, newdata =train.new )
r2.dv9.new5.train<- R2(train.new$DV.9, pred.dv9.new5.train)

pred.dv9.new5.test<- predict(model.dv9.new5, newdata =test.new )
r2.dv9.new5.test<- R2(test.new$DV.9, pred.dv9.new5.test)

#---------------------------------Big5-------------------------------#

model.dv9.big5 <- lm(DV.9~ O+C+E+A+N, train.new)
summary(model.dv9.big)

pred.dv9.big5.train<- predict(model.dv9.big5, newdata =train.new )
r2.dv9.big5.train<- R2(train.new$DV.9, pred.dv9.big5.train)

pred.dv9.big5.test<- predict(model.dv9.big5, newdata =test.new )
r2.dv9.big5.test<- R2(test.new$DV.9, pred.dv9.big5.test)

#----------------------------------100Q--------------------------------#

dv9.train.only100<- data.table(train.only100, DV.9= train.new$DV.9)
dv9.test.only100<- data.table(test.only100, DV.9= test.new$DV.9)

model.dv9.100 <- lm(DV.9~., dv9.train.only100)
summary(model.dv9.100)

pred.dv9.100.test<- predict(model.dv9.100, newdata =dv9.test.only100 )
r2.dv9.100.test<- R2(dv9.test.only100$DV.9, pred.dv9.100.test)

pred.dv9.100.train<- predict(model.dv9.100, newdata =dv9.train.only100 )
r2.dv9.100.train<- R2(dv9.train.only100$DV.9, pred.dv9.100.train)


##################################DV10:################################

#-----------------------------New5-----------------------------------#

model.dv10.new5 <- lm(DV.10~ V1+V2+V3+V4+V5, train.new)
summary(model.dv10.new5)

pred.dv10.new5.train<- predict(model.dv10.new5, newdata =train.new )
r2.dv10.new5.train<- R2(train.new$DV.10, pred.dv10.new5.train)

pred.dv10.new5.test<- predict(model.dv10.new5, newdata =test.new )
r2.dv10.new5.test<- R2(test.new$DV.10, pred.dv10.new5.test)

#---------------------------------Big5-------------------------------#

model.dv10.big5 <- lm(DV.10~ O+C+E+A+N, train.new)
summary(model.dv10.big)

pred.dv10.big5.train<- predict(model.dv10.big5, newdata =train.new )
r2.dv10.big5.train<- R2(train.new$DV.10, pred.dv10.big5.train)

pred.dv10.big5.test<- predict(model.dv10.big5, newdata =test.new )
r2.dv10.big5.test<- R2(test.new$DV.10, pred.dv10.big5.test)

#----------------------------------100Q--------------------------------#

dv10.train.only100<- data.table(train.only100, DV.10= train.new$DV.10)
dv10.test.only100<- data.table(test.only100, DV.10= test.new$DV.10)

model.dv10.100 <- lm(DV.10~., dv10.train.only100)
summary(model.dv10.100)

pred.dv10.100.test<- predict(model.dv10.100, newdata =dv10.test.only100 )
r2.dv10.100.test<- R2(dv10.test.only100$DV.10, pred.dv10.100.test)

pred.dv10.100.train<- predict(model.dv10.100, newdata =dv10.train.only100 )
r2.dv10.100.train<- R2(dv10.train.only100$DV.10, pred.dv10.100.train)



##################################DV11:################################

#-----------------------------New5-----------------------------------#

model.dv11.new5 <- lm(DV.11~ V1+V2+V3+V4+V5, train.new)
summary(model.dv11.new5)

pred.dv11.new5.train<- predict(model.dv11.new5, newdata =train.new )
r2.dv11.new5.train<- R2(train.new$DV.11, pred.dv11.new5.train)

pred.dv11.new5.test<- predict(model.dv11.new5, newdata =test.new )
r2.dv11.new5.test<- R2(test.new$DV.11, pred.dv11.new5.test)

#---------------------------------Big5-------------------------------#

model.dv11.big5 <- lm(DV.11~ O+C+E+A+N, train.new)
summary(model.dv11.big)

pred.dv11.big5.train<- predict(model.dv11.big5, newdata =train.new )
r2.dv11.big5.train<- R2(train.new$DV.11, pred.dv11.big5.train)

pred.dv11.big5.test<- predict(model.dv11.big5, newdata =test.new )
r2.dv11.big5.test<- R2(test.new$DV.11, pred.dv11.big5.test)

#----------------------------------100Q--------------------------------#

dv11.train.only100<- data.table(train.only100, DV.11= train.new$DV.11)
dv11.test.only100<- data.table(test.only100, DV.11= test.new$DV.11)

model.dv11.100 <- lm(DV.11~., dv11.train.only100)
summary(model.dv11.100)

pred.dv11.100.test<- predict(model.dv11.100, newdata =dv11.test.only100 )
r2.dv11.100.test<- R2(dv11.test.only100$DV.11, pred.dv11.100.test)

pred.dv11.100.train<- predict(model.dv11.100, newdata =dv11.train.only100 )
r2.dv11.100.train<- R2(dv11.train.only100$DV.11, pred.dv11.100.train)

##################################DV12:################################

#-----------------------------New5-----------------------------------#

model.dv12.new5 <- lm(DV.12~ V1+V2+V3+V4+V5, train.new)
summary(model.dv12.new5)

pred.dv12.new5.train<- predict(model.dv12.new5, newdata =train.new )
r2.dv12.new5.train<- R2(train.new$DV.12, pred.dv12.new5.train)

pred.dv12.new5.test<- predict(model.dv12.new5, newdata =test.new )
r2.dv12.new5.test<- R2(test.new$DV.12, pred.dv12.new5.test)

#---------------------------------Big5-------------------------------#

model.dv12.big5 <- lm(DV.12~ O+C+E+A+N, train.new)
summary(model.dv12.big)

pred.dv12.big5.train<- predict(model.dv12.big5, newdata =train.new )
r2.dv12.big5.train<- R2(train.new$DV.12, pred.dv12.big5.train)

pred.dv12.big5.test<- predict(model.dv12.big5, newdata =test.new )
r2.dv12.big5.test<- R2(test.new$DV.12, pred.dv12.big5.test)

#----------------------------------100Q--------------------------------#

dv12.train.only100<- data.table(train.only100, DV.12= train.new$DV.12)
dv12.test.only100<- data.table(test.only100, DV.12= test.new$DV.12)

model.dv12.100 <- lm(DV.12~., dv12.train.only100)
summary(model.dv12.100)

pred.dv12.100.test<- predict(model.dv12.100, newdata =dv12.test.only100 )
r2.dv12.100.test<- R2(dv12.test.only100$DV.12, pred.dv12.100.test)

pred.dv12.100.train<- predict(model.dv12.100, newdata =dv12.train.only100 )
r2.dv12.100.train<- R2(dv12.train.only100$DV.12, pred.dv12.100.train)


#############################################################
####Gender####
train.new$gender<- as.factor(train.new$gender)
class(train.new$gender)
model.gender<- glm(gender ~O+C+E+A+N,
              family=binomial,data=train.new)

pred.gender.new5<- predict(model.gender, newdata =test.new, type="response")

gender.pred = rep("0", dim(test.new)[1])
gender.pred[pred.gender.new5 > .5] = "1"
table(gender.pred, test.new$gender)
mean(gender.pred == test.new$gender)
 
#############################################################
###School#####

model.school.new5 <- lm(school~ V1+V2+V3+V4+V5, train.new)
summary(model.school.new5)

pred.school.new5<- predict(model.school.new5, newdata =test.new )
r2.school.new5<- R2(test.new$school, pred.school.new5)

model.school.big5 <- lm(school~O+C+E+A+N, train.new)
summary(model.school.big5)

pred.school.big5<- predict(model.school.big5, newdata =test.new )
r2.school.big5<- R2(test.new$school, pred.school.big5)

#############################################################
###wealth#####

model.wealth.new5 <- lm(wealth~ V1+V2+V3+V4+V5, train.new)
summary(model.wealth.new5)

pred.wealth.new5<- predict(model.wealth.new5, newdata =test.new )
r2.wealth.new5<- R2(test.new$wealth, pred.wealth.new5)

model.wealth.big5 <- lm(wealth~O+C+E+A+N, train.new)
summary(model.wealth.big5)

pred.wealth.big5<- predict(model.wealth.big5, newdata =test.new )
r2.wealth.big5<- R2(test.new$wealth, pred.wealth.big5)

#############################################################
###religious#####

model.religious.new5 <- lm(religious~ V1+V2+V3+V4+V5, train.new)
summary(model.religious.new5)

pred.religious.new5<- predict(model.religious.new5, newdata =test.new )
r2.religious.new5<- R2(test.new$religious, pred.religious.new5)

model.religious.big5 <- lm(religious~O+C+E+A+N, train.new)
summary(model.religious.big5)

pred.religious.big5<- predict(model.religious.big5, newdata =test.new )
r2.religious.big5<- R2(test.new$religious, pred.religious.big5)


#############################################################
###religious#####

model.religious.new5 <- lm(religious~ V1+V2+V3+V4+V5, train.new)
summary(model.religious.new5)

pred.religious.new5<- predict(model.religious.new5, newdata =test.new )
r2.religious.new5<- R2(test.new$religious, pred.religious.new5)

model.religious.big5 <- lm(religious~O+C+E+A+N, train.new)
summary(model.religious.big5)

pred.religious.big5<- predict(model.religious.big5, newdata =test.new )
r2.religious.big5<- R2(test.new$religious, pred.religious.big5)