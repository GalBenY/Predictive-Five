
#New Mturk Data:

new.data<- fread("new.mturk.reverse.csv", header = T)
names(new.data)
new.data.100<- na.omit (new.data)
apply(new.data.100,c(1,2), is.na)

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

##create the results table:

newdata.cv.results<- data.table(dv=rep(c("DV1","DV2","DV3","DV4","DV5","DV6",
                                         "DV7","DV8","DV9","DV10","DV11","DV12"), each=32), 
                            Type= rep(c("100Q","Predictive Five", "Big Five", "PC Five"), each=8, times= 12),
                            Set= rep(c("Test", "Train"),each=4, times= 48))

#Create the five new factors using a rotation matrix,
#and add them to the original table:
pc5<-as.matrix(PC$rotation[,1:5])
new.data.100<- na.omit(new.data.100)

only.100<- as.matrix(new.data.100[,q1:q100])
new5<- only.100 %*% rotation.mat
newpc5<- only.100 %*%pc5
all.new<-  as.data.table(cbind(new.data.100, new5,newpc5))
dim(all.new)
names(all.new)


set.seed(1)
folds <-4
fold.assignment <- sample(1:folds, nrow(all.new), replace = TRUE)

r2.12<-NULL
sd.12<-NULL

###################################DV1################################

#100Q with CV, Fold=4:
errors <- NULL

for (k in 1:folds){
  dv1.cross.train <- all.new[fold.assignment!=k,] # train subset
  dv1.cross.test <-  all.new[fold.assignment==k,] # test subset
  .ols <- lm(DV.1~q1+q2+q3+q4+q5+q6+q7+q8+q9+q10+q11+q12+q13+q14+q15+
               q16+q17+q18+q19+q20+q21+q22+q23+q24+q25+q26+q27+q28+
               q29+q30+q31+q32+q33+q34+q35+q36+q37+q38+q39+q40+q41+
               q42+q43+q44+q45+q46+q47+q48+q49+q50+q51+q52+q53+q54+
               q55+q56+q57+q58+q59+q60+q61+q62+q63+q64+q65+q66+q67+
               q68+q69+q70+q71+q72+q73+q74+q75+q76+q77+q78+q79+q80+
               q81+q82+q83+q84+q85+q86+q87+q88+q89+q90+q91+q92+q93+
               q94+q95+q96+q97+q98+q99+q100 ,data = dv1.cross.train) # train
  .predictions <- predict(.ols, newdata=dv1.cross.test)
  .errors <-   R2(dv1.cross.test$DV.1, .predictions) # save prediction errors in the fold
  errors <- c(errors, .errors) # aggregate error over folds.
}


r2.12<-c(r2.12,errors)

errors <- NULL

for (k in 1:folds){
  dv1.cross.train <- all.new[fold.assignment!=k,] # train subset
  dv1.cross.test <-  all.new[fold.assignment==k,] # test subset
  .ols <- lm(DV.1~q1+q2+q3+q4+q5+q6+q7+q8+q9+q10+q11+q12+q13+q14+q15+
               q16+q17+q18+q19+q20+q21+q22+q23+q24+q25+q26+q27+q28+
               q29+q30+q31+q32+q33+q34+q35+q36+q37+q38+q39+q40+q41+
               q42+q43+q44+q45+q46+q47+q48+q49+q50+q51+q52+q53+q54+
               q55+q56+q57+q58+q59+q60+q61+q62+q63+q64+q65+q66+q67+
               q68+q69+q70+q71+q72+q73+q74+q75+q76+q77+q78+q79+q80+
               q81+q82+q83+q84+q85+q86+q87+q88+q89+q90+q91+q92+q93+
               q94+q95+q96+q97+q98+q99+q100 ,data = dv1.cross.train) # train
  .predictions <- predict(.ols, newdata=dv1.cross.train)
  .errors <-   R2(dv1.cross.train$DV.1, .predictions) # save prediction errors in the fold
  errors <- c(errors, .errors) # aggregate error over folds.
}


r2.12<-c(r2.12,errors)


#New 5 model with CV, fold=4:

errors <- NULL

for (k in 1:folds){
  dv1.cross.train <- all.new[fold.assignment!=k,] # train subset
  dv1.cross.test <-  all.new[fold.assignment==k,] # test subset
  .ols <- lm(DV.1~V1+V2+V3+V4+V5 ,data = dv1.cross.train) # train
  .predictions <- predict(.ols, newdata=dv1.cross.test)
  .errors <-   R2(dv1.cross.test$DV.1, .predictions) # save prediction errors in the fold
  errors <- c(errors, .errors) # aggregate error over folds.
}


r2.12<-c(r2.12,errors)


errors <- NULL

for (k in 1:folds){
  dv1.cross.train <- all.new[fold.assignment!=k,] # train subset
  dv1.cross.test <-  all.new[fold.assignment==k,] # test subset
  .ols <- lm(DV.1~V1+V2+V3+V4+V5 ,data = dv1.cross.train) # train
  .predictions <- predict(.ols, newdata=dv1.cross.train)
  .errors <-   R2(dv1.cross.train$DV.1, .predictions) # save prediction errors in the fold
  errors <- c(errors, .errors) # aggregate error over folds.
}


r2.12<-c(r2.12,errors)


#Big5 model with CV, fold=5:

errors <- NULL

for (k in 1:folds){
  dv1.cross.train <- all.new[fold.assignment!=k,] # train subset
  dv1.cross.test <-  all.new[fold.assignment==k,] # test subset
  .ols <- lm(DV.1~O+C+E+A+N ,data = dv1.cross.train) # train
  .predictions <- predict(.ols, newdata=dv1.cross.test)
  .errors <-   R2(dv1.cross.test$DV.1, .predictions) # save prediction errors in the fold
  errors <- c(errors, .errors) # aggregate error over folds.
}


r2.12<-c(r2.12,errors)

errors <- NULL

for (k in 1:folds){
  dv1.cross.train <- all.new[fold.assignment!=k,] # train subset
  dv1.cross.test <-  all.new[fold.assignment==k,] # test subset
  .ols <- lm(DV.1~O+C+E+A+N ,data = dv1.cross.train) # train
  .predictions <- predict(.ols, newdata=dv1.cross.train)
  .errors <-   R2(dv1.cross.train$DV.1, .predictions) # save prediction errors in the fold
  errors <- c(errors, .errors) # aggregate error over folds.
}


r2.12<-c(r2.12,errors)

#PC5 model with CV, fold=5:
errors <- NULL

for (k in 1:folds){
  dv1.cross.train <- all.new[fold.assignment!=k,] # train subset
  dv1.cross.test <-  all.new[fold.assignment==k,] # test subset
  .ols <- lm(DV.1~PC1+PC2+PC3+PC4+PC5 ,data = dv1.cross.train) # train
  .predictions <- predict(.ols, newdata=dv1.cross.test)
  .errors <-   R2(dv1.cross.test$DV.1, .predictions) # save prediction errors in the fold
  errors <- c(errors, .errors) # aggregate error over folds.
}


r2.12<-c(r2.12,errors)


errors <- NULL

for (k in 1:folds){
  dv1.cross.train <- all.new[fold.assignment!=k,] # train subset
  dv1.cross.test <-  all.new[fold.assignment==k,] # test subset
  .ols <- lm(DV.1~PC1+PC2+PC3+PC4+PC5 ,data = dv1.cross.train) # train
  .predictions <- predict(.ols, newdata=dv1.cross.train)
  .errors <-   R2(dv1.cross.train$DV.1, .predictions) # save prediction errors in the fold
  errors <- c(errors, .errors) # aggregate error over folds.
}


r2.12<-c(r2.12,errors)


###################################DV2################################
#100Q with CV, Fold=4:
errors <- NULL

for (k in 1:folds){
  dv2.cross.train <- all.new[fold.assignment!=k,] # train subset
  dv2.cross.test <-  all.new[fold.assignment==k,] # test subset
  .ols <- lm(DV.2~q1+q2+q3+q4+q5+q6+q7+q8+q9+q10+q11+q12+q13+q14+q15+
               q16+q17+q18+q19+q20+q21+q22+q23+q24+q25+q26+q27+q28+
               q29+q30+q31+q32+q33+q34+q35+q36+q37+q38+q39+q40+q41+
               q42+q43+q44+q45+q46+q47+q48+q49+q50+q51+q52+q53+q54+
               q55+q56+q57+q58+q59+q60+q61+q62+q63+q64+q65+q66+q67+
               q68+q69+q70+q71+q72+q73+q74+q75+q76+q77+q78+q79+q80+
               q81+q82+q83+q84+q85+q86+q87+q88+q89+q90+q91+q92+q93+
               q94+q95+q96+q97+q98+q99+q100 ,data = dv2.cross.train) # train
  .predictions <- predict(.ols, newdata=dv2.cross.test)
  .errors <-   R2(dv2.cross.test$DV.2, .predictions) # save prediction errors in the fold
  errors <- c(errors, .errors) # aggregate error over folds.
}


r2.12<-c(r2.12,errors)

errors <- NULL

for (k in 1:folds){
  dv2.cross.train <- all.new[fold.assignment!=k,] # train subset
  dv2.cross.test <-  all.new[fold.assignment==k,] # test subset
  .ols <- lm(DV.2~q1+q2+q3+q4+q5+q6+q7+q8+q9+q10+q11+q12+q13+q14+q15+
               q16+q17+q18+q19+q20+q21+q22+q23+q24+q25+q26+q27+q28+
               q29+q30+q31+q32+q33+q34+q35+q36+q37+q38+q39+q40+q41+
               q42+q43+q44+q45+q46+q47+q48+q49+q50+q51+q52+q53+q54+
               q55+q56+q57+q58+q59+q60+q61+q62+q63+q64+q65+q66+q67+
               q68+q69+q70+q71+q72+q73+q74+q75+q76+q77+q78+q79+q80+
               q81+q82+q83+q84+q85+q86+q87+q88+q89+q90+q91+q92+q93+
               q94+q95+q96+q97+q98+q99+q100 ,data = dv2.cross.train) # train
  .predictions <- predict(.ols, newdata=dv2.cross.train)
  .errors <-   R2(dv2.cross.train$DV.2, .predictions) # save prediction errors in the fold
  errors <- c(errors, .errors) # aggregate error over folds.
}


r2.12<-c(r2.12,errors)
#New 5 model with CV, fold=5:

errors <- NULL

for (k in 1:folds){
  dv2.cross.train <- all.new[fold.assignment!=k,] # train subset
  dv2.cross.test <-  all.new[fold.assignment==k,] # test subset
  .ols <- lm(DV.2~V1+V2+V3+V4+V5 ,data = dv2.cross.train) # train
  .predictions <- predict(.ols, newdata=dv2.cross.test)
  .errors <-   R2(dv2.cross.test$DV.2, .predictions) # save prediction errors in the fold
  errors <- c(errors, .errors) # aggregate error over folds.
}


r2.12<-c(r2.12,errors)

errors <- NULL

for (k in 1:folds){
  dv2.cross.train <- all.new[fold.assignment!=k,] # train subset
  dv2.cross.test <-  all.new[fold.assignment==k,] # test subset
  .ols <- lm(DV.2~V1+V2+V3+V4+V5 ,data = dv2.cross.train) # train
  .predictions <- predict(.ols, newdata=dv2.cross.train)
  .errors <-   R2(dv2.cross.train$DV.2, .predictions) # save prediction errors in the fold
  errors <- c(errors, .errors) # aggregate error over folds.
}


r2.12<-c(r2.12,errors)

#Big5 model with CV, fold=5:

errors <- NULL

for (k in 1:folds){
  dv2.cross.train <- all.new[fold.assignment!=k,] # train subset
  dv2.cross.test <-  all.new[fold.assignment==k,] # test subset
  .ols <- lm(DV.2~O+C+E+A+N ,data = dv2.cross.train) # train
  .predictions <- predict(.ols, newdata=dv2.cross.test)
  .errors <-   R2(dv2.cross.test$DV.2, .predictions) # save prediction errors in the fold
  errors <- c(errors, .errors) # aggregate error over folds.
}


r2.12<-c(r2.12,errors)


errors <- NULL

for (k in 1:folds){
  dv2.cross.train <- all.new[fold.assignment!=k,] # train subset
  dv2.cross.test <-  all.new[fold.assignment==k,] # test subset
  .ols <- lm(DV.2~O+C+E+A+N ,data = dv2.cross.train) # train
  .predictions <- predict(.ols, newdata=dv2.cross.train)
  .errors <-   R2(dv2.cross.train$DV.2, .predictions) # save prediction errors in the fold
  errors <- c(errors, .errors) # aggregate error over folds.
}


r2.12<-c(r2.12,errors)
#PC5 model with CV, fold=5:

errors <- NULL

for (k in 1:folds){
  dv2.cross.train <- all.new[fold.assignment!=k,] # train subset
  dv2.cross.test <-  all.new[fold.assignment==k,] # test subset
  .ols <- lm(DV.2~PC1+PC2+PC3+PC4+PC5 ,data = dv2.cross.train) # train
  .predictions <- predict(.ols, newdata=dv2.cross.test)
  .errors <-   R2(dv2.cross.test$DV.2, .predictions) # save prediction errors in the fold
  errors <- c(errors, .errors) # aggregate error over folds.
}


r2.12<-c(r2.12,errors)

errors <- NULL

for (k in 1:folds){
  dv2.cross.train <- all.new[fold.assignment!=k,] # train subset
  dv2.cross.test <-  all.new[fold.assignment==k,] # test subset
  .ols <- lm(DV.2~PC1+PC2+PC3+PC4+PC5 ,data = dv2.cross.train) # train
  .predictions <- predict(.ols, newdata=dv2.cross.train)
  .errors <-   R2(dv2.cross.train$DV.2, .predictions) # save prediction errors in the fold
  errors <- c(errors, .errors) # aggregate error over folds.
}


r2.12<-c(r2.12,errors)

###################################DV3################################
#100Q with CV, Fold=4:
errors <- NULL

for (k in 1:folds){
  dv3.cross.train <- all.new[fold.assignment!=k,] # train subset
  dv3.cross.test <-  all.new[fold.assignment==k,] # test subset
  .ols <- lm(DV.3~q1+q2+q3+q4+q5+q6+q7+q8+q9+q10+q11+q12+q13+q14+q15+
               q16+q17+q18+q19+q20+q21+q22+q23+q24+q25+q26+q27+q28+
               q29+q30+q31+q32+q33+q34+q35+q36+q37+q38+q39+q40+q41+
               q42+q43+q44+q45+q46+q47+q48+q49+q50+q51+q52+q53+q54+
               q55+q56+q57+q58+q59+q60+q61+q62+q63+q64+q65+q66+q67+
               q68+q69+q70+q71+q72+q73+q74+q75+q76+q77+q78+q79+q80+
               q81+q82+q83+q84+q85+q86+q87+q88+q89+q90+q91+q92+q93+
               q94+q95+q96+q97+q98+q99+q100 ,data = dv3.cross.train) # train
  .predictions <- predict(.ols, newdata=dv3.cross.test)
  .errors <-   R2(dv3.cross.test$DV.3, .predictions) # save prediction errors in the fold
  errors <- c(errors, .errors) # aggregate error over folds.
}


r2.12<-c(r2.12,errors)

errors <- NULL

for (k in 1:folds){
  dv3.cross.train <- all.new[fold.assignment!=k,] # train subset
  dv3.cross.test <-  all.new[fold.assignment==k,] # test subset
  .ols <- lm(DV.3~q1+q2+q3+q4+q5+q6+q7+q8+q9+q10+q11+q12+q13+q14+q15+
               q16+q17+q18+q19+q20+q21+q22+q23+q24+q25+q26+q27+q28+
               q29+q30+q31+q32+q33+q34+q35+q36+q37+q38+q39+q40+q41+
               q42+q43+q44+q45+q46+q47+q48+q49+q50+q51+q52+q53+q54+
               q55+q56+q57+q58+q59+q60+q61+q62+q63+q64+q65+q66+q67+
               q68+q69+q70+q71+q72+q73+q74+q75+q76+q77+q78+q79+q80+
               q81+q82+q83+q84+q85+q86+q87+q88+q89+q90+q91+q92+q93+
               q94+q95+q96+q97+q98+q99+q100 ,data = dv3.cross.train) # train
  .predictions <- predict(.ols, newdata=dv3.cross.train)
  .errors <-   R2(dv3.cross.train$DV.3, .predictions) # save prediction errors in the fold
  errors <- c(errors, .errors) # aggregate error over folds.
}


r2.12<-c(r2.12,errors)

#New 5 model with CV, fold=4:

errors <- NULL

for (k in 1:folds){
  dv3.cross.train <- all.new[fold.assignment!=k,] # train subset
  dv3.cross.test <-  all.new[fold.assignment==k,] # test subset
  .ols <- lm(DV.3~V1+V2+V3+V4+V5 ,data = dv3.cross.train) # train
  .predictions <- predict(.ols, newdata=dv3.cross.test)
  .errors <-   R2(dv3.cross.test$DV.3, .predictions) # save prediction errors in the fold
  errors <- c(errors, .errors) # aggregate error over folds.
}


r2.12<-c(r2.12,errors)


errors <- NULL

for (k in 1:folds){
  dv3.cross.train <- all.new[fold.assignment!=k,] # train subset
  dv3.cross.test <-  all.new[fold.assignment==k,] # test subset
  .ols <- lm(DV.3~V1+V2+V3+V4+V5 ,data = dv3.cross.train) # train
  .predictions <- predict(.ols, newdata=dv3.cross.train)
  .errors <-   R2(dv3.cross.train$DV.3, .predictions) # save prediction errors in the fold
  errors <- c(errors, .errors) # aggregate error over folds.
}


r2.12<-c(r2.12,errors)

#Big5 model with CV, fold=5:

errors <- NULL

for (k in 1:folds){
  dv3.cross.train <- all.new[fold.assignment!=k,] # train subset
  dv3.cross.test <-  all.new[fold.assignment==k,] # test subset
  .ols <- lm(DV.3~O+C+E+A+N ,data = dv3.cross.train) # train
  .predictions <- predict(.ols, newdata=dv3.cross.test)
  .errors <-   R2(dv3.cross.test$DV.3, .predictions) # save prediction errors in the fold
  errors <- c(errors, .errors) # aggregate error over folds.
}


r2.12<-c(r2.12,errors)


errors <- NULL

for (k in 1:folds){
  dv3.cross.train <- all.new[fold.assignment!=k,] # train subset
  dv3.cross.test <-  all.new[fold.assignment==k,] # test subset
  .ols <- lm(DV.3~O+C+E+A+N ,data = dv3.cross.train) # train
  .predictions <- predict(.ols, newdata=dv3.cross.train)
  .errors <-   R2(dv3.cross.train$DV.3, .predictions) # save prediction errors in the fold
  errors <- c(errors, .errors) # aggregate error over folds.
}


r2.12<-c(r2.12,errors)

#PC5 model with CV, fold=5:

errors <- NULL

for (k in 1:folds){
  dv3.cross.train <- all.new[fold.assignment!=k,] # train subset
  dv3.cross.test <-  all.new[fold.assignment==k,] # test subset
  .ols <- lm(DV.3~PC1+PC2+PC3+PC4+PC5 ,data = dv3.cross.train) # train
  .predictions <- predict(.ols, newdata=dv3.cross.test)
  .errors <-   R2(dv3.cross.test$DV.3, .predictions) # save prediction errors in the fold
  errors <- c(errors, .errors) # aggregate error over folds.
}


r2.12<-c(r2.12,errors)


errors <- NULL

for (k in 1:folds){
  dv3.cross.train <- all.new[fold.assignment!=k,] # train subset
  dv3.cross.test <-  all.new[fold.assignment==k,] # test subset
  .ols <- lm(DV.3~PC1+PC2+PC3+PC4+PC5 ,data = dv3.cross.train) # train
  .predictions <- predict(.ols, newdata=dv3.cross.train)
  .errors <-   R2(dv3.cross.train$DV.3, .predictions) # save prediction errors in the fold
  errors <- c(errors, .errors) # aggregate error over folds.
}


r2.12<-c(r2.12,errors)

###################################DV4################################
#100Q with CV, Fold=4:

errors <- NULL

for (k in 1:folds){
  dv4.cross.train <- all.new[fold.assignment!=k,] # train subset
  dv4.cross.test <-  all.new[fold.assignment==k,] # test subset
  .ols <- lm(DV.4~q1+q2+q3+q4+q5+q6+q7+q8+q9+q10+q11+q12+q13+q14+q15+
               q16+q17+q18+q19+q20+q21+q22+q23+q24+q25+q26+q27+q28+
               q29+q30+q31+q32+q33+q34+q35+q36+q37+q38+q39+q40+q41+
               q42+q43+q44+q45+q46+q47+q48+q49+q50+q51+q52+q53+q54+
               q55+q56+q57+q58+q59+q60+q61+q62+q63+q64+q65+q66+q67+
               q68+q69+q70+q71+q72+q73+q74+q75+q76+q77+q78+q79+q80+
               q81+q82+q83+q84+q85+q86+q87+q88+q89+q90+q91+q92+q93+
               q94+q95+q96+q97+q98+q99+q100 ,data = dv4.cross.train) # train
  .predictions <- predict(.ols, newdata=dv4.cross.test)
  .errors <-   R2(dv4.cross.test$DV.4, .predictions) # save prediction errors in the fold
  errors <- c(errors, .errors) # aggregate error over folds.
}


r2.12<-c(r2.12,errors)
errors <- NULL

for (k in 1:folds){
  dv4.cross.train <- all.new[fold.assignment!=k,] # train subset
  dv4.cross.test <-  all.new[fold.assignment==k,] # test subset
  .ols <- lm(DV.4~q1+q2+q3+q4+q5+q6+q7+q8+q9+q10+q11+q12+q13+q14+q15+
               q16+q17+q18+q19+q20+q21+q22+q23+q24+q25+q26+q27+q28+
               q29+q30+q31+q32+q33+q34+q35+q36+q37+q38+q39+q40+q41+
               q42+q43+q44+q45+q46+q47+q48+q49+q50+q51+q52+q53+q54+
               q55+q56+q57+q58+q59+q60+q61+q62+q63+q64+q65+q66+q67+
               q68+q69+q70+q71+q72+q73+q74+q75+q76+q77+q78+q79+q80+
               q81+q82+q83+q84+q85+q86+q87+q88+q89+q90+q91+q92+q93+
               q94+q95+q96+q97+q98+q99+q100 ,data = dv4.cross.train) # train
  .predictions <- predict(.ols, newdata=dv4.cross.train)
  .errors <-   R2(dv4.cross.train$DV.4, .predictions) # save prediction errors in the fold
  errors <- c(errors, .errors) # aggregate error over folds.
}


r2.12<-c(r2.12,errors)

#New 5 model with CV, fold=5:

errors <- NULL

for (k in 1:folds){
  dv4.cross.train <- all.new[fold.assignment!=k,] # train subset
  dv4.cross.test <-  all.new[fold.assignment==k,] # test subset
  .ols <- lm(DV.4~V1+V2+V3+V4+V5 ,data = dv4.cross.train) # train
  .predictions <- predict(.ols, newdata=dv4.cross.test)
  .errors <-   R2(dv4.cross.test$DV.4, .predictions) # save prediction errors in the fold
  errors <- c(errors, .errors) # aggregate error over folds.
}


r2.12<-c(r2.12,errors)


errors <- NULL

for (k in 1:folds){
  dv4.cross.train <- all.new[fold.assignment!=k,] # train subset
  dv4.cross.test <-  all.new[fold.assignment==k,] # test subset
  .ols <- lm(DV.4~V1+V2+V3+V4+V5 ,data = dv4.cross.train) # train
  .predictions <- predict(.ols, newdata=dv4.cross.train)
  .errors <-   R2(dv4.cross.train$DV.4, .predictions) # save prediction errors in the fold
  errors <- c(errors, .errors) # aggregate error over folds.
}


r2.12<-c(r2.12,errors)

#Big5 model with CV, fold=5:

errors <- NULL

for (k in 1:folds){
  dv4.cross.train <- all.new[fold.assignment!=k,] # train subset
  dv4.cross.test <-  all.new[fold.assignment==k,] # test subset
  .ols <- lm(DV.4~O+C+E+A+N ,data = dv4.cross.train) # train
  .predictions <- predict(.ols, newdata=dv4.cross.test)
  .errors <-   R2(dv4.cross.test$DV.4, .predictions) # save prediction errors in the fold
  errors <- c(errors, .errors) # aggregate error over folds.
}


r2.12<-c(r2.12,errors)


errors <- NULL

for (k in 1:folds){
  dv4.cross.train <- all.new[fold.assignment!=k,] # train subset
  dv4.cross.test <-  all.new[fold.assignment==k,] # test subset
  .ols <- lm(DV.4~O+C+E+A+N ,data = dv4.cross.train) # train
  .predictions <- predict(.ols, newdata=dv4.cross.train)
  .errors <-   R2(dv4.cross.train$DV.4, .predictions) # save prediction errors in the fold
  errors <- c(errors, .errors) # aggregate error over folds.
}


r2.12<-c(r2.12,errors)

#PC5 model with CV, fold=5:

errors <- NULL

for (k in 1:folds){
  dv4.cross.train <- all.new[fold.assignment!=k,] # train subset
  dv4.cross.test <-  all.new[fold.assignment==k,] # test subset
  .ols <- lm(DV.4~PC1+PC2+PC3+PC4+PC5 ,data = dv4.cross.train) # train
  .predictions <- predict(.ols, newdata=dv4.cross.test)
  .errors <-   R2(dv4.cross.test$DV.4, .predictions) # save prediction errors in the fold
  errors <- c(errors, .errors) # aggregate error over folds.
}


r2.12<-c(r2.12,errors)


errors <- NULL

for (k in 1:folds){
  dv4.cross.train <- all.new[fold.assignment!=k,] # train subset
  dv4.cross.test <-  all.new[fold.assignment==k,] # test subset
  .ols <- lm(DV.4~PC1+PC2+PC3+PC4+PC5 ,data = dv4.cross.train) # train
  .predictions <- predict(.ols, newdata=dv4.cross.train)
  .errors <-   R2(dv4.cross.train$DV.4, .predictions) # save prediction errors in the fold
  errors <- c(errors, .errors) # aggregate error over folds.
}


r2.12<-c(r2.12,errors)

###################################DV5################################
#100Q with CV, Fold=4:
errors <- NULL

for (k in 1:folds){
  dv5.cross.train <- all.new[fold.assignment!=k,] # train subset
  dv5.cross.test <-  all.new[fold.assignment==k,] # test subset
  .ols <- lm(DV.5~q1+q2+q3+q4+q5+q6+q7+q8+q9+q10+q11+q12+q13+q14+q15+
               q16+q17+q18+q19+q20+q21+q22+q23+q24+q25+q26+q27+q28+
               q29+q30+q31+q32+q33+q34+q35+q36+q37+q38+q39+q40+q41+
               q42+q43+q44+q45+q46+q47+q48+q49+q50+q51+q52+q53+q54+
               q55+q56+q57+q58+q59+q60+q61+q62+q63+q64+q65+q66+q67+
               q68+q69+q70+q71+q72+q73+q74+q75+q76+q77+q78+q79+q80+
               q81+q82+q83+q84+q85+q86+q87+q88+q89+q90+q91+q92+q93+
               q94+q95+q96+q97+q98+q99+q100 ,data = dv5.cross.train) # train
  .predictions <- predict(.ols, newdata=dv5.cross.test)
  .errors <-   R2(dv5.cross.test$DV.5, .predictions) # save prediction errors in the fold
  errors <- c(errors, .errors) # aggregate error over folds.
}


r2.12<-c(r2.12,errors)

errors <- NULL

for (k in 1:folds){
  dv5.cross.train <- all.new[fold.assignment!=k,] # train subset
  dv5.cross.test <-  all.new[fold.assignment==k,] # test subset
  .ols <- lm(DV.5~q1+q2+q3+q4+q5+q6+q7+q8+q9+q10+q11+q12+q13+q14+q15+
               q16+q17+q18+q19+q20+q21+q22+q23+q24+q25+q26+q27+q28+
               q29+q30+q31+q32+q33+q34+q35+q36+q37+q38+q39+q40+q41+
               q42+q43+q44+q45+q46+q47+q48+q49+q50+q51+q52+q53+q54+
               q55+q56+q57+q58+q59+q60+q61+q62+q63+q64+q65+q66+q67+
               q68+q69+q70+q71+q72+q73+q74+q75+q76+q77+q78+q79+q80+
               q81+q82+q83+q84+q85+q86+q87+q88+q89+q90+q91+q92+q93+
               q94+q95+q96+q97+q98+q99+q100 ,data = dv5.cross.train) # train
  .predictions <- predict(.ols, newdata=dv5.cross.train)
  .errors <-   R2(dv5.cross.train$DV.5, .predictions) # save prediction errors in the fold
  errors <- c(errors, .errors) # aggregate error over folds.
}


r2.12<-c(r2.12,errors)

#New 5 model with CV, fold=4:

errors <- NULL

for (k in 1:folds){
  dv5.cross.train <- all.new[fold.assignment!=k,] # train subset
  dv5.cross.test <-  all.new[fold.assignment==k,] # test subset
  .ols <- lm(DV.5~V1+V2+V3+V4+V5 ,data = dv5.cross.train) # train
  .predictions <- predict(.ols, newdata=dv5.cross.test)
  .errors <-   R2(dv5.cross.test$DV.5, .predictions) # save prediction errors in the fold
  errors <- c(errors, .errors) # aggregate error over folds.
}


r2.12<-c(r2.12,errors)

errors <- NULL

for (k in 1:folds){
  dv5.cross.train <- all.new[fold.assignment!=k,] # train subset
  dv5.cross.test <-  all.new[fold.assignment==k,] # test subset
  .ols <- lm(DV.5~V1+V2+V3+V4+V5 ,data = dv5.cross.train) # train
  .predictions <- predict(.ols, newdata=dv5.cross.train)
  .errors <-   R2(dv5.cross.train$DV.5, .predictions) # save prediction errors in the fold
  errors <- c(errors, .errors) # aggregate error over folds.
}


r2.12<-c(r2.12,errors)

#Big5 model with CV, fold=5:

errors <- NULL

for (k in 1:folds){
  dv5.cross.train <- all.new[fold.assignment!=k,] # train subset
  dv5.cross.test <-  all.new[fold.assignment==k,] # test subset
  .ols <- lm(DV.5~O+C+E+A+N ,data = dv5.cross.train) # train
  .predictions <- predict(.ols, newdata=dv5.cross.test)
  .errors <-   R2(dv5.cross.test$DV.5, .predictions) # save prediction errors in the fold
  errors <- c(errors, .errors) # aggregate error over folds.
}


r2.12<-c(r2.12,errors)

errors <- NULL

for (k in 1:folds){
  dv5.cross.train <- all.new[fold.assignment!=k,] # train subset
  dv5.cross.test <-  all.new[fold.assignment==k,] # test subset
  .ols <- lm(DV.5~O+C+E+A+N ,data = dv5.cross.train) # train
  .predictions <- predict(.ols, newdata=dv5.cross.train)
  .errors <-   R2(dv5.cross.train$DV.5, .predictions) # save prediction errors in the fold
  errors <- c(errors, .errors) # aggregate error over folds.
}


r2.12<-c(r2.12,errors)

#PC5 model with CV, fold=5:


errors <- NULL

for (k in 1:folds){
  dv5.cross.train <- all.new[fold.assignment!=k,] # train subset
  dv5.cross.test <-  all.new[fold.assignment==k,] # test subset
  .ols <- lm(DV.5~PC1+PC2+PC3+PC4+PC5 ,data = dv5.cross.train) # train
  .predictions <- predict(.ols, newdata=dv5.cross.test)
  .errors <-   R2(dv5.cross.test$DV.5, .predictions) # save prediction errors in the fold
  errors <- c(errors, .errors) # aggregate error over folds.
}


r2.12<-c(r2.12,errors)


errors <- NULL

for (k in 1:folds){
  dv5.cross.train <- all.new[fold.assignment!=k,] # train subset
  dv5.cross.test <-  all.new[fold.assignment==k,] # test subset
  .ols <- lm(DV.5~PC1+PC2+PC3+PC4+PC5 ,data = dv5.cross.train) # train
  .predictions <- predict(.ols, newdata=dv5.cross.train)
  .errors <-   R2(dv5.cross.train$DV.5, .predictions) # save prediction errors in the fold
  errors <- c(errors, .errors) # aggregate error over folds.
}


r2.12<-c(r2.12,errors)

###################################DV6################################
#100Q with CV, Fold=4:

errors <- NULL

for (k in 1:folds){
  dv6.cross.train <- all.new[fold.assignment!=k,] # train subset
  dv6.cross.test <-  all.new[fold.assignment==k,] # test subset
  .ols <- lm(DV.6~q1+q2+q3+q4+q5+q6+q7+q8+q9+q10+q11+q12+q13+q14+q15+
               q16+q17+q18+q19+q20+q21+q22+q23+q24+q25+q26+q27+q28+
               q29+q30+q31+q32+q33+q34+q35+q36+q37+q38+q39+q40+q41+
               q42+q43+q44+q45+q46+q47+q48+q49+q50+q51+q52+q53+q54+
               q55+q56+q57+q58+q59+q60+q61+q62+q63+q64+q65+q66+q67+
               q68+q69+q70+q71+q72+q73+q74+q75+q76+q77+q78+q79+q80+
               q81+q82+q83+q84+q85+q86+q87+q88+q89+q90+q91+q92+q93+
               q94+q95+q96+q97+q98+q99+q100 ,data = dv6.cross.train) # train
  .predictions <- predict(.ols, newdata=dv6.cross.test)
  .errors <-   R2(dv6.cross.test$DV.6, .predictions) # save prediction errors in the fold
  errors <- c(errors, .errors) # aggregate error over folds.
}


r2.12<-c(r2.12,errors)

errors <- NULL

for (k in 1:folds){
  dv6.cross.train <- all.new[fold.assignment!=k,] # train subset
  dv6.cross.test <-  all.new[fold.assignment==k,] # test subset
  .ols <- lm(DV.6~q1+q2+q3+q4+q5+q6+q7+q8+q9+q10+q11+q12+q13+q14+q15+
               q16+q17+q18+q19+q20+q21+q22+q23+q24+q25+q26+q27+q28+
               q29+q30+q31+q32+q33+q34+q35+q36+q37+q38+q39+q40+q41+
               q42+q43+q44+q45+q46+q47+q48+q49+q50+q51+q52+q53+q54+
               q55+q56+q57+q58+q59+q60+q61+q62+q63+q64+q65+q66+q67+
               q68+q69+q70+q71+q72+q73+q74+q75+q76+q77+q78+q79+q80+
               q81+q82+q83+q84+q85+q86+q87+q88+q89+q90+q91+q92+q93+
               q94+q95+q96+q97+q98+q99+q100 ,data = dv6.cross.train) # train
  .predictions <- predict(.ols, newdata=dv6.cross.train)
  .errors <-   R2(dv6.cross.train$DV.6, .predictions) # save prediction errors in the fold
  errors <- c(errors, .errors) # aggregate error over folds.
}


r2.12<-c(r2.12,errors)

#New 5 model with CV, fold=5:

errors <- NULL

for (k in 1:folds){
  dv6.cross.train <- all.new[fold.assignment!=k,] # train subset
  dv6.cross.test <-  all.new[fold.assignment==k,] # test subset
  .ols <- lm(DV.6~V1+V2+V3+V4+V5 ,data = dv6.cross.train) # train
  .predictions <- predict(.ols, newdata=dv6.cross.test)
  .errors <-   R2(dv6.cross.test$DV.6, .predictions) # save prediction errors in the fold
  errors <- c(errors, .errors) # aggregate error over folds.
}


r2.12<-c(r2.12,errors)


errors <- NULL

for (k in 1:folds){
  dv6.cross.train <- all.new[fold.assignment!=k,] # train subset
  dv6.cross.test <-  all.new[fold.assignment==k,] # test subset
  .ols <- lm(DV.6~V1+V2+V3+V4+V5 ,data = dv6.cross.train) # train
  .predictions <- predict(.ols, newdata=dv6.cross.train)
  .errors <-   R2(dv6.cross.train$DV.6, .predictions) # save prediction errors in the fold
  errors <- c(errors, .errors) # aggregate error over folds.
}


r2.12<-c(r2.12,errors)

#Big5 model with CV, fold=5:

errors <- NULL

for (k in 1:folds){
  dv6.cross.train <- all.new[fold.assignment!=k,] # train subset
  dv6.cross.test <-  all.new[fold.assignment==k,] # test subset
  .ols <- lm(DV.6~O+C+E+A+N ,data = dv6.cross.train) # train
  .predictions <- predict(.ols, newdata=dv6.cross.test)
  .errors <-   R2(dv6.cross.test$DV.6, .predictions) # save prediction errors in the fold
  errors <- c(errors, .errors) # aggregate error over folds.
}


r2.12<-c(r2.12,errors)

errors <- NULL

for (k in 1:folds){
  dv6.cross.train <- all.new[fold.assignment!=k,] # train subset
  dv6.cross.test <-  all.new[fold.assignment==k,] # test subset
  .ols <- lm(DV.6~O+C+E+A+N ,data = dv6.cross.train) # train
  .predictions <- predict(.ols, newdata=dv6.cross.train)
  .errors <-   R2(dv6.cross.train$DV.6, .predictions) # save prediction errors in the fold
  errors <- c(errors, .errors) # aggregate error over folds.
}


r2.12<-c(r2.12,errors)

#PC5 model with CV, fold=5:

errors <- NULL

for (k in 1:folds){
  dv6.cross.train <- all.new[fold.assignment!=k,] # train subset
  dv6.cross.test <-  all.new[fold.assignment==k,] # test subset
  .ols <- lm(DV.6~PC1+PC2+PC3+PC4+PC5 ,data = dv6.cross.train) # train
  .predictions <- predict(.ols, newdata=dv6.cross.test)
  .errors <-   R2(dv6.cross.test$DV.6, .predictions) # save prediction errors in the fold
  errors <- c(errors, .errors) # aggregate error over folds.
}


r2.12<-c(r2.12,errors)

errors <- NULL

for (k in 1:folds){
  dv6.cross.train <- all.new[fold.assignment!=k,] # train subset
  dv6.cross.test <-  all.new[fold.assignment==k,] # test subset
  .ols <- lm(DV.6~PC1+PC2+PC3+PC4+PC5 ,data = dv6.cross.train) # train
  .predictions <- predict(.ols, newdata=dv6.cross.train)
  .errors <-   R2(dv6.cross.train$DV.6, .predictions) # save prediction errors in the fold
  errors <- c(errors, .errors) # aggregate error over folds.
}


r2.12<-c(r2.12,errors)

###################################DV7################################

#100Q with CV, Fold=4:
errors <- NULL

for (k in 1:folds){
  dv7.cross.train <- all.new[fold.assignment!=k,] # train subset
  dv7.cross.test <-  all.new[fold.assignment==k,] # test subset
  .ols <- lm(DV.7~q1+q2+q3+q4+q5+q6+q7+q8+q9+q10+q11+q12+q13+q14+q15+
               q16+q17+q18+q19+q20+q21+q22+q23+q24+q25+q26+q27+q28+
               q29+q30+q31+q32+q33+q34+q35+q36+q37+q38+q39+q40+q41+
               q42+q43+q44+q45+q46+q47+q48+q49+q50+q51+q52+q53+q54+
               q55+q56+q57+q58+q59+q60+q61+q62+q63+q64+q65+q66+q67+
               q68+q69+q70+q71+q72+q73+q74+q75+q76+q77+q78+q79+q80+
               q81+q82+q83+q84+q85+q86+q87+q88+q89+q90+q91+q92+q93+
               q94+q95+q96+q97+q98+q99+q100 ,data = dv7.cross.train) # train
  .predictions <- predict(.ols, newdata=dv7.cross.test)
  .errors <-   R2(dv7.cross.test$DV.7, .predictions) # save prediction errors in the fold
  errors <- c(errors, .errors) # aggregate error over folds.
}


r2.12<-c(r2.12,errors)
#New 5 model with CV, fold=5:


errors <- NULL

for (k in 1:folds){
  dv7.cross.train <- all.new[fold.assignment!=k,] # train subset
  dv7.cross.test <-  all.new[fold.assignment==k,] # test subset
  .ols <- lm(DV.7~q1+q2+q3+q4+q5+q6+q7+q8+q9+q10+q11+q12+q13+q14+q15+
               q16+q17+q18+q19+q20+q21+q22+q23+q24+q25+q26+q27+q28+
               q29+q30+q31+q32+q33+q34+q35+q36+q37+q38+q39+q40+q41+
               q42+q43+q44+q45+q46+q47+q48+q49+q50+q51+q52+q53+q54+
               q55+q56+q57+q58+q59+q60+q61+q62+q63+q64+q65+q66+q67+
               q68+q69+q70+q71+q72+q73+q74+q75+q76+q77+q78+q79+q80+
               q81+q82+q83+q84+q85+q86+q87+q88+q89+q90+q91+q92+q93+
               q94+q95+q96+q97+q98+q99+q100 ,data = dv7.cross.train) # train
  .predictions <- predict(.ols, newdata=dv7.cross.train)
  .errors <-   R2(dv7.cross.train$DV.7, .predictions) # save prediction errors in the fold
  errors <- c(errors, .errors) # aggregate error over folds.
}


r2.12<-c(r2.12,errors)
#New 5 model with CV, fold=5:

errors <- NULL

for (k in 1:folds){
  dv7.cross.train <- all.new[fold.assignment!=k,] # train subset
  dv7.cross.test <-  all.new[fold.assignment==k,] # test subset
  .ols <- lm(DV.7~V1+V2+V3+V4+V5 ,data = dv7.cross.train) # train
  .predictions <- predict(.ols, newdata=dv7.cross.test)
  .errors <-   R2(dv7.cross.test$DV.7, .predictions) # save prediction errors in the fold
  errors <- c(errors, .errors) # aggregate error over folds.
}


r2.12<-c(r2.12,errors)


errors <- NULL

for (k in 1:folds){
  dv7.cross.train <- all.new[fold.assignment!=k,] # train subset
  dv7.cross.test <-  all.new[fold.assignment==k,] # test subset
  .ols <- lm(DV.7~V1+V2+V3+V4+V5 ,data = dv7.cross.train) # train
  .predictions <- predict(.ols, newdata=dv7.cross.train)
  .errors <-   R2(dv7.cross.train$DV.7, .predictions) # save prediction errors in the fold
  errors <- c(errors, .errors) # aggregate error over folds.
}


r2.12<-c(r2.12,errors)

#Big5 model with CV, fold=5:

errors <- NULL

for (k in 1:folds){
  dv7.cross.train <- all.new[fold.assignment!=k,] # train subset
  dv7.cross.test <-  all.new[fold.assignment==k,] # test subset
  .ols <- lm(DV.7~O+C+E+A+N ,data = dv7.cross.train) # train
  .predictions <- predict(.ols, newdata=dv7.cross.test)
  .errors <-   R2(dv7.cross.test$DV.7, .predictions) # save prediction errors in the fold
  errors <- c(errors, .errors) # aggregate error over folds.
}


r2.12<-c(r2.12,errors)


errors <- NULL

for (k in 1:folds){
  dv7.cross.train <- all.new[fold.assignment!=k,] # train subset
  dv7.cross.test <-  all.new[fold.assignment==k,] # test subset
  .ols <- lm(DV.7~O+C+E+A+N ,data = dv7.cross.train) # train
  .predictions <- predict(.ols, newdata=dv7.cross.train)
  .errors <-   R2(dv7.cross.train$DV.7, .predictions) # save prediction errors in the fold
  errors <- c(errors, .errors) # aggregate error over folds.
}


r2.12<-c(r2.12,errors)

#PC5 model with CV, fold=5:

errors <- NULL

for (k in 1:folds){
  dv7.cross.train <- all.new[fold.assignment!=k,] # train subset
  dv7.cross.test <-  all.new[fold.assignment==k,] # test subset
  .ols <- lm(DV.7~PC1+PC2+PC3+PC4+PC5 ,data = dv7.cross.train) # train
  .predictions <- predict(.ols, newdata=dv7.cross.test)
  .errors <-   R2(dv7.cross.test$DV.7, .predictions) # save prediction errors in the fold
  errors <- c(errors, .errors) # aggregate error over folds.
}


r2.12<-c(r2.12,errors)


errors <- NULL

for (k in 1:folds){
  dv7.cross.train <- all.new[fold.assignment!=k,] # train subset
  dv7.cross.test <-  all.new[fold.assignment==k,] # test subset
  .ols <- lm(DV.7~PC1+PC2+PC3+PC4+PC5 ,data = dv7.cross.train) # train
  .predictions <- predict(.ols, newdata=dv7.cross.train)
  .errors <-   R2(dv7.cross.train$DV.7, .predictions) # save prediction errors in the fold
  errors <- c(errors, .errors) # aggregate error over folds.
}


r2.12<-c(r2.12,errors)

###################################DV8################################

#100Q with CV, Fold=4:
errors <- NULL

for (k in 1:folds){
  dv8.cross.train <- all.new[fold.assignment!=k,] # train subset
  dv8.cross.test <-  all.new[fold.assignment==k,] # test subset
  .ols <- lm(DV.8~q1+q2+q3+q4+q5+q6+q7+q8+q9+q10+q11+q12+q13+q14+q15+
               q16+q17+q18+q19+q20+q21+q22+q23+q24+q25+q26+q27+q28+
               q29+q30+q31+q32+q33+q34+q35+q36+q37+q38+q39+q40+q41+
               q42+q43+q44+q45+q46+q47+q48+q49+q50+q51+q52+q53+q54+
               q55+q56+q57+q58+q59+q60+q61+q62+q63+q64+q65+q66+q67+
               q68+q69+q70+q71+q72+q73+q74+q75+q76+q77+q78+q79+q80+
               q81+q82+q83+q84+q85+q86+q87+q88+q89+q90+q91+q92+q93+
               q94+q95+q96+q97+q98+q99+q100 ,data = dv8.cross.train) # train
  .predictions <- predict(.ols, newdata=dv8.cross.test)
  .errors <-   R2(dv8.cross.test$DV.8, .predictions) # save prediction errors in the fold
  errors <- c(errors, .errors) # aggregate error over folds.
}


r2.12<-c(r2.12,errors)


errors <- NULL

for (k in 1:folds){
  dv8.cross.train <- all.new[fold.assignment!=k,] # train subset
  dv8.cross.test <-  all.new[fold.assignment==k,] # test subset
  .ols <- lm(DV.8~q1+q2+q3+q4+q5+q6+q7+q8+q9+q10+q11+q12+q13+q14+q15+
               q16+q17+q18+q19+q20+q21+q22+q23+q24+q25+q26+q27+q28+
               q29+q30+q31+q32+q33+q34+q35+q36+q37+q38+q39+q40+q41+
               q42+q43+q44+q45+q46+q47+q48+q49+q50+q51+q52+q53+q54+
               q55+q56+q57+q58+q59+q60+q61+q62+q63+q64+q65+q66+q67+
               q68+q69+q70+q71+q72+q73+q74+q75+q76+q77+q78+q79+q80+
               q81+q82+q83+q84+q85+q86+q87+q88+q89+q90+q91+q92+q93+
               q94+q95+q96+q97+q98+q99+q100 ,data = dv8.cross.train) # train
  .predictions <- predict(.ols, newdata=dv8.cross.train)
  .errors <-   R2(dv8.cross.train$DV.8, .predictions) # save prediction errors in the fold
  errors <- c(errors, .errors) # aggregate error over folds.
}


r2.12<-c(r2.12,errors)
#New 5 model with CV, fold=5:

errors <- NULL

for (k in 1:folds){
  dv8.cross.train <- all.new[fold.assignment!=k,] # train subset
  dv8.cross.test <-  all.new[fold.assignment==k,] # test subset
  .ols <- lm(DV.8~V1+V2+V3+V4+V5 ,data = dv8.cross.train) # train
  .predictions <- predict(.ols, newdata=dv8.cross.test)
  .errors <-   R2(dv8.cross.test$DV.8, .predictions) # save prediction errors in the fold
  errors <- c(errors, .errors) # aggregate error over folds.
}


r2.12<-c(r2.12,errors)


errors <- NULL

for (k in 1:folds){
  dv8.cross.train <- all.new[fold.assignment!=k,] # train subset
  dv8.cross.test <-  all.new[fold.assignment==k,] # test subset
  .ols <- lm(DV.8~V1+V2+V3+V4+V5 ,data = dv8.cross.train) # train
  .predictions <- predict(.ols, newdata=dv8.cross.train)
  .errors <-   R2(dv8.cross.train$DV.8, .predictions) # save prediction errors in the fold
  errors <- c(errors, .errors) # aggregate error over folds.
}


r2.12<-c(r2.12,errors)

#Big5 model with CV, fold=5:

errors <- NULL

for (k in 1:folds){
  dv8.cross.train <- all.new[fold.assignment!=k,] # train subset
  dv8.cross.test <-  all.new[fold.assignment==k,] # test subset
  .ols <- lm(DV.8~O+C+E+A+N ,data = dv8.cross.train) # train
  .predictions <- predict(.ols, newdata=dv8.cross.test)
  .errors <-   R2(dv8.cross.test$DV.8, .predictions) # save prediction errors in the fold
  errors <- c(errors, .errors) # aggregate error over folds.
}


r2.12<-c(r2.12,errors)

errors <- NULL

for (k in 1:folds){
  dv8.cross.train <- all.new[fold.assignment!=k,] # train subset
  dv8.cross.test <-  all.new[fold.assignment==k,] # test subset
  .ols <- lm(DV.8~O+C+E+A+N ,data = dv8.cross.train) # train
  .predictions <- predict(.ols, newdata=dv8.cross.train)
  .errors <-   R2(dv8.cross.train$DV.8, .predictions) # save prediction errors in the fold
  errors <- c(errors, .errors) # aggregate error over folds.
}


r2.12<-c(r2.12,errors)

#PC5 model with CV, fold=5:

errors <- NULL

for (k in 1:folds){
  dv8.cross.train <- all.new[fold.assignment!=k,] # train subset
  dv8.cross.test <-  all.new[fold.assignment==k,] # test subset
  .ols <- lm(DV.8~PC1+PC2+PC3+PC4+PC5 ,data = dv8.cross.train) # train
  .predictions <- predict(.ols, newdata=dv8.cross.test)
  .errors <-   R2(dv8.cross.test$DV.8, .predictions) # save prediction errors in the fold
  errors <- c(errors, .errors) # aggregate error over folds.
}


r2.12<-c(r2.12,errors)


errors <- NULL

for (k in 1:folds){
  dv8.cross.train <- all.new[fold.assignment!=k,] # train subset
  dv8.cross.test <-  all.new[fold.assignment==k,] # test subset
  .ols <- lm(DV.8~PC1+PC2+PC3+PC4+PC5 ,data = dv8.cross.train) # train
  .predictions <- predict(.ols, newdata=dv8.cross.train)
  .errors <-   R2(dv8.cross.train$DV.8, .predictions) # save prediction errors in the fold
  errors <- c(errors, .errors) # aggregate error over folds.
}


r2.12<-c(r2.12,errors)

###################################DV9################################

#100Q with CV, Fold=4:

errors <- NULL

for (k in 1:folds){
  dv9.cross.train <- all.new[fold.assignment!=k,] # train subset
  dv9.cross.test <-  all.new[fold.assignment==k,] # test subset
  .ols <- lm(DV.9~q1+q2+q3+q4+q5+q6+q7+q8+q9+q10+q11+q12+q13+q14+q15+
               q16+q17+q18+q19+q20+q21+q22+q23+q24+q25+q26+q27+q28+
               q29+q30+q31+q32+q33+q34+q35+q36+q37+q38+q39+q40+q41+
               q42+q43+q44+q45+q46+q47+q48+q49+q50+q51+q52+q53+q54+
               q55+q56+q57+q58+q59+q60+q61+q62+q63+q64+q65+q66+q67+
               q68+q69+q70+q71+q72+q73+q74+q75+q76+q77+q78+q79+q80+
               q81+q82+q83+q84+q85+q86+q87+q88+q89+q90+q91+q92+q93+
               q94+q95+q96+q97+q98+q99+q100 ,data = dv9.cross.train) # train
  .predictions <- predict(.ols, newdata=dv9.cross.test)
  .errors <-   R2(dv9.cross.test$DV.9, .predictions) # save prediction errors in the fold
  errors <- c(errors, .errors) # aggregate error over folds.
}


r2.12<-c(r2.12,errors)

errors <- NULL

for (k in 1:folds){
  dv9.cross.train <- all.new[fold.assignment!=k,] # train subset
  dv9.cross.test <-  all.new[fold.assignment==k,] # test subset
  .ols <- lm(DV.9~q1+q2+q3+q4+q5+q6+q7+q8+q9+q10+q11+q12+q13+q14+q15+
               q16+q17+q18+q19+q20+q21+q22+q23+q24+q25+q26+q27+q28+
               q29+q30+q31+q32+q33+q34+q35+q36+q37+q38+q39+q40+q41+
               q42+q43+q44+q45+q46+q47+q48+q49+q50+q51+q52+q53+q54+
               q55+q56+q57+q58+q59+q60+q61+q62+q63+q64+q65+q66+q67+
               q68+q69+q70+q71+q72+q73+q74+q75+q76+q77+q78+q79+q80+
               q81+q82+q83+q84+q85+q86+q87+q88+q89+q90+q91+q92+q93+
               q94+q95+q96+q97+q98+q99+q100 ,data = dv9.cross.train) # train
  .predictions <- predict(.ols, newdata=dv9.cross.train)
  .errors <-   R2(dv9.cross.train$DV.9, .predictions) # save prediction errors in the fold
  errors <- c(errors, .errors) # aggregate error over folds.
}


r2.12<-c(r2.12,errors)
#New 5 model with CV, fold=5:

errors <- NULL

for (k in 1:folds){
  dv9.cross.train <- all.new[fold.assignment!=k,] # train subset
  dv9.cross.test <-  all.new[fold.assignment==k,] # test subset
  .ols <- lm(DV.9~V1+V2+V3+V4+V5 ,data = dv9.cross.train) # train
  .predictions <- predict(.ols, newdata=dv9.cross.test)
  .errors <-   R2(dv9.cross.test$DV.9, .predictions) # save prediction errors in the fold
  errors <- c(errors, .errors) # aggregate error over folds.
}


r2.12<-c(r2.12,errors)


errors <- NULL

for (k in 1:folds){
  dv9.cross.train <- all.new[fold.assignment!=k,] # train subset
  dv9.cross.test <-  all.new[fold.assignment==k,] # test subset
  .ols <- lm(DV.9~V1+V2+V3+V4+V5 ,data = dv9.cross.train) # train
  .predictions <- predict(.ols, newdata=dv9.cross.train)
  .errors <-   R2(dv9.cross.train$DV.9, .predictions) # save prediction errors in the fold
  errors <- c(errors, .errors) # aggregate error over folds.
}


r2.12<-c(r2.12,errors)

#Big5 model with CV, fold=5:
errors <- NULL

for (k in 1:folds){
  dv9.cross.train <- all.new[fold.assignment!=k,] # train subset
  dv9.cross.test <-  all.new[fold.assignment==k,] # test subset
  .ols <- lm(DV.9~O+C+E+A+N ,data = dv9.cross.train) # train
  .predictions <- predict(.ols, newdata=dv9.cross.test)
  .errors <-   R2(dv9.cross.test$DV.9, .predictions) # save prediction errors in the fold
  errors <- c(errors, .errors) # aggregate error over folds.
}


r2.12<-c(r2.12,errors)



errors <- NULL

for (k in 1:folds){
  dv9.cross.train <- all.new[fold.assignment!=k,] # train subset
  dv9.cross.test <-  all.new[fold.assignment==k,] # test subset
  .ols <- lm(DV.9~O+C+E+A+N ,data = dv9.cross.train) # train
  .predictions <- predict(.ols, newdata=dv9.cross.train)
  .errors <-   R2(dv9.cross.train$DV.9, .predictions) # save prediction errors in the fold
  errors <- c(errors, .errors) # aggregate error over folds.
}


r2.12<-c(r2.12,errors)

#PC5 model with CV, fold=5:

errors <- NULL

for (k in 1:folds){
  dv9.cross.train <- all.new[fold.assignment!=k,] # train subset
  dv9.cross.test <-  all.new[fold.assignment==k,] # test subset
  .ols <- lm(DV.9~PC1+PC2+PC3+PC4+PC5 ,data = dv9.cross.train) # train
  .predictions <- predict(.ols, newdata=dv9.cross.test)
  .errors <-   R2(dv9.cross.test$DV.9, .predictions) # save prediction errors in the fold
  errors <- c(errors, .errors) # aggregate error over folds.
}


r2.12<-c(r2.12,errors)


errors <- NULL

for (k in 1:folds){
  dv9.cross.train <- all.new[fold.assignment!=k,] # train subset
  dv9.cross.test <-  all.new[fold.assignment==k,] # test subset
  .ols <- lm(DV.9~PC1+PC2+PC3+PC4+PC5 ,data = dv9.cross.train) # train
  .predictions <- predict(.ols, newdata=dv9.cross.train)
  .errors <-   R2(dv9.cross.train$DV.9, .predictions) # save prediction errors in the fold
  errors <- c(errors, .errors) # aggregate error over folds.
}


r2.12<-c(r2.12,errors)

###################################DV10################################
#100Q with CV, Fold=4:
errors <- NULL

for (k in 1:folds){
  dv10.cross.train <- all.new[fold.assignment!=k,] # train subset
  dv10.cross.test <-  all.new[fold.assignment==k,] # test subset
  .ols <- lm(DV.10~q1+q2+q3+q4+q5+q6+q7+q8+q9+q10+q11+q12+q13+q14+q15+
               q16+q17+q18+q19+q20+q21+q22+q23+q24+q25+q26+q27+q28+
               q29+q30+q31+q32+q33+q34+q35+q36+q37+q38+q39+q40+q41+
               q42+q43+q44+q45+q46+q47+q48+q49+q50+q51+q52+q53+q54+
               q55+q56+q57+q58+q59+q60+q61+q62+q63+q64+q65+q66+q67+
               q68+q69+q70+q71+q72+q73+q74+q75+q76+q77+q78+q79+q80+
               q81+q82+q83+q84+q85+q86+q87+q88+q89+q90+q91+q92+q93+
               q94+q95+q96+q97+q98+q99+q100 ,data = dv10.cross.train) # train
  .predictions <- predict(.ols, newdata=dv10.cross.test)
  .errors <-   R2(dv10.cross.test$DV.10, .predictions) # save prediction errors in the fold
  errors <- c(errors, .errors) # aggregate error over folds.
}


r2.12<-c(r2.12,errors)

errors <- NULL

for (k in 1:folds){
  dv10.cross.train <- all.new[fold.assignment!=k,] # train subset
  dv10.cross.test <-  all.new[fold.assignment==k,] # test subset
  .ols <- lm(DV.10~q1+q2+q3+q4+q5+q6+q7+q8+q9+q10+q11+q12+q13+q14+q15+
               q16+q17+q18+q19+q20+q21+q22+q23+q24+q25+q26+q27+q28+
               q29+q30+q31+q32+q33+q34+q35+q36+q37+q38+q39+q40+q41+
               q42+q43+q44+q45+q46+q47+q48+q49+q50+q51+q52+q53+q54+
               q55+q56+q57+q58+q59+q60+q61+q62+q63+q64+q65+q66+q67+
               q68+q69+q70+q71+q72+q73+q74+q75+q76+q77+q78+q79+q80+
               q81+q82+q83+q84+q85+q86+q87+q88+q89+q90+q91+q92+q93+
               q94+q95+q96+q97+q98+q99+q100 ,data = dv10.cross.train) # train
  .predictions <- predict(.ols, newdata=dv10.cross.train)
  .errors <-   R2(dv10.cross.train$DV.10, .predictions) # save prediction errors in the fold
  errors <- c(errors, .errors) # aggregate error over folds.
}


r2.12<-c(r2.12,errors)

#New 5 model with CV, fold=5:

errors <- NULL

for (k in 1:folds){
  dv10.cross.train <- all.new[fold.assignment!=k,] # train subset
  dv10.cross.test <-  all.new[fold.assignment==k,] # test subset
  .ols <- lm(DV.10~V1+V2+V3+V4+V5 ,data = dv10.cross.train) # train
  .predictions <- predict(.ols, newdata=dv10.cross.test)
  .errors <-   R2(dv10.cross.test$DV.10, .predictions) # save prediction errors in the fold
  errors <- c(errors, .errors) # aggregate error over folds.
}


r2.12<-c(r2.12,errors)


errors <- NULL

for (k in 1:folds){
  dv10.cross.train <- all.new[fold.assignment!=k,] # train subset
  dv10.cross.test <-  all.new[fold.assignment==k,] # test subset
  .ols <- lm(DV.10~V1+V2+V3+V4+V5 ,data = dv10.cross.train) # train
  .predictions <- predict(.ols, newdata=dv10.cross.train)
  .errors <-   R2(dv10.cross.train$DV.10, .predictions) # save prediction errors in the fold
  errors <- c(errors, .errors) # aggregate error over folds.
}


r2.12<-c(r2.12,errors)

#Big5 model with CV, fold=5:

errors <- NULL

for (k in 1:folds){
  dv10.cross.train <- all.new[fold.assignment!=k,] # train subset
  dv10.cross.test <-  all.new[fold.assignment==k,] # test subset
  .ols <- lm(DV.10~O+C+E+A+N ,data = dv10.cross.train) # train
  .predictions <- predict(.ols, newdata=dv10.cross.test)
  .errors <-   R2(dv10.cross.test$DV.10, .predictions) # save prediction errors in the fold
  errors <- c(errors, .errors) # aggregate error over folds.
}


r2.12<-c(r2.12,errors)



errors <- NULL

for (k in 1:folds){
  dv10.cross.train <- all.new[fold.assignment!=k,] # train subset
  dv10.cross.test <-  all.new[fold.assignment==k,] # test subset
  .ols <- lm(DV.10~O+C+E+A+N ,data = dv10.cross.train) # train
  .predictions <- predict(.ols, newdata=dv10.cross.train)
  .errors <-   R2(dv10.cross.train$DV.10, .predictions) # save prediction errors in the fold
  errors <- c(errors, .errors) # aggregate error over folds.
}


r2.12<-c(r2.12,errors)

#PC5 model with CV, fold=5:

errors <- NULL

for (k in 1:folds){
  dv10.cross.train <- all.new[fold.assignment!=k,] # train subset
  dv10.cross.test <-  all.new[fold.assignment==k,] # test subset
  .ols <- lm(DV.10~PC1+PC2+PC3+PC4+PC5 ,data = dv10.cross.train) # train
  .predictions <- predict(.ols, newdata=dv10.cross.test)
  .errors <-   R2(dv10.cross.test$DV.10, .predictions) # save prediction errors in the fold
  errors <- c(errors, .errors) # aggregate error over folds.
}


r2.12<-c(r2.12,errors)


errors <- NULL

for (k in 1:folds){
  dv10.cross.train <- all.new[fold.assignment!=k,] # train subset
  dv10.cross.test <-  all.new[fold.assignment==k,] # test subset
  .ols <- lm(DV.10~PC1+PC2+PC3+PC4+PC5 ,data = dv10.cross.train) # train
  .predictions <- predict(.ols, newdata=dv10.cross.train)
  .errors <-   R2(dv10.cross.train$DV.10, .predictions) # save prediction errors in the fold
  errors <- c(errors, .errors) # aggregate error over folds.
}


r2.12<-c(r2.12,errors)

###################################DV11################################
#100Q with CV, Fold=4:
errors <- NULL

for (k in 1:folds){
  dv11.cross.train <- all.new[fold.assignment!=k,] # train subset
  dv11.cross.test <-  all.new[fold.assignment==k,] # test subset
  .ols <- lm(DV.11~q1+q2+q3+q4+q5+q6+q7+q8+q9+q10+q11+q12+q13+q14+q15+
               q16+q17+q18+q19+q20+q21+q22+q23+q24+q25+q26+q27+q28+
               q29+q30+q31+q32+q33+q34+q35+q36+q37+q38+q39+q40+q41+
               q42+q43+q44+q45+q46+q47+q48+q49+q50+q51+q52+q53+q54+
               q55+q56+q57+q58+q59+q60+q61+q62+q63+q64+q65+q66+q67+
               q68+q69+q70+q71+q72+q73+q74+q75+q76+q77+q78+q79+q80+
               q81+q82+q83+q84+q85+q86+q87+q88+q89+q90+q91+q92+q93+
               q94+q95+q96+q97+q98+q99+q100 ,data = dv11.cross.train) # train
  .predictions <- predict(.ols, newdata=dv11.cross.test)
  .errors <-   R2(dv11.cross.test$DV.11, .predictions) # save prediction errors in the fold
  errors <- c(errors, .errors) # aggregate error over folds.
}


r2.12<-c(r2.12,errors)


errors <- NULL

for (k in 1:folds){
  dv11.cross.train <- all.new[fold.assignment!=k,] # train subset
  dv11.cross.test <-  all.new[fold.assignment==k,] # test subset
  .ols <- lm(DV.11~q1+q2+q3+q4+q5+q6+q7+q8+q9+q10+q11+q12+q13+q14+q15+
               q16+q17+q18+q19+q20+q21+q22+q23+q24+q25+q26+q27+q28+
               q29+q30+q31+q32+q33+q34+q35+q36+q37+q38+q39+q40+q41+
               q42+q43+q44+q45+q46+q47+q48+q49+q50+q51+q52+q53+q54+
               q55+q56+q57+q58+q59+q60+q61+q62+q63+q64+q65+q66+q67+
               q68+q69+q70+q71+q72+q73+q74+q75+q76+q77+q78+q79+q80+
               q81+q82+q83+q84+q85+q86+q87+q88+q89+q90+q91+q92+q93+
               q94+q95+q96+q97+q98+q99+q100 ,data = dv11.cross.train) # train
  .predictions <- predict(.ols, newdata=dv11.cross.train)
  .errors <-   R2(dv11.cross.train$DV.11, .predictions) # save prediction errors in the fold
  errors <- c(errors, .errors) # aggregate error over folds.
}


r2.12<-c(r2.12,errors)

#New 5 model with CV, fold=5:
errors <- NULL

for (k in 1:folds){
  dv11.cross.train <- all.new[fold.assignment!=k,] # train subset
  dv11.cross.test <-  all.new[fold.assignment==k,] # test subset
  .ols <- lm(DV.11~V1+V2+V3+V4+V5 ,data = dv11.cross.train) # train
  .predictions <- predict(.ols, newdata=dv11.cross.test)
  .errors <-   R2(dv11.cross.test$DV.11, .predictions) # save prediction errors in the fold
  errors <- c(errors, .errors) # aggregate error over folds.
}


r2.12<-c(r2.12,errors)


errors <- NULL

for (k in 1:folds){
  dv11.cross.train <- all.new[fold.assignment!=k,] # train subset
  dv11.cross.test <-  all.new[fold.assignment==k,] # test subset
  .ols <- lm(DV.11~V1+V2+V3+V4+V5 ,data = dv11.cross.train) # train
  .predictions <- predict(.ols, newdata=dv11.cross.train)
  .errors <-   R2(dv11.cross.train$DV.11, .predictions) # save prediction errors in the fold
  errors <- c(errors, .errors) # aggregate error over folds.
}


r2.12<-c(r2.12,errors)

#Big5 model with CV, fold=5:

errors <- NULL

for (k in 1:folds){
  dv11.cross.train <- all.new[fold.assignment!=k,] # train subset
  dv11.cross.test <-  all.new[fold.assignment==k,] # test subset
  .ols <- lm(DV.11~O+C+E+A+N ,data = dv11.cross.train) # train
  .predictions <- predict(.ols, newdata=dv11.cross.test)
  .errors <-   R2(dv11.cross.test$DV.11, .predictions) # save prediction errors in the fold
  errors <- c(errors, .errors) # aggregate error over folds.
}


r2.12<-c(r2.12,errors)



errors <- NULL

for (k in 1:folds){
  dv11.cross.train <- all.new[fold.assignment!=k,] # train subset
  dv11.cross.test <-  all.new[fold.assignment==k,] # test subset
  .ols <- lm(DV.11~O+C+E+A+N ,data = dv11.cross.train) # train
  .predictions <- predict(.ols, newdata=dv11.cross.train)
  .errors <-   R2(dv11.cross.train$DV.11, .predictions) # save prediction errors in the fold
  errors <- c(errors, .errors) # aggregate error over folds.
}


r2.12<-c(r2.12,errors)

#PC5 model with CV, fold=5:

errors <- NULL

for (k in 1:folds){
  dv11.cross.train <- all.new[fold.assignment!=k,] # train subset
  dv11.cross.test <-  all.new[fold.assignment==k,] # test subset
  .ols <- lm(DV.11~PC1+PC2+PC3+PC4+PC5 ,data = dv11.cross.train) # train
  .predictions <- predict(.ols, newdata=dv11.cross.test)
  .errors <-   R2(dv11.cross.test$DV.11, .predictions) # save prediction errors in the fold
  errors <- c(errors, .errors) # aggregate error over folds.
}


r2.12<-c(r2.12,errors)


errors <- NULL

for (k in 1:folds){
  dv11.cross.train <- all.new[fold.assignment!=k,] # train subset
  dv11.cross.test <-  all.new[fold.assignment==k,] # test subset
  .ols <- lm(DV.11~PC1+PC2+PC3+PC4+PC5 ,data = dv11.cross.train) # train
  .predictions <- predict(.ols, newdata=dv11.cross.train)
  .errors <-   R2(dv11.cross.train$DV.11, .predictions) # save prediction errors in the fold
  errors <- c(errors, .errors) # aggregate error over folds.
}


r2.12<-c(r2.12,errors)

###################################DV12################################

#100Q with CV, Fold=4:
errors <- NULL

for (k in 1:folds){
  dv12.cross.train <- all.new[fold.assignment!=k,] # train subset
  dv12.cross.test <-  all.new[fold.assignment==k,] # test subset
  .ols <- lm(DV.12~q1+q2+q3+q4+q5+q6+q7+q8+q9+q10+q11+q12+q13+q14+q15+
               q16+q17+q18+q19+q20+q21+q22+q23+q24+q25+q26+q27+q28+
               q29+q30+q31+q32+q33+q34+q35+q36+q37+q38+q39+q40+q41+
               q42+q43+q44+q45+q46+q47+q48+q49+q50+q51+q52+q53+q54+
               q55+q56+q57+q58+q59+q60+q61+q62+q63+q64+q65+q66+q67+
               q68+q69+q70+q71+q72+q73+q74+q75+q76+q77+q78+q79+q80+
               q81+q82+q83+q84+q85+q86+q87+q88+q89+q90+q91+q92+q93+
               q94+q95+q96+q97+q98+q99+q100 ,data = dv12.cross.train) # train
  .predictions <- predict(.ols, newdata=dv12.cross.test)
  .errors <-   R2(dv12.cross.test$DV.12, .predictions) # save prediction errors in the fold
  errors <- c(errors, .errors) # aggregate error over folds.
}


r2.12<-c(r2.12,errors)

errors <- NULL

for (k in 1:folds){
  dv12.cross.train <- all.new[fold.assignment!=k,] # train subset
  dv12.cross.test <-  all.new[fold.assignment==k,] # test subset
  .ols <- lm(DV.12~q1+q2+q3+q4+q5+q6+q7+q8+q9+q10+q11+q12+q13+q14+q15+
               q16+q17+q18+q19+q20+q21+q22+q23+q24+q25+q26+q27+q28+
               q29+q30+q31+q32+q33+q34+q35+q36+q37+q38+q39+q40+q41+
               q42+q43+q44+q45+q46+q47+q48+q49+q50+q51+q52+q53+q54+
               q55+q56+q57+q58+q59+q60+q61+q62+q63+q64+q65+q66+q67+
               q68+q69+q70+q71+q72+q73+q74+q75+q76+q77+q78+q79+q80+
               q81+q82+q83+q84+q85+q86+q87+q88+q89+q90+q91+q92+q93+
               q94+q95+q96+q97+q98+q99+q100 ,data = dv12.cross.train) # train
  .predictions <- predict(.ols, newdata=dv12.cross.train)
  .errors <-   R2(dv12.cross.train$DV.12, .predictions) # save prediction errors in the fold
  errors <- c(errors, .errors) # aggregate error over folds.
}


r2.12<-c(r2.12,errors)

#new5 model with 
errors <- NULL

for (k in 1:folds){
  dv12.cross.train <- all.new[fold.assignment!=k,] # train subset
  dv12.cross.test <-  all.new[fold.assignment==k,] # test subset
  .ols <- lm(DV.12~V1+V2+V3+V4+V5 ,data = dv12.cross.train) # train
  .predictions <- predict(.ols, newdata=dv12.cross.test)
  .errors <-   R2(dv12.cross.test$DV.12, .predictions) # save prediction errors in the fold
  errors <- c(errors, .errors) # aggregate error over folds.
}


r2.12<-c(r2.12,errors)


errors <- NULL

for (k in 1:folds){
  dv12.cross.train <- all.new[fold.assignment!=k,] # train subset
  dv12.cross.test <-  all.new[fold.assignment==k,] # test subset
  .ols <- lm(DV.12~V1+V2+V3+V4+V5 ,data = dv12.cross.train) # train
  .predictions <- predict(.ols, newdata=dv12.cross.train)
  .errors <-   R2(dv12.cross.train$DV.12, .predictions) # save prediction errors in the fold
  errors <- c(errors, .errors) # aggregate error over folds.
}


r2.12<-c(r2.12,errors)

#Big5 model with CV, fold=5:

errors <- NULL

for (k in 1:folds){
  dv12.cross.train <- all.new[fold.assignment!=k,] # train subset
  dv12.cross.test <-  all.new[fold.assignment==k,] # test subset
  .ols <- lm(DV.12~O+C+E+A+N ,data = dv12.cross.train) # train
  .predictions <- predict(.ols, newdata=dv12.cross.test)
  .errors <-   R2(dv12.cross.test$DV.12, .predictions) # save prediction errors in the fold
  errors <- c(errors, .errors) # aggregate error over folds.
}


r2.12<-c(r2.12,errors)



errors <- NULL

for (k in 1:folds){
  dv12.cross.train <- all.new[fold.assignment!=k,] # train subset
  dv12.cross.test <-  all.new[fold.assignment==k,] # test subset
  .ols <- lm(DV.12~O+C+E+A+N ,data = dv12.cross.train) # train
  .predictions <- predict(.ols, newdata=dv12.cross.train)
  .errors <-   R2(dv12.cross.train$DV.12, .predictions) # save prediction errors in the fold
  errors <- c(errors, .errors) # aggregate error over folds.
}


r2.12<-c(r2.12,errors)

#PC5 model with CV, fold=5:

errors <- NULL

for (k in 1:folds){
  dv12.cross.train <- all.new[fold.assignment!=k,] # train subset
  dv12.cross.test <-  all.new[fold.assignment==k,] # test subset
  .ols <- lm(DV.12~PC1+PC2+PC3+PC4+PC5 ,data = dv12.cross.train) # train
  .predictions <- predict(.ols, newdata=dv12.cross.test)
  .errors <-   R2(dv12.cross.test$DV.12, .predictions) # save prediction errors in the fold
  errors <- c(errors, .errors) # aggregate error over folds.
}


r2.12<-c(r2.12,errors)


errors <- NULL

for (k in 1:folds){
  dv12.cross.train <- all.new[fold.assignment!=k,] # train subset
  dv12.cross.test <-  all.new[fold.assignment==k,] # test subset
  .ols <- lm(DV.12~PC1+PC2+PC3+PC4+PC5 ,data = dv12.cross.train) # train
  .predictions <- predict(.ols, newdata=dv12.cross.train)
  .errors <-   R2(dv12.cross.train$DV.12, .predictions) # save prediction errors in the fold
  errors <- c(errors, .errors) # aggregate error over folds.
}


r2.12<-c(r2.12,errors)

###########################combine:


newdata.cv.results$r2<- r2.12
View(newdata.cv.results)
sum.table.12<- newdata.cv.results[,.(Mean=mean(r2)), by=.(Type,Set,dv)]
sum.table.12.test.long<- as.data.table(sum.table.12[sum.table.12$Set=="Test"])
sum.table.12.test.wide<- dcast(sum.table.12.test.long, dv~Type, value.var="Mean")

sum.table.12.test.wide$diff.big5<- sum.table.12.test.wide$`Predictive Five`-sum.table.12.test.wide$`Big Five`
sum.table.12.test.wide$Improvement<- sum.table.12.test.wide$diff.big5/sum.table.12.test.wide$`Big Five`

mean(sum.table.12.test.wide$`Predictive Five`)
mean(sum.table.12.test.wide$`Big Five`)
mean(sum.table.12.test.wide$`PC Five`)

fold.4.ten.dv.results<-ten.dv.results