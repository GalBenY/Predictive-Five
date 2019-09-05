###New five- train and predict- all on the test set######

#Create a rotation matrix from the RRR model:
rotation.mat <- as.matrix(rrpack.model$coef.ls %*% rrpack.model$A)
pc5<-as.matrix(PC$rotation[,1:5])
dim(rotation.mat)

###################################IQ################################

iq.100<- as.matrix(test.iq[,q1:q100])
dim(iq.100)

iq.new5<- iq.100%*% rotation.mat
iq.pca<- iq.100%*%pc5
iq.new5.all<- as.data.table(cbind(test.iq, iq.new5,iq.pca ))
dim(iq.new5.all)
names(iq.new5.all)

set.seed(1)
folds <- 4
fold.assignment <- sample(1:folds, nrow(iq.new5.all), replace = TRUE)
table(fold.assignment)

#New 5 model with CV, fold=5:

errors <- NULL
for (k in 1:folds){
  iq.cross.train <- iq.new5.all[fold.assignment!=k,] # train subset
  iq.cross.test <-  iq.new5.all[fold.assignment==k,] # test subset
  .ols <- lm(iq~V1+V2+V3+V4+V5+V6+V7+V8+V9+V10 ,data = iq.cross.train) # train
  .predictions <- predict(.ols, newdata=iq.cross.test)
  .errors <-   R2(iq.cross.test$iq, .predictions) # save prediction errors in the fold
  errors <- c(errors, .errors) # aggregate error over folds.
}


mean(errors)



###################################swl################################

swl.100<- as.matrix(test.swl[,q1:q100])
dim(swl.100)

swl.new5<- swl.100%*% rotation.mat
swl.pca<- swl.100%*%pc5
swl.new5.all<- as.data.table(cbind(test.swl, swl.new5,swl.pca ))
dim(swl.new5.all)
names(swl.new5.all)


set.seed(1)
folds <-4
fold.assignment <- sample(1:folds, nrow(swl.new5.all), replace = TRUE)

#New 5 model with CV, fold=4:

errors <- NULL

for (k in 1:folds){
  swl.cross.train <- swl.new5.all[fold.assignment!=k,] # train subset
  swl.cross.test <-  swl.new5.all[fold.assignment==k,] # test subset
  .ols <- lm(swl~V1+V2+V3+V4+V5+V6+V7+V8+V9+V10 ,data = swl.cross.train) # train
  .predictions <- predict(.ols, newdata=swl.cross.test)
  .errors <-   R2(swl.cross.test$swl, .predictions) # save prediction errors in the fold
  errors <- c(errors, .errors) # aggregate error over folds.
}


mean(errors)

###################################cesd################################

cesd.100<- as.matrix(test.cesd[,q1:q100])
dim(cesd.100)

cesd.new5<- cesd.100%*% rotation.mat
cesd.pca<- cesd.100%*%pc5
cesd.new5.all<- as.data.table(cbind(test.cesd, cesd.new5,cesd.pca ))
dim(cesd.new5.all)
names(cesd.new5.all)

set.seed(1)
folds <- 4
fold.assignment <- sample(1:folds, nrow(cesd.new5.all), replace = TRUE)

#New 5 model with CV, fold=5:

errors <- NULL

for (k in 1:folds){
  cesd.cross.train <- cesd.new5.all[fold.assignment!=k,] # train subset
  cesd.cross.test <-  cesd.new5.all[fold.assignment==k,] # test subset
  .ols <- lm(score.cesd~V1+V2+V3+V4+V5+V6+V7+V8+V9+V10 ,data = cesd.cross.train) # train
  .predictions <- predict(.ols, newdata=cesd.cross.test)
  .errors <-   R2(cesd.cross.test$score.cesd, .predictions) # save prediction errors in the fold
  errors <- c(errors, .errors) # aggregate error over folds.
}


mean(errors)


###################################liberal################################

liberal.100<- as.matrix(test.liberal[,q1:q100])
dim(liberal.100)

liberal.new5<- liberal.100%*% rotation.mat
liberal.pca<- liberal.100%*%pc5
liberal.new5.all<- as.data.table(cbind(test.liberal, liberal.new5,liberal.pca ))
dim(liberal.new5.all)
names(liberal.new5.all)


set.seed(1)
folds <- 4
fold.assignment <- sample(
  1:folds, nrow(liberal.new5.all), replace = TRUE)

#New 5 model with CV, fold=5:

errors <- NULL

for (k in 1:folds){
  liberal.cross.train <- liberal.new5.all[fold.assignment!=k,] # train subset
  liberal.cross.test <-  liberal.new5.all[fold.assignment==k,] # test subset
  .ols <- lm(liberalness~V1+V2+V3+V4+V5+V6+V7+V8+V9+V10 ,data = liberal.cross.train) # train
  .predictions <- predict(.ols, newdata=liberal.cross.test)
  .errors <-   R2(liberal.cross.test$liberalness, .predictions) # save prediction errors in the fold
  errors <- c(errors, .errors) # aggregate error over folds.
}


mean(errors)


###################################risky################################

risky.100<- as.matrix(test.risky[,q1:q100])
dim(risky.100)

risky.new5<- risky.100%*% rotation.mat
risky.pca<- risky.100%*%pc5
risky.new5.all<- as.data.table(cbind(test.risky, risky.new5,risky.pca ))
dim(risky.new5.all)
names(risky.new5.all)

set.seed(1)
folds <- 4
fold.assignment <- sample(
  1:folds, nrow(risky.new5.all), replace = TRUE)

#New 5 model with CV, fold=5:

errors <- NULL

for (k in 1:folds){
  risky.cross.train <- risky.new5.all[fold.assignment!=k,] # train subset
  risky.cross.test <-  risky.new5.all[fold.assignment==k,] # test subset
  .ols <- lm(risky~V1+V2+V3+V4+V5+V6+V7+V8+V9+V10 ,data = risky.cross.train) # train
  .predictions <- predict(.ols, newdata=risky.cross.test)
  .errors <-   R2(risky.cross.test$risky, .predictions) # save prediction errors in the fold
  errors <- c(errors, .errors) # aggregate error over folds.
}


mean(errors)

###################################network################################

network.100<- as.matrix(test.network[,q1:q100])
dim(network.100)

network.new5<- network.100%*% rotation.mat
network.pca<- network.100%*%pc5
network.new5.all<- as.data.table(cbind(test.network, network.new5,network.pca ))
dim(network.new5.all)
names(network.new5.all)

set.seed(1)
folds <- 4
fold.assignment <- sample(
  1:folds, nrow(network.new5.all), replace = TRUE)
#New 5 model with CV, fold=5:

errors <- NULL

for (k in 1:folds){
  network.cross.train <- network.new5.all[fold.assignment!=k,] # train subset
  network.cross.test <-  network.new5.all[fold.assignment==k,] # test subset
  .ols <- lm(network_size~V1+V2+V3+V4+V5+V6+V7+V8+V9+V10 ,data = network.cross.train) # train
  .predictions <- predict(.ols, newdata=network.cross.test)
  .errors <-   R2(network.cross.test$network_size, .predictions) # save prediction errors in the fold
  errors <- c(errors, .errors) # aggregate error over folds.
}


mean(errors)


###################################nonhealthy################################

nonhealthy.100<- as.matrix(test.nonhealthy[,q1:q100])
dim(nonhealthy.100)

nonhealthy.new5<- nonhealthy.100%*% rotation.mat
nonhealthy.pca<- nonhealthy.100%*%pc5
nonhealthy.new5.all<- as.data.table(cbind(test.nonhealthy, nonhealthy.new5,nonhealthy.pca ))
dim(nonhealthy.new5.all)
names(nonhealthy.new5.all)

set.seed(1)
folds <- 4
fold.assignment <- sample(
  1:folds, nrow(nonhealthy.new5.all), replace = TRUE)

#New 5 model with CV, fold=5:

errors <- NULL

for (k in 1:folds){
  nonhealthy.cross.train <- nonhealthy.new5.all[fold.assignment!=k,] # train subset
  nonhealthy.cross.test <-  nonhealthy.new5.all[fold.assignment==k,] # test subset
  .ols <- lm(nonhealthy~V1+V2+V3+V4+V5+V6+V7+V8+V9+V10 ,data = nonhealthy.cross.train) # train
  .predictions <- predict(.ols, newdata=nonhealthy.cross.test)
  .errors <-   R2(nonhealthy.cross.test$nonhealthy, .predictions) # save prediction errors in the fold
  errors <- c(errors, .errors) # aggregate error over folds.
}


mean(errors)


###################################svq.trans################################

svq.trans.100<- as.matrix(test.svq.trans[,q1:q100])
dim(svq.trans.100)

svq.trans.new5<- svq.trans.100%*% rotation.mat
svq.trans.pca<- svq.trans.100%*%pc5
svq.trans.new5.all<- as.data.table(cbind(test.svq.trans, svq.trans.new5,svq.trans.pca ))
dim(svq.trans.new5.all)
names(svq.trans.new5.all)

set.seed(1)
folds <- 4
fold.assignment <- sample(
  1:folds, nrow(svq.trans.new5.all), replace = TRUE)

#New 5 model with CV, fold=5:

errors <- NULL

for (k in 1:folds){
  svq.trans.cross.train <- svq.trans.new5.all[fold.assignment!=k,] # train subset
  svq.trans.cross.test <-  svq.trans.new5.all[fold.assignment==k,] # test subset
  .ols <- lm(sv.trans~V1+V2+V3+V4+V5+V6+V7+V8+V9+V10 ,data = svq.trans.cross.train) # train
  .predictions <- predict(.ols, newdata=svq.trans.cross.test)
  .errors <-   R2(svq.trans.cross.test$sv.trans, .predictions) # save prediction errors in the fold
  errors <- c(errors, .errors) # aggregate error over folds.
}


mean(errors)

###################################svq.openness################################

svq.openness.100<- as.matrix(test.svq.openness[,q1:q100])
dim(svq.openness.100)

svq.openness.new5<- svq.openness.100%*% rotation.mat
svq.openness.pca<- svq.openness.100%*%pc5
svq.openness.new5.all<- as.data.table(cbind(test.svq.openness, svq.openness.new5,svq.openness.pca ))
dim(svq.openness.new5.all)
names(svq.openness.new5.all)

set.seed(1)
folds <- 4
fold.assignment <- sample(
  1:folds, nrow(svq.openness.new5.all), replace = TRUE)

#New 5 model with CV, fold=5:

errors <- NULL

for (k in 1:folds){
  svq.openness.cross.train <- svq.openness.new5.all[fold.assignment!=k,] # train subset
  svq.openness.cross.test <-  svq.openness.new5.all[fold.assignment==k,] # test subset
  .ols <- lm(sv.openness~V1+V2+V3+V4+V5+V6+V7+V8+V9+V10 ,data = svq.openness.cross.train) # train
  .predictions <- predict(.ols, newdata=svq.openness.cross.test)
  .errors <-   R2(svq.openness.cross.test$sv.openness, .predictions) # save prediction errors in the fold
  errors <- c(errors, .errors) # aggregate error over folds.
}


mean(errors)

###################################empathy################################

empathy.100<- as.matrix(test.empathy[,q1:q100])
dim(empathy.100)

empathy.new5<- empathy.100%*% rotation.mat
empathy.pca<- empathy.100%*%pc5
empathy.new5.all<- as.data.table(cbind(test.empathy, empathy.new5,empathy.pca ))
dim(empathy.new5.all)
names(empathy.new5.all)

set.seed(1)
folds <- 4
fold.assignment <- sample(
  1:folds, nrow(empathy.new5.all), replace = TRUE)

#New 5 model with CV, fold=5:

errors <- NULL

for (k in 1:folds){
  empathy.cross.train <- empathy.new5.all[fold.assignment!=k,] # train subset
  empathy.cross.test <-  empathy.new5.all[fold.assignment==k,] # test subset
  .ols <- lm(empathy~V1+V2+V3+V4+V5+V6+V7+V8+V9+V10,data = empathy.cross.train) # train
  .predictions <- predict(.ols, newdata=empathy.cross.test)
  .errors <-   R2(empathy.cross.test$empathy, .predictions) # save prediction errors in the fold
  errors <- c(errors, .errors) # aggregate error over folds.
}


mean(errors)

