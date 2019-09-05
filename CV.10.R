###New five- train and predict- all on the test set######

######create the PCA object and the PC5:

my_data<-all.NA[,-c("userid","O","C","E","A","N")]
dim(my_data)
PC <-prcomp(my_data)
summary(PC)
pc5<-as.matrix(PC$rotation[,1:5])

#Create a rotation matrix from the RRR model:
rotation.mat <- as.matrix(rrpack.model$coef.ls %*% rrpack.model$A)
pc5<-as.matrix(PC$rotation[,1:5])
dim(rotation.mat)

#create table for the results:

ten.dv.results<- data.table(dv=rep(c("Intelligence", "Well-being", "Depression",
                                    "Political Orientation", "Risky Behavior", "Number of Facebook Friends",
                                    "Nonhealthy Behavior", "Transcendence Values", "Openness Values",
                                    "Empathy"), each=24), 
                           Type= rep(c("Predictive Five", "Big Five", "PC Five"), each=8, times= 10),
                           Set= rep(c("Test", "Train"),each=4, times= 30))

View(ten.dv.results)

r2<-NULL
sd<-NULL

###################################IQ################################

iq.100<- as.matrix(test.iq[,q1:q100])
dim(iq.100)

iq.new5<- iq.100%*% rotation.mat
iq.pca<- iq.100%*%pc5
iq.new5.all<- as.data.table(cbind(test.iq, iq.new5,iq.pca ))
dim(iq.new5.all)
names(iq.new5.all)

set.seed(10)
folds <- 4
fold.assignment <- sample(1:folds, nrow(iq.new5.all), replace = TRUE)


#new5 model with CV, fold=5:

errors <- NULL

for (k in 1:folds){
  iq.cross.train <- iq.new5.all[fold.assignment!=k,] # train subset
  iq.cross.test <-  iq.new5.all[fold.assignment==k,] # test subset
  .ols <- lm(iq~V1+V2+V3+V4+V5  ,data = iq.cross.train) # train
  .predictions <- predict(.ols, newdata=iq.cross.test)
  .errors <-   R2(iq.cross.test$iq, .predictions) # save prediction errors in the fold
  errors <- c(errors, .errors) # aggregate error over folds.
}
mean(errors)
r2<- c(r2, errors)


errors <- NULL

for (k in 1:folds){
  iq.cross.train <- iq.new5.all[fold.assignment!=k,] # train subset
  iq.cross.test <-  iq.new5.all[fold.assignment==k,] # test subset
  .ols <- lm(iq~V1+V2+V3+V4+V5  ,data = iq.cross.train) # train
  .predictions <- predict(.ols, newdata=iq.cross.train)
  .errors <-   R2(iq.cross.train$iq, .predictions) # save prediction errors in the fold
  errors <- c(errors, .errors) # aggregate error over folds.
}

r2<- c(r2, errors)

#Big5 model with CV, fold=4:

errors <- NULL

for (k in 1:folds){
  iq.cross.train <- iq.new5.all[fold.assignment!=k,] # train subset
  iq.cross.test <-  iq.new5.all[fold.assignment==k,] # test subset
  .ols <- lm(iq~O+C+E+A+N ,data = iq.cross.train) # train
  .predictions <- predict(.ols, newdata=iq.cross.test)
  .errors <-   R2(iq.cross.test$iq, .predictions) # save prediction errors in the fold
  errors <- c(errors, .errors) # aggregate error over folds.
}
mean(errors)
r2<- c(r2, errors)

errors <- NULL

for (k in 1:folds){
  iq.cross.train <- iq.new5.all[fold.assignment!=k,] # train subset
  iq.cross.test <-  iq.new5.all[fold.assignment==k,] # test subset
  .ols <- lm(iq~O+C+E+A+N ,data = iq.cross.train) # train
  .predictions <- predict(.ols, newdata=iq.cross.train)
  .errors <-   R2(iq.cross.train$iq, .predictions) # save prediction errors in the fold
  errors <- c(errors, .errors) # aggregate error over folds.
}

r2<- c(r2, errors)


#PC5 model with CV, fold=4:

errors <- NULL

for (k in 1:folds){
  iq.cross.train <- iq.new5.all[fold.assignment!=k,] # train subset
  iq.cross.test <-  iq.new5.all[fold.assignment==k,] # test subset
  .ols <- lm(iq~PC1+PC2+PC3+PC4+PC5 ,data = iq.cross.train) # train
  .predictions <- predict(.ols, newdata=iq.cross.test)
  .errors <-   R2(iq.cross.test$iq, .predictions) # save prediction errors in the fold
  errors <- c(errors, .errors) # aggregate error over folds.
}


r2<- c(r2, errors)

errors <- NULL

for (k in 1:folds){
  iq.cross.train <- iq.new5.all[fold.assignment!=k,] # train subset
  iq.cross.test <-  iq.new5.all[fold.assignment==k,] # test subset
  .ols <- lm(iq~PC1+PC2+PC3+PC4+PC5 ,data = iq.cross.train) # train
  .predictions <- predict(.ols, newdata=iq.cross.train)
  .errors <-   R2(iq.cross.train$iq, .predictions) # save prediction errors in the fold
  errors <- c(errors, .errors) # aggregate error over folds.
}


r2<- c(r2, errors)

###################################swl################################

swl.100<- as.matrix(test.swl[,q1:q100])
dim(swl.100)

swl.new5<- swl.100%*% rotation.mat
swl.pca<- swl.100%*%pc5
swl.new5.all<- as.data.table(cbind(test.swl, swl.new5,swl.pca ))
dim(swl.new5.all)
names(swl.new5.all)


set.seed(1)
folds <- 4
fold.assignment <- sample(1:folds, nrow(swl.new5.all), replace = TRUE)

#New 5 model with CV, fold=4:

errors <- NULL

for (k in 1:folds){
  swl.cross.train <- swl.new5.all[fold.assignment!=k,] # train subset
  swl.cross.test <-  swl.new5.all[fold.assignment==k,] # test subset
  .ols <- lm(swl~V1+V2+V3+V4+V5 ,data = swl.cross.train) # train
  .predictions <- predict(.ols, newdata=swl.cross.test)
  .errors <-   R2(swl.cross.test$swl, .predictions) # save prediction errors in the fold
  errors <- c(errors, .errors) # aggregate error over folds.
}


r2<- c(r2, errors)

errors <- NULL

for (k in 1:folds){
  swl.cross.train <- swl.new5.all[fold.assignment!=k,] # train subset
  swl.cross.test <-  swl.new5.all[fold.assignment==k,] # test subset
  .ols <- lm(swl~V1+V2+V3+V4+V5 ,data = swl.cross.train) # train
  .predictions <- predict(.ols, newdata=swl.cross.train)
  .errors <-   R2(swl.cross.train$swl, .predictions) # save prediction errors in the fold
  errors <- c(errors, .errors) # aggregate error over folds.
}


r2<- c(r2, errors)

#Big5 model with CV, fold=5:

errors <- NULL

for (k in 1:folds){
  swl.cross.train <- swl.new5.all[fold.assignment!=k,] # train subset
  swl.cross.test <-  swl.new5.all[fold.assignment==k,] # test subset
  .ols <- lm(swl~O+C+E+A+N ,data = swl.cross.train) # train
  .predictions <- predict(.ols, newdata=swl.cross.test)
  .errors <-   R2(swl.cross.test$swl, .predictions) # save prediction errors in the fold
  errors <- c(errors, .errors) # aggregate error over folds.
}


r2<- c(r2, errors)


errors <- NULL

for (k in 1:folds){
  swl.cross.train <- swl.new5.all[fold.assignment!=k,] # train subset
  swl.cross.test <-  swl.new5.all[fold.assignment==k,] # test subset
  .ols <- lm(swl~O+C+E+A+N ,data = swl.cross.train) # train
  .predictions <- predict(.ols, newdata=swl.cross.train)
  .errors <-   R2(swl.cross.train$swl, .predictions) # save prediction errors in the fold
  errors <- c(errors, .errors) # aggregate error over folds.
}


r2<- c(r2, errors)

#PC5 model with CV, fold=5:

errors <- NULL

for (k in 1:folds){
  swl.cross.train <- swl.new5.all[fold.assignment!=k,] # train subset
  swl.cross.test <-  swl.new5.all[fold.assignment==k,] # test subset
  .ols <- lm(swl~PC1+PC2+PC3+PC4+PC5 ,data = swl.cross.train) # train
  .predictions <- predict(.ols, newdata=swl.cross.test)
  .errors <-   R2(swl.cross.test$swl, .predictions) # save prediction errors in the fold
  errors <- c(errors, .errors) # aggregate error over folds.
}


r2<- c(r2, errors)


errors <- NULL

for (k in 1:folds){
  swl.cross.train <- swl.new5.all[fold.assignment!=k,] # train subset
  swl.cross.test <-  swl.new5.all[fold.assignment==k,] # test subset
  .ols <- lm(swl~PC1+PC2+PC3+PC4+PC5 ,data = swl.cross.train) # train
  .predictions <- predict(.ols, newdata=swl.cross.train)
  .errors <-   R2(swl.cross.train$swl, .predictions) # save prediction errors in the fold
  errors <- c(errors, .errors) # aggregate error over folds.
}


r2<- c(r2, errors)

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
  .ols <- lm(score.cesd~V1+V2+V3+V4+V5 ,data = cesd.cross.train) # train
  .predictions <- predict(.ols, newdata=cesd.cross.test)
  .errors <-   R2(cesd.cross.test$score.cesd, .predictions) # save prediction errors in the fold
  errors <- c(errors, .errors) # aggregate error over folds.
}


r2<- c(r2, errors)

errors <- NULL

for (k in 1:folds){
  cesd.cross.train <- cesd.new5.all[fold.assignment!=k,] # train subset
  cesd.cross.test <-  cesd.new5.all[fold.assignment==k,] # test subset
  .ols <- lm(score.cesd~V1+V2+V3+V4+V5 ,data = cesd.cross.train) # train
  .predictions <- predict(.ols, newdata=cesd.cross.train)
  .errors <-   R2(cesd.cross.train$score.cesd, .predictions) # save prediction errors in the fold
  errors <- c(errors, .errors) # aggregate error over folds.
}


r2<- c(r2, errors)

#Big5 model with CV, fold=5:

errors <- NULL

for (k in 1:folds){
  cesd.cross.train <- cesd.new5.all[fold.assignment!=k,] # train subset
  cesd.cross.test <-  cesd.new5.all[fold.assignment==k,] # test subset
  .ols <- lm(score.cesd~O+C+E+A+N ,data = cesd.cross.train) # train
  .predictions <- predict(.ols, newdata=cesd.cross.test)
  .errors <-   R2(cesd.cross.test$score.cesd, .predictions) # save prediction errors in the fold
  errors <- c(errors, .errors) # aggregate error over folds.
}


r2<- c(r2, errors)

errors <- NULL

for (k in 1:folds){
  cesd.cross.train <- cesd.new5.all[fold.assignment!=k,] # train subset
  cesd.cross.test <-  cesd.new5.all[fold.assignment==k,] # test subset
  .ols <- lm(score.cesd~O+C+E+A+N ,data = cesd.cross.train) # train
  .predictions <- predict(.ols, newdata=cesd.cross.train)
  .errors <-   R2(cesd.cross.train$score.cesd, .predictions) # save prediction errors in the fold
  errors <- c(errors, .errors) # aggregate error over folds.
}


r2<- c(r2, errors)

#PC5 model with CV, fold=5:

errors <- NULL

for (k in 1:folds){
  cesd.cross.train <- cesd.new5.all[fold.assignment!=k,] # train subset
  cesd.cross.test <-  cesd.new5.all[fold.assignment==k,] # test subset
  .ols <- lm(score.cesd~PC1+PC2+PC3+PC4+PC5 ,data = cesd.cross.train) # train
  .predictions <- predict(.ols, newdata=cesd.cross.test)
  .errors <-   R2(cesd.cross.test$score.cesd, .predictions) # save prediction errors in the fold
  errors <- c(errors, .errors) # aggregate error over folds.
}

r2<- c(r2, errors)


errors <- NULL

for (k in 1:folds){
  cesd.cross.train <- cesd.new5.all[fold.assignment!=k,] # train subset
  cesd.cross.test <-  cesd.new5.all[fold.assignment==k,] # test subset
  .ols <- lm(score.cesd~PC1+PC2+PC3+PC4+PC5 ,data = cesd.cross.train) # train
  .predictions <- predict(.ols, newdata=cesd.cross.train)
  .errors <-   R2(cesd.cross.train$score.cesd, .predictions) # save prediction errors in the fold
  errors <- c(errors, .errors) # aggregate error over folds.
}

r2<- c(r2, errors)

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
  .ols <- lm(liberalness~V1+V2+V3+V4+V5 ,data = liberal.cross.train) # train
  .predictions <- predict(.ols, newdata=liberal.cross.test)
  .errors <-   R2(liberal.cross.test$liberalness, .predictions) # save prediction errors in the fold
  errors <- c(errors, .errors) # aggregate error over folds.
}


r2<- c(r2, errors)

errors <- NULL

for (k in 1:folds){
  liberal.cross.train <- liberal.new5.all[fold.assignment!=k,] # train subset
  liberal.cross.test <-  liberal.new5.all[fold.assignment==k,] # test subset
  .ols <- lm(liberalness~V1+V2+V3+V4+V5 ,data = liberal.cross.train) # train
  .predictions <- predict(.ols, newdata=liberal.cross.train)
  .errors <-   R2(liberal.cross.train$liberalness, .predictions) # save prediction errors in the fold
  errors <- c(errors, .errors) # aggregate error over folds.
}


r2<- c(r2, errors)

#Big5 model with CV, fold=4:

errors <- NULL

for (k in 1:folds){
  liberal.cross.train <- liberal.new5.all[fold.assignment!=k,] # train subset
  liberal.cross.test <-  liberal.new5.all[fold.assignment==k,] # test subset
  .ols <- lm(liberalness~O+C+E+A+N ,data = liberal.cross.train) # train
  .predictions <- predict(.ols, newdata=liberal.cross.test)
  .errors <-   R2(liberal.cross.test$liberalness, .predictions) # save prediction errors in the fold
  errors <- c(errors, .errors) # aggregate error over folds.
}


r2<- c(r2, errors)

errors <- NULL

for (k in 1:folds){
  liberal.cross.train <- liberal.new5.all[fold.assignment!=k,] # train subset
  liberal.cross.test <-  liberal.new5.all[fold.assignment==k,] # test subset
  .ols <- lm(liberalness~O+C+E+A+N ,data = liberal.cross.train) # train
  .predictions <- predict(.ols, newdata=liberal.cross.train)
  .errors <-   R2(liberal.cross.train$liberalness, .predictions) # save prediction errors in the fold
  errors <- c(errors, .errors) # aggregate error over folds.
}


r2<- c(r2, errors)

#PC5 model with CV, fold=5:

errors <- NULL

for (k in 1:folds){
  liberal.cross.train <- liberal.new5.all[fold.assignment!=k,] # train subset
  liberal.cross.test <-  liberal.new5.all[fold.assignment==k,] # test subset
  .ols <- lm(liberalness~PC1+PC2+PC3+PC4+PC5 ,data = liberal.cross.train) # train
  .predictions <- predict(.ols, newdata=liberal.cross.test)
  .errors <-   R2(liberal.cross.test$liberalness, .predictions) # save prediction errors in the fold
  errors <- c(errors, .errors) # aggregate error over folds.
}


r2<- c(r2, errors)


errors <- NULL

for (k in 1:folds){
  liberal.cross.train <- liberal.new5.all[fold.assignment!=k,] # train subset
  liberal.cross.test <-  liberal.new5.all[fold.assignment==k,] # test subset
  .ols <- lm(liberalness~PC1+PC2+PC3+PC4+PC5 ,data = liberal.cross.train) # train
  .predictions <- predict(.ols, newdata=liberal.cross.train)
  .errors <-   R2(liberal.cross.train$liberalness, .predictions) # save prediction errors in the fold
  errors <- c(errors, .errors) # aggregate error over folds.
}


r2<- c(r2, errors)

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
  .ols <- lm(risky~V1+V2+V3+V4+V5 ,data = risky.cross.train) # train
  .predictions <- predict(.ols, newdata=risky.cross.test)
  .errors <-   R2(risky.cross.test$risky, .predictions) # save prediction errors in the fold
  errors <- c(errors, .errors) # aggregate error over folds.
}


r2<- c(r2, errors)

errors <- NULL

for (k in 1:folds){
  risky.cross.train <- risky.new5.all[fold.assignment!=k,] # train subset
  risky.cross.test <-  risky.new5.all[fold.assignment==k,] # test subset
  .ols <- lm(risky~V1+V2+V3+V4+V5 ,data = risky.cross.train) # train
  .predictions <- predict(.ols, newdata=risky.cross.train)
  .errors <-   R2(risky.cross.train$risky, .predictions) # save prediction errors in the fold
  errors <- c(errors, .errors) # aggregate error over folds.
}


r2<- c(r2, errors)

#Big5 model with CV, fold=5:

errors <- NULL

for (k in 1:folds){
  risky.cross.train <- risky.new5.all[fold.assignment!=k,] # train subset
  risky.cross.test <-  risky.new5.all[fold.assignment==k,] # test subset
  .ols <- lm(risky~O+C+E+A+N ,data = risky.cross.train) # train
  .predictions <- predict(.ols, newdata=risky.cross.test)
  .errors <-   R2(risky.cross.test$risky, .predictions) # save prediction errors in the fold
  errors <- c(errors, .errors) # aggregate error over folds.
}


r2<- c(r2, errors)

errors <- NULL

for (k in 1:folds){
  risky.cross.train <- risky.new5.all[fold.assignment!=k,] # train subset
  risky.cross.test <-  risky.new5.all[fold.assignment==k,] # test subset
  .ols <- lm(risky~O+C+E+A+N ,data = risky.cross.train) # train
  .predictions <- predict(.ols, newdata=risky.cross.train)
  .errors <-   R2(risky.cross.train$risky, .predictions) # save prediction errors in the fold
  errors <- c(errors, .errors) # aggregate error over folds.
}

r2<- c(r2, errors)

#PC5 model with CV, fold=5:

errors <- NULL

for (k in 1:folds){
  risky.cross.train <- risky.new5.all[fold.assignment!=k,] # train subset
  risky.cross.test <-  risky.new5.all[fold.assignment==k,] # test subset
  .ols <- lm(risky~PC1+PC2+PC3+PC4+PC5 ,data = risky.cross.train) # train
  .predictions <- predict(.ols, newdata=risky.cross.test)
  .errors <-   R2(risky.cross.test$risky, .predictions) # save prediction errors in the fold
  errors <- c(errors, .errors) # aggregate error over folds.
}


r2<- c(r2, errors)

errors <- NULL

for (k in 1:folds){
  risky.cross.train <- risky.new5.all[fold.assignment!=k,] # train subset
  risky.cross.test <-  risky.new5.all[fold.assignment==k,] # test subset
  .ols <- lm(risky~PC1+PC2+PC3+PC4+PC5 ,data = risky.cross.train) # train
  .predictions <- predict(.ols, newdata=risky.cross.train)
  .errors <-   R2(risky.cross.train$risky, .predictions) # save prediction errors in the fold
  errors <- c(errors, .errors) # aggregate error over folds.
}


r2<- c(r2, errors)

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
  .ols <- lm(network_size~V1+V2+V3+V4+V5 ,data = network.cross.train) # train
  .predictions <- predict(.ols, newdata=network.cross.test)
  .errors <-   R2(network.cross.test$network_size, .predictions) # save prediction errors in the fold
  errors <- c(errors, .errors) # aggregate error over folds.
}


r2<- c(r2, errors)


errors <- NULL

for (k in 1:folds){
  network.cross.train <- network.new5.all[fold.assignment!=k,] # train subset
  network.cross.test <-  network.new5.all[fold.assignment==k,] # test subset
  .ols <- lm(network_size~V1+V2+V3+V4+V5 ,data = network.cross.train) # train
  .predictions <- predict(.ols, newdata=network.cross.train)
  .errors <-   R2(network.cross.train$network_size, .predictions) # save prediction errors in the fold
  errors <- c(errors, .errors) # aggregate error over folds.
}


r2<- c(r2, errors)

#Big5 model with CV, fold=5:

errors <- NULL

for (k in 1:folds){
  network.cross.train <- network.new5.all[fold.assignment!=k,] # train subset
  network.cross.test <-  network.new5.all[fold.assignment==k,] # test subset
  .ols <- lm(network_size~O+C+E+A+N ,data = network.cross.train) # train
  .predictions <- predict(.ols, newdata=network.cross.test)
  .errors <-   R2(network.cross.test$network_size, .predictions) # save prediction errors in the fold
  errors <- c(errors, .errors) # aggregate error over folds.
}


r2<- c(r2, errors)
errors <- NULL

for (k in 1:folds){
  network.cross.train <- network.new5.all[fold.assignment!=k,] # train subset
  network.cross.test <-  network.new5.all[fold.assignment==k,] # test subset
  .ols <- lm(network_size~O+C+E+A+N ,data = network.cross.train) # train
  .predictions <- predict(.ols, newdata=network.cross.train)
  .errors <-   R2(network.cross.train$network_size, .predictions) # save prediction errors in the fold
  errors <- c(errors, .errors) # aggregate error over folds.
}


r2<- c(r2, errors)

#PC5 model with CV, fold=5:

errors <- NULL

for (k in 1:folds){
  network.cross.train <- network.new5.all[fold.assignment!=k,] # train subset
  network.cross.test <-  network.new5.all[fold.assignment==k,] # test subset
  .ols <- lm(network_size~PC1+PC2+PC3+PC4+PC5 ,data = network.cross.train) # train
  .predictions <- predict(.ols, newdata=network.cross.test)
  .errors <-   R2(network.cross.test$network_size, .predictions) # save prediction errors in the fold
  errors <- c(errors, .errors) # aggregate error over folds.
}


r2<- c(r2, errors)


errors <- NULL

for (k in 1:folds){
  network.cross.train <- network.new5.all[fold.assignment!=k,] # train subset
  network.cross.test <-  network.new5.all[fold.assignment==k,] # test subset
  .ols <- lm(network_size~PC1+PC2+PC3+PC4+PC5 ,data = network.cross.train) # train
  .predictions <- predict(.ols, newdata=network.cross.train)
  .errors <-   R2(network.cross.train$network_size, .predictions) # save prediction errors in the fold
  errors <- c(errors, .errors) # aggregate error over folds.
}


r2<- c(r2, errors)

###################################nonhealthy################################

nonhealthy.100<- as.matrix(test.nonhealthy[,q1:q100])
dim(nonhealthy.100)

nonhealthy.new5<- nonhealthy.100%*% rotation.mat
nonhealthy.pca<- nonhealthy.100%*%pc5
nonhealthy.new5.all<- as.data.table(cbind(test.nonhealthy, nonhealthy.new5,nonhealthy.pca ))
dim(nonhealthy.new5.all)
names(nonhealthy.new5.all)

set.seed(4)
folds <- 4
fold.assignment <- sample(
        1:folds, nrow(nonhealthy.new5.all), replace = TRUE)

#New 5 model with CV, fold=5:

errors <- NULL

for (k in 1:folds){
  nonhealthy.cross.train <- nonhealthy.new5.all[fold.assignment!=k,] # train subset
  nonhealthy.cross.test <-  nonhealthy.new5.all[fold.assignment==k,] # test subset
  .ols <- lm(nonhealthy~V1+V2+V3+V4+V5 ,data = nonhealthy.cross.train) # train
  .predictions <- predict(.ols, newdata=nonhealthy.cross.test)
  .errors <-   R2(nonhealthy.cross.test$nonhealthy, .predictions) # save prediction errors in the fold
  errors <- c(errors, .errors) # aggregate error over folds.
}

r2<- c(r2, errors)

errors <- NULL

for (k in 1:folds){
  nonhealthy.cross.train <- nonhealthy.new5.all[fold.assignment!=k,] # train subset
  nonhealthy.cross.test <-  nonhealthy.new5.all[fold.assignment==k,] # test subset
  .ols <- lm(nonhealthy~V1+V2+V3+V4+V5 ,data = nonhealthy.cross.train) # train
  .predictions <- predict(.ols, newdata=nonhealthy.cross.train)
  .errors <-   R2(nonhealthy.cross.train$nonhealthy, .predictions) # save prediction errors in the fold
  errors <- c(errors, .errors) # aggregate error over folds.
}


r2<- c(r2, errors)

#Big5 model with CV, fold=5:

errors <- NULL

for (k in 1:folds){
  nonhealthy.cross.train <- nonhealthy.new5.all[fold.assignment!=k,] # train subset
  nonhealthy.cross.test <-  nonhealthy.new5.all[fold.assignment==k,] # test subset
  .ols <- lm(nonhealthy~O+C+E+A+N ,data = nonhealthy.cross.train) # train
  .predictions <- predict(.ols, newdata=nonhealthy.cross.test)
  .errors <-   R2(nonhealthy.cross.test$nonhealthy, .predictions) # save prediction errors in the fold
  errors <- c(errors, .errors) # aggregate error over folds.
}

r2<- c(r2, errors)


errors <- NULL

for (k in 1:folds){
  nonhealthy.cross.train <- nonhealthy.new5.all[fold.assignment!=k,] # train subset
  nonhealthy.cross.test <-  nonhealthy.new5.all[fold.assignment==k,] # test subset
  .ols <- lm(nonhealthy~O+C+E+A+N ,data = nonhealthy.cross.train) # train
  .predictions <- predict(.ols, newdata=nonhealthy.cross.train)
  .errors <-   R2(nonhealthy.cross.train$nonhealthy, .predictions) # save prediction errors in the fold
  errors <- c(errors, .errors) # aggregate error over folds.
}


r2<- c(r2, errors)

#PC5 model with CV, fold=5:

errors <- NULL

for (k in 1:folds){
  nonhealthy.cross.train <- nonhealthy.new5.all[fold.assignment!=k,] # train subset
  nonhealthy.cross.test <-  nonhealthy.new5.all[fold.assignment==k,] # test subset
  .ols <- lm(nonhealthy~PC1+PC2+PC3+PC4+PC5 ,data = nonhealthy.cross.train) # train
  .predictions <- predict(.ols, newdata=nonhealthy.cross.test)
  .errors <-   R2(nonhealthy.cross.test$nonhealthy, .predictions) # save prediction errors in the fold
  errors <- c(errors, .errors) # aggregate error over folds.
}


r2<- c(r2, errors)

errors <- NULL

for (k in 1:folds){
  nonhealthy.cross.train <- nonhealthy.new5.all[fold.assignment!=k,] # train subset
  nonhealthy.cross.test <-  nonhealthy.new5.all[fold.assignment==k,] # test subset
  .ols <- lm(nonhealthy~PC1+PC2+PC3+PC4+PC5 ,data = nonhealthy.cross.train) # train
  .predictions <- predict(.ols, newdata=nonhealthy.cross.train)
  .errors <-   R2(nonhealthy.cross.train$nonhealthy, .predictions) # save prediction errors in the fold
  errors <- c(errors, .errors) # aggregate error over folds.
}


r2<- c(r2, errors)

###################################svq.trans################################

svq.trans.100<- as.matrix(test.svq.trans[,q1:q100])
dim(svq.trans.100)

svq.trans.new5<- svq.trans.100%*% rotation.mat
svq.trans.pca<- svq.trans.100%*%pc5
svq.trans.new5.all<- as.data.table(cbind(test.svq.trans, svq.trans.new5,svq.trans.pca ))
dim(svq.trans.new5.all)
names(svq.trans.new5.all)

set.seed(1234)
folds <- 4
fold.assignment <- sample(
            1:folds, nrow(svq.trans.new5.all), replace = TRUE)

#New 5 model with CV, fold=4:

errors <- NULL

for (k in 1:folds){
  svq.trans.cross.train <- svq.trans.new5.all[fold.assignment!=k,] # train subset
  svq.trans.cross.test <-  svq.trans.new5.all[fold.assignment==k,] # test subset
  .ols <- lm(sv.trans~V1+V2+V3+V4+V5 ,data = svq.trans.cross.train) # train
  .predictions <- predict(.ols, newdata=svq.trans.cross.test)
  .errors <-   R2(svq.trans.cross.test$sv.trans, .predictions) # save prediction errors in the fold
  errors <- c(errors, .errors) # aggregate error over folds.
}
r2<- c(r2, errors)

errors <- NULL

for (k in 1:folds){
  svq.trans.cross.train <- svq.trans.new5.all[fold.assignment!=k,] # train subset
  svq.trans.cross.test <-  svq.trans.new5.all[fold.assignment==k,] # test subset
  .ols <- lm(sv.trans~V1+V2+V3+V4+V5 ,data = svq.trans.cross.train) # train
  .predictions <- predict(.ols, newdata=svq.trans.cross.train)
  .errors <-   R2(svq.trans.cross.train$sv.trans, .predictions) # save prediction errors in the fold
  errors <- c(errors, .errors) # aggregate error over folds.
}

r2<- c(r2, errors)

#Big5 model with CV, fold=5:

errors <- NULL

for (k in 1:folds){
  svq.trans.cross.train <- svq.trans.new5.all[fold.assignment!=k,] # train subset
  svq.trans.cross.test <-  svq.trans.new5.all[fold.assignment==k,] # test subset
  .ols <- lm(sv.trans~O+C+E+A+N ,data = svq.trans.cross.train) # train
  .predictions <- predict(.ols, newdata=svq.trans.cross.test)
  .errors <-   R2(svq.trans.cross.test$sv.trans, .predictions) # save prediction errors in the fold
  errors <- c(errors, .errors) # aggregate error over folds.
}

r2<- c(r2, errors)

errors <- NULL

for (k in 1:folds){
  svq.trans.cross.train <- svq.trans.new5.all[fold.assignment!=k,] # train subset
  svq.trans.cross.test <-  svq.trans.new5.all[fold.assignment==k,] # test subset
  .ols <- lm(sv.trans~O+C+E+A+N ,data = svq.trans.cross.train) # train
  .predictions <- predict(.ols, newdata=svq.trans.cross.train)
  .errors <-   R2(svq.trans.cross.train$sv.trans, .predictions) # save prediction errors in the fold
  errors <- c(errors, .errors) # aggregate error over folds.
}
mean(errors)

r2<- c(r2, errors)

#PC5 model with CV, fold=5:

errors <- NULL

for (k in 1:folds){
  svq.trans.cross.train <- svq.trans.new5.all[fold.assignment!=k,] # train subset
  svq.trans.cross.test <-  svq.trans.new5.all[fold.assignment==k,] # test subset
  .ols <- lm(sv.trans~PC1+PC2+PC3+PC4+PC5 ,data = svq.trans.cross.train) # train
  .predictions <- predict(.ols, newdata=svq.trans.cross.test)
  .errors <-   R2(svq.trans.cross.test$sv.trans, .predictions) # save prediction errors in the fold
  errors <- c(errors, .errors) # aggregate error over folds.
}


r2<- c(r2, errors)


errors <- NULL

for (k in 1:folds){
  svq.trans.cross.train <- svq.trans.new5.all[fold.assignment!=k,] # train subset
  svq.trans.cross.test <-  svq.trans.new5.all[fold.assignment==k,] # test subset
  .ols <- lm(sv.trans~PC1+PC2+PC3+PC4+PC5 ,data = svq.trans.cross.train) # train
  .predictions <- predict(.ols, newdata=svq.trans.cross.train)
  .errors <-   R2(svq.trans.cross.train$sv.trans, .predictions) # save prediction errors in the fold
  errors <- c(errors, .errors) # aggregate error over folds.
}


r2<- c(r2, errors)

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
  .ols <- lm(sv.openness~V1+V2+V3+V4+V5 ,data = svq.openness.cross.train) # train
  .predictions <- predict(.ols, newdata=svq.openness.cross.test)
  .errors <-   R2(svq.openness.cross.test$sv.openness, .predictions) # save prediction errors in the fold
  errors <- c(errors, .errors) # aggregate error over folds.
}

mean(errors)
r2<- c(r2, errors)


errors <- NULL

for (k in 1:folds){
  svq.openness.cross.train <- svq.openness.new5.all[fold.assignment!=k,] # train subset
  svq.openness.cross.test <-  svq.openness.new5.all[fold.assignment==k,] # test subset
  .ols <- lm(sv.openness~V1+V2+V3+V4+V5 ,data = svq.openness.cross.train) # train
  .predictions <- predict(.ols, newdata=svq.openness.cross.train)
  .errors <-   R2(svq.openness.cross.train$sv.openness, .predictions) # save prediction errors in the fold
  errors <- c(errors, .errors) # aggregate error over folds.
}


r2<- c(r2, errors)

#Big5 model with CV, fold=5:

errors <- NULL

for (k in 1:folds){
  svq.openness.cross.train <- svq.openness.new5.all[fold.assignment!=k,] # train subset
  svq.openness.cross.test <-  svq.openness.new5.all[fold.assignment==k,] # test subset
  .ols <- lm(sv.openness~O+C+E+A+N ,data = svq.openness.cross.train) # train
  .predictions <- predict(.ols, newdata=svq.openness.cross.test)
  .errors <-   R2(svq.openness.cross.test$sv.openness, .predictions) # save prediction errors in the fold
  errors <- c(errors, .errors) # aggregate error over folds.
}

mean(errors)
r2<- c(r2, errors)

errors <- NULL

for (k in 1:folds){
  svq.openness.cross.train <- svq.openness.new5.all[fold.assignment!=k,] # train subset
  svq.openness.cross.test <-  svq.openness.new5.all[fold.assignment==k,] # test subset
  .ols <- lm(sv.openness~O+C+E+A+N ,data = svq.openness.cross.train) # train
  .predictions <- predict(.ols, newdata=svq.openness.cross.train)
  .errors <-   R2(svq.openness.cross.train$sv.openness, .predictions) # save prediction errors in the fold
  errors <- c(errors, .errors) # aggregate error over folds.
}


r2<- c(r2, errors)

#PC5 model with CV, fold=5:

errors <- NULL

for (k in 1:folds){
  svq.openness.cross.train <- svq.openness.new5.all[fold.assignment!=k,] # train subset
  svq.openness.cross.test <-  svq.openness.new5.all[fold.assignment==k,] # test subset
  .ols <- lm(sv.openness~PC1+PC2+PC3+PC4+PC5 ,data = svq.openness.cross.train) # train
  .predictions <- predict(.ols, newdata=svq.openness.cross.test)
  .errors <-   R2(svq.openness.cross.test$sv.openness, .predictions) # save prediction errors in the fold
  errors <- c(errors, .errors) # aggregate error over folds.
}


r2<- c(r2, errors)

errors <- NULL

for (k in 1:folds){
  svq.openness.cross.train <- svq.openness.new5.all[fold.assignment!=k,] # train subset
  svq.openness.cross.test <-  svq.openness.new5.all[fold.assignment==k,] # test subset
  .ols <- lm(sv.openness~PC1+PC2+PC3+PC4+PC5 ,data = svq.openness.cross.train) # train
  .predictions <- predict(.ols, newdata=svq.openness.cross.train)
  .errors <-   R2(svq.openness.cross.train$sv.openness, .predictions) # save prediction errors in the fold
  errors <- c(errors, .errors) # aggregate error over folds.
}


r2<- c(r2, errors)

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
  .ols <- lm(empathy~V1+V2+V3+V4+V5 ,data = empathy.cross.train) # train
  .predictions <- predict(.ols, newdata=empathy.cross.test)
  .errors <-   R2(empathy.cross.test$empathy, .predictions) # save prediction errors in the fold
  errors <- c(errors, .errors) # aggregate error over folds.
}

r2<- c(r2, errors)


errors <- NULL

for (k in 1:folds){
  empathy.cross.train <- empathy.new5.all[fold.assignment!=k,] # train subset
  empathy.cross.test <-  empathy.new5.all[fold.assignment==k,] # test subset
  .ols <- lm(empathy~V1+V2+V3+V4+V5 ,data = empathy.cross.train) # train
  .predictions <- predict(.ols, newdata=empathy.cross.train)
  .errors <-   R2(empathy.cross.train$empathy, .predictions) # save prediction errors in the fold
  errors <- c(errors, .errors) # aggregate error over folds.
}

r2<- c(r2, errors)

#Big5 model with CV, fold=5:

errors <- NULL

for (k in 1:folds){
  empathy.cross.train <- empathy.new5.all[fold.assignment!=k,] # train subset
  empathy.cross.test <-  empathy.new5.all[fold.assignment==k,] # test subset
  .ols <- lm(empathy~O+C+E+A+N ,data = empathy.cross.train) # train
  .predictions <- predict(.ols, newdata=empathy.cross.test)
  .errors <-   R2(empathy.cross.test$empathy, .predictions) # save prediction errors in the fold
  errors <- c(errors, .errors) # aggregate error over folds.
}

r2<- c(r2, errors)

errors <- NULL

for (k in 1:folds){
  empathy.cross.train <- empathy.new5.all[fold.assignment!=k,] # train subset
  empathy.cross.test <-  empathy.new5.all[fold.assignment==k,] # test subset
  .ols <- lm(empathy~O+C+E+A+N ,data = empathy.cross.train) # train
  .predictions <- predict(.ols, newdata=empathy.cross.train)
  .errors <-   R2(empathy.cross.train$empathy, .predictions) # save prediction errors in the fold
  errors <- c(errors, .errors) # aggregate error over folds.
}

r2<- c(r2, errors)


#PC5 model with CV, fold=5:

errors <- NULL

for (k in 1:folds){
  empathy.cross.train <- empathy.new5.all[fold.assignment!=k,] # train subset
  empathy.cross.test <-  empathy.new5.all[fold.assignment==k,] # test subset
  .ols <- lm(empathy~PC1+PC2+PC3+PC4+PC5 ,data = empathy.cross.train) # train
  .predictions <- predict(.ols, newdata=empathy.cross.test)
  .errors <-   R2(empathy.cross.test$empathy, .predictions) # save prediction errors in the fold
  errors <- c(errors, .errors) # aggregate error over folds.
}

r2<- c(r2, errors)

errors <- NULL

for (k in 1:folds){
  empathy.cross.train <- empathy.new5.all[fold.assignment!=k,] # train subset
  empathy.cross.test <-  empathy.new5.all[fold.assignment==k,] # test subset
  .ols <- lm(empathy~PC1+PC2+PC3+PC4+PC5 ,data = empathy.cross.train) # train
  .predictions <- predict(.ols, newdata=empathy.cross.train)
  .errors <-   R2(empathy.cross.train$empathy, .predictions) # save prediction errors in the fold
  errors <- c(errors, .errors) # aggregate error over folds.
}

r2<- c(r2, errors)

####################combine all:
ten.dv.results$r2<- r2
View(ten.dv.results)
sum.table.10<- ten.dv.results[,.(Mean=mean(r2)), by=.(Type,Set,dv)]
sum.table.10.test.long<- as.data.table(sum.table.10[sum.table.10$Set=="Test"])
sum.table.10.test.wide<- dcast(sum.table.10.test.long, dv~Type, value.var="Mean")

sum.table.10.test.wide$diff.big5<- sum.table.10.test.wide$`Predictive Five`-sum.table.10.test.wide$`Big Five`
sum.table.10.test.wide$Improvement<- sum.table.10.test.wide$diff.big5/sum.table.10.test.wide$`Big Five`

mean(sum.table.10.test.wide$`Predictive Five`)
mean(sum.table.10.test.wide$`Big Five`)
mean(sum.table.10.test.wide$`PC Five`)

fold.4.ten.dv.results<-ten.dv.results