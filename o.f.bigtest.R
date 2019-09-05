
rotation.mat <- as.matrix(rrpack.model$coef.ls %*% rrpack.model$A)
r2.sd.results<- data.table(dv=rep(c("Intelligence", "Well-being", "Depression",
                "Political Orientation", "Risky Behavior", "Number of Facebook Friends",
                "Nonhealthy Behavior", "Transcendence Values", "Openness Values",
                "Empathy", "DV.1","DV.2","DV.3","DV.4","DV.5","DV.6","DV.7","DV.8",
                "DV.9","DV.10","DV.11","DV.12"), each=30), 
                Type= rep(c("Predictive Five", "100Q", "Big Five"), each=10, times= 22),
                Set= rep(c("Test", "Train"),each=5, times= 66))

View(r2.sd.results)

r2<-NULL
sd<-NULL

#######################################iq#################################

#try to predict IQ with the new 5 and with 100 Q:

q.100<- as.data.table(
  as.matrix(test.iq[,q1:q100]) %*% rotation.mat)
iq.all<- as.data.table(cbind(test.iq, q.100))
dim(iq.all)

###predictive five:

errors.train <- NULL
errors.test<-NULL

for (k in 21:25){
  set.seed(k)
  size_test <- 250
  test_ind <- sample(seq_len(nrow(iq.all)),
                     size = size_test, 
                     replace = FALSE)
  iq.train <- iq.all[test_ind, ]
  iq.test <- iq.all[-test_ind, ]

  iq.model <- lm(iq~V1+V2+V3+V4+V5 ,data = iq.train) # train
  
  pred.train.iq <- predict(iq.model, newdata=iq.train)
  .errors.train<-R2(iq.train$iq,pred.train.iq)
  errors.train <- c(errors.train, .errors.train)
 
  pred.test.iq <- predict(iq.model, newdata=iq.test)
  .errors.test<-R2(iq.test$iq,pred.test.iq)
  errors.test <- c(errors.test, .errors.test) 
  
}

r2<- c(r2, (errors.test), (errors.train))
sd<- c(sd, sd(errors.test), sd(errors.train))


#all 100q:

iq.100q<-as.data.table(cbind(iq.all[,q1:q100],iq=iq.all$iq))

errors.train <- NULL
errors.test<-NULL

for (k in 1:5){
  
  set.seed(k)
  size_test <- 250
  test_ind <- sample(seq_len(nrow(iq.100q)),
                     size = size_test, 
                     replace = FALSE)
  iq.train <- iq.100q[test_ind, ]
  iq.test <- iq.100q[-test_ind, ]
  
  iq.model <- lm(iq~., data = iq.train) # train
  
  pred.train.iq <- predict(iq.model, newdata=iq.train)
  .errors.train<-R2(iq.train$iq,pred.train.iq)
  errors.train <- c(errors.train, .errors.train)
  
  pred.test.iq <- predict(iq.model, newdata=iq.test)
  .errors.test<-R2(iq.test$iq,pred.test.iq)
  errors.test <- c(errors.test, .errors.test) 
  
}
r2<- c(r2,  (errors.test),  (errors.train))
sd<- c(sd, sd(errors.test), sd(errors.train))

######big5:

errors.train <- NULL
errors.test<-NULL

for (k in 1:5){
  set.seed(k)
  size_test <- 250
  test_ind <- sample(seq_len(nrow(iq.all)),
                     size = size_test, 
                     replace = FALSE)
  iq.train <- iq.all[test_ind, ]
  iq.test <- iq.all[-test_ind, ]
  
  iq.model <- lm(iq~O+C+E+A+N ,data = iq.train) # train
  
  pred.train.iq <- predict(iq.model, newdata=iq.train)
  .errors.train<-R2(iq.train$iq,pred.train.iq)
  errors.train <- c(errors.train, .errors.train)
  
  pred.test.iq <- predict(iq.model, newdata=iq.test)
  .errors.test<-R2(iq.test$iq,pred.test.iq)
  errors.test <- c(errors.test, .errors.test) 
  
}
r2<- c(r2,  (errors.test),  (errors.train))
sd<- c(sd, sd(errors.test), sd(errors.train))
  
#######################################swl#################################

#try to predict swl with the new 5 and with 100 Q:

q.100<- as.data.table(
  as.matrix(test.swl[,q1:q100]) %*% rotation.mat)
swl.all<- as.data.table(cbind(test.swl, q.100))
dim(swl.all)

###predictive five:

errors.train <- NULL
errors.test<-NULL

for (k in 1:5){
  set.seed(k)
  size_test <- 250
  test_ind <- sample(seq_len(nrow(swl.all)),
                     size = size_test, 
                     replace = FALSE)
  swl.train <- swl.all[test_ind, ]
  swl.test <- swl.all[-test_ind, ]
  
  swl.model <- lm(swl~V1+V2+V3+V4+V5 ,data = swl.train) # train
  
  pred.train.swl <- predict(swl.model, newdata=swl.train)
  .errors.train<-R2(swl.train$swl,pred.train.swl)
  errors.train <- c(errors.train, .errors.train)
  
  pred.test.swl <- predict(swl.model, newdata=swl.test)
  .errors.test<-R2(swl.test$swl,pred.test.swl)
  errors.test <- c(errors.test, .errors.test) 
  
}

r2<- c(r2,  (errors.test),  (errors.train))
sd<- c(sd, sd(errors.test), sd(errors.train))

#all 100q:

swl.100q<-as.data.table(cbind(swl.all[,q1:q100],swl=swl.all$swl))

errors.train <- NULL
errors.test<-NULL

for (k in 1:5){
  
  set.seed(k)
  size_test <- 250
  test_ind <- sample(seq_len(nrow(swl.100q)),
                     size = size_test, 
                     replace = FALSE)
  swl.train <- swl.100q[test_ind, ]
  swl.test <- swl.100q[-test_ind, ]
  
  swl.model <- lm(swl~., data = swl.train) # train
  
  pred.train.swl <- predict(swl.model, newdata=swl.train)
  .errors.train<-R2(swl.train$swl,pred.train.swl)
  errors.train <- c(errors.train, .errors.train)
  
  pred.test.swl <- predict(swl.model, newdata=swl.test)
  .errors.test<-R2(swl.test$swl,pred.test.swl)
  errors.test <- c(errors.test, .errors.test) 
  
}
r2<- c(r2,  (errors.test),  (errors.train))
sd<- c(sd, sd(errors.test), sd(errors.train))

######big5:

errors.train <- NULL
errors.test<-NULL

for (k in 1:5){
  set.seed(k)
  size_test <- 250
  test_ind <- sample(seq_len(nrow(swl.all)),
                     size = size_test, 
                     replace = FALSE)
  swl.train <- swl.all[test_ind, ]
  swl.test <- swl.all[-test_ind, ]
  
  swl.model <- lm(swl~O+C+E+A+N ,data = swl.train) # train
  
  pred.train.swl <- predict(swl.model, newdata=swl.train)
  .errors.train<-R2(swl.train$swl,pred.train.swl)
  errors.train <- c(errors.train, .errors.train)
  
  pred.test.swl <- predict(swl.model, newdata=swl.test)
  .errors.test<-R2(swl.test$swl,pred.test.swl)
  errors.test <- c(errors.test, .errors.test) 
  
}
r2<- c(r2,  (errors.test),  (errors.train))
sd<- c(sd, sd(errors.test), sd(errors.train))


#######################################CESD#################################

#try to predict IQ with the new 5 and with 100 Q:

q.100<- as.data.table(
  as.matrix(test.cesd[,q1:q100]) %*% rotation.mat)
cesd.all<- as.data.table(cbind(test.cesd, q.100))
dim(cesd.all)

###predictive five:

errors.train <- NULL
errors.test<-NULL

for (k in 1:5){
  set.seed(k)
  size_test <- 250
  test_ind <- sample(seq_len(nrow(cesd.all)),
                     size = size_test, 
                     replace = FALSE)
  cesd.train <- cesd.all[test_ind, ]
  cesd.test <- cesd.all[-test_ind, ]
  
  cesd.model <- lm(score.cesd~V1+V2+V3+V4+V5 ,data = cesd.train) # train
  
  pred.train.cesd <- predict(cesd.model, newdata=cesd.train)
  .errors.train<-R2(cesd.train$score.cesd, pred.train.cesd)
  errors.train <- c(errors.train, .errors.train)
  
  pred.test.cesd <- predict(cesd.model, newdata=cesd.test)
  .errors.test<-R2(cesd.test$score.cesd, pred.test.cesd)
  errors.test <- c(errors.test, .errors.test) 
  
}
r2<- c(r2,  (errors.test),  (errors.train))
sd<- c(sd, sd(errors.test), sd(errors.train))

#all 100q:

cesd.100q<-as.data.table(cbind(cesd.all[,q1:q100],cesd=cesd.all$score.cesd))

errors.train <- NULL
errors.test<-NULL

for (k in 1:5){
  
  set.seed(k)
  size_test <- 250
  test_ind <- sample(seq_len(nrow(cesd.100q)),
                     size = size_test, 
                     replace = FALSE)
  cesd.train <- cesd.100q[test_ind, ]
  cesd.test <- cesd.100q[-test_ind, ]
  
  cesd.model <- lm(cesd~., data = cesd.train) # train
  
  pred.train.cesd <- predict(cesd.model, newdata=cesd.train)
  .errors.train<-R2(cesd.train$cesd,pred.train.cesd)
  errors.train <- c(errors.train, .errors.train)
  
  pred.test.cesd <- predict(cesd.model, newdata=cesd.test)
  .errors.test<-R2(cesd.test$cesd,pred.test.cesd)
  errors.test <- c(errors.test, .errors.test) 
  
}

r2<- c(r2,  (errors.test),  (errors.train))
sd<- c(sd, sd(errors.test), sd(errors.train))

######big5:

errors.train <- NULL
errors.test<-NULL

for (k in 1:5){
  set.seed(k)
  size_test <- 250
  test_ind <- sample(seq_len(nrow(cesd.all)),
                     size = size_test, 
                     replace = FALSE)
  cesd.train <- cesd.all[test_ind, ]
  cesd.test <- cesd.all[-test_ind, ]
  
  cesd.model <- lm(score.cesd~O+C+E+A+N ,data = cesd.train) # train
  
  pred.train.cesd <- predict(cesd.model, newdata=cesd.train)
  .errors.train<-R2(cesd.train$score.cesd,pred.train.cesd)
  errors.train <- c(errors.train, .errors.train)
  
  pred.test.cesd <- predict(cesd.model, newdata=cesd.test)
  .errors.test<-R2(cesd.test$score.cesd,pred.test.cesd)
  errors.test <- c(errors.test, .errors.test) 
  
}

r2<- c(r2,  (errors.test),  (errors.train))
sd<- c(sd, sd(errors.test), sd(errors.train))

#######################################liberal#################################

#try to predict liberal with the new 5 and with 100 Q:

q.100<- as.data.table(
  as.matrix(test.liberal[,q1:q100]) %*% rotation.mat)
liberal.all<- as.data.table(cbind(test.liberal, q.100))
dim(liberal.all)

###predictive five:

errors.train <- NULL
errors.test<-NULL

for (k in 1:5){
  set.seed(k)
  size_test <- 250
  test_ind <- sample(seq_len(nrow(liberal.all)),
                     size = size_test, 
                     replace = FALSE)
  liberal.train <- liberal.all[test_ind, ]
  liberal.test <- liberal.all[-test_ind, ]
  
  liberal.model <- lm(liberalness~V1+V2+V3+V4+V5 ,data = liberal.train) # train
  
  pred.train.liberal <- predict(liberal.model, newdata=liberal.train)
  .errors.train<-R2(liberal.train$liberal,pred.train.liberal)
  errors.train <- c(errors.train, .errors.train)
  
  pred.test.liberal <- predict(liberal.model, newdata=liberal.test)
  .errors.test<-R2(liberal.test$liberal,pred.test.liberal)
  errors.test <- c(errors.test, .errors.test) 
  
}

r2<- c(r2,  (errors.test),  (errors.train))
sd<- c(sd, sd(errors.test), sd(errors.train))

#all 100q:

liberal.100q<-as.data.table(cbind(liberal.all[,q1:q100],liberal=liberal.all$liberalness))

errors.train <- NULL
errors.test<-NULL

for (k in 1:5){
  
  set.seed(k)
  size_test <- 250
  test_ind <- sample(seq_len(nrow(liberal.100q)),
                     size = size_test, 
                     replace = FALSE)
  liberal.train <- liberal.100q[test_ind, ]
  liberal.test <- liberal.100q[-test_ind, ]
  
  liberal.model <- lm(liberal~., data = liberal.train) # train
  
  pred.train.liberal <- predict(liberal.model, newdata=liberal.train)
  .errors.train<-R2(liberal.train$liberal,pred.train.liberal)
  errors.train <- c(errors.train, .errors.train)
  
  pred.test.liberal <- predict(liberal.model, newdata=liberal.test)
  .errors.test<-R2(liberal.test$liberal,pred.test.liberal)
  errors.test <- c(errors.test, .errors.test) 
  
}

r2<- c(r2,  (errors.test),  (errors.train))
sd<- c(sd, sd(errors.test), sd(errors.train))

######big5:

errors.train <- NULL
errors.test<-NULL

for (k in 1:5){
  set.seed(k)
  size_test <- 250
  test_ind <- sample(seq_len(nrow(liberal.all)),
                     size = size_test, 
                     replace = FALSE)
  liberal.train <- liberal.all[test_ind, ]
  liberal.test <- liberal.all[-test_ind, ]
  
  liberal.model <- lm(liberalness~O+C+E+A+N ,data = liberal.train) # train
  
  pred.train.liberal <- predict(liberal.model, newdata=liberal.train)
  .errors.train<-R2(liberal.train$liberalness,pred.train.liberal)
  errors.train <- c(errors.train, .errors.train)
  
  pred.test.liberal <- predict(liberal.model, newdata=liberal.test)
  .errors.test<-R2(liberal.test$liberalness,pred.test.liberal)
  errors.test <- c(errors.test, .errors.test) 
  
}

r2<- c(r2,  (errors.test),  (errors.train))
sd<- c(sd, sd(errors.test), sd(errors.train))

#######################################risky#################################

#try to predict risky with the new 5 and with 100 Q:

q.100<- as.data.table(
  as.matrix(test.risky[,q1:q100]) %*% rotation.mat)
risky.all<- as.data.table(cbind(test.risky, q.100))
dim(risky.all)

###predictive five:

errors.train <- NULL
errors.test<-NULL

for (k in 1:5){
  set.seed(k)
  size_test <- 250
  test_ind <- sample(seq_len(nrow(risky.all)),
                     size = size_test, 
                     replace = FALSE)
  risky.train <- risky.all[test_ind, ]
  risky.test <- risky.all[-test_ind, ]
  
  risky.model <- lm(risky~V1+V2+V3+V4+V5 ,data = risky.train) # train
  
  pred.train.risky <- predict(risky.model, newdata=risky.train)
  .errors.train<-R2(risky.train$risky,pred.train.risky)
  errors.train <- c(errors.train, .errors.train)
  
  pred.test.risky <- predict(risky.model, newdata=risky.test)
  .errors.test<-R2(risky.test$risky,pred.test.risky)
  errors.test <- c(errors.test, .errors.test) 
  
}

r2<- c(r2,  (errors.test),  (errors.train))
sd<- c(sd, sd(errors.test), sd(errors.train))

#all 100q:

risky.100q<-as.data.table(cbind(risky.all[,q1:q100],risky=risky.all$risky))

errors.train <- NULL
errors.test<-NULL

for (k in 1:5){
  
  set.seed(k)
  size_test <- 250
  test_ind <- sample(seq_len(nrow(risky.100q)),
                     size = size_test, 
                     replace = FALSE)
  risky.train <- risky.100q[test_ind, ]
  risky.test <- risky.100q[-test_ind, ]
  
  risky.model <- lm(risky~., data = risky.train) # train
  
  pred.train.risky <- predict(risky.model, newdata=risky.train)
  .errors.train<-R2(risky.train$risky,pred.train.risky)
  errors.train <- c(errors.train, .errors.train)
  
  pred.test.risky <- predict(risky.model, newdata=risky.test)
  .errors.test<-R2(risky.test$risky,pred.test.risky)
  errors.test <- c(errors.test, .errors.test) 
  
}

r2<- c(r2,  (errors.test),  (errors.train))
sd<- c(sd, sd(errors.test), sd(errors.train))

######big5:

errors.train <- NULL
errors.test<-NULL

for (k in 1:5){
  set.seed(k)
  size_test <- 250
  test_ind <- sample(seq_len(nrow(risky.all)),
                     size = size_test, 
                     replace = FALSE)
  risky.train <- risky.all[test_ind, ]
  risky.test <- risky.all[-test_ind, ]
  
  risky.model <- lm(risky~O+C+E+A+N ,data = risky.train) # train
  
  pred.train.risky <- predict(risky.model, newdata=risky.train)
  .errors.train<-R2(risky.train$risky,pred.train.risky)
  errors.train <- c(errors.train, .errors.train)
  
  pred.test.risky <- predict(risky.model, newdata=risky.test)
  .errors.test<-R2(risky.test$risky,pred.test.risky)
  errors.test <- c(errors.test, .errors.test) 
  
}

r2<- c(r2,  (errors.test),  (errors.train))
sd<- c(sd, sd(errors.test), sd(errors.train))

#######################################network#################################

#try to predict network with the new 5 and with 100 Q:

q.100<- as.data.table(
  as.matrix(test.network[,q1:q100]) %*% rotation.mat)
network.all<- as.data.table(cbind(test.network, q.100))
dim(network.all)

###predictive five:

errors.train <- NULL
errors.test<-NULL

for (k in 1:5){
  set.seed(k)
  size_test <- 250
  test_ind <- sample(seq_len(nrow(network.all)),
                     size = size_test, 
                     replace = FALSE)
  network.train <- network.all[test_ind, ]
  network.test <- network.all[-test_ind, ]
  
  network.model <- lm(network_size~V1+V2+V3+V4+V5 ,data = network.train) # train
  
  pred.train.network <- predict(network.model, newdata=network.train)
  .errors.train<-R2(network.train$network_size,pred.train.network)
  errors.train <- c(errors.train, .errors.train)
  
  pred.test.network <- predict(network.model, newdata=network.test)
  .errors.test<-R2(network.test$network_size,pred.test.network)
  errors.test <- c(errors.test, .errors.test) 
  
}

r2<- c(r2,  (errors.test),  (errors.train))
sd<- c(sd, sd(errors.test), sd(errors.train))

#all 100q:

network.100q<-as.data.table(cbind(network.all[,q1:q100],network=network.all$network))

errors.train <- NULL
errors.test<-NULL

for (k in 1:5){
  
  set.seed(k)
  size_test <- 250
  test_ind <- sample(seq_len(nrow(network.100q)),
                     size = size_test, 
                     replace = FALSE)
  network.train <- network.100q[test_ind, ]
  network.test <- network.100q[-test_ind, ]
  
  network.model <- lm(network~., data = network.train) # train
  
  pred.train.network <- predict(network.model, newdata=network.train)
  .errors.train<-R2(network.train$network,pred.train.network)
  errors.train <- c(errors.train, .errors.train)
  
  pred.test.network <- predict(network.model, newdata=network.test)
  .errors.test<-R2(network.test$network,pred.test.network)
  errors.test <- c(errors.test, .errors.test) 
  
}

r2<- c(r2,  (errors.test),  (errors.train))
sd<- c(sd, sd(errors.test), sd(errors.train))

######big5:

errors.train <- NULL
errors.test<-NULL

for (k in 1:5){
  set.seed(k)
  size_test <- 250
  test_ind <- sample(seq_len(nrow(network.all)),
                     size = size_test, 
                     replace = FALSE)
  network.train <- network.all[test_ind, ]
  network.test <- network.all[-test_ind, ]
  
  network.model <- lm(network_size~O+C+E+A+N ,data = network.train) # train
  
  pred.train.network <- predict(network.model, newdata=network.train)
  .errors.train<-R2(network.train$network_size,pred.train.network)
  errors.train <- c(errors.train, .errors.train)
  
  pred.test.network <- predict(network.model, newdata=network.test)
  .errors.test<-R2(network.test$network_size,pred.test.network)
  errors.test <- c(errors.test, .errors.test) 
  
}

r2<- c(r2,  (errors.test),  (errors.train))
sd<- c(sd, sd(errors.test), sd(errors.train))

#######################################nonhealthy#################################

#try to predict nonhealthy with the new 5 and with 100 Q:

q.100<- as.data.table(
  as.matrix(test.nonhealthy[,q1:q100]) %*% rotation.mat)
nonhealthy.all<- as.data.table(cbind(test.nonhealthy, q.100))
dim(nonhealthy.all)

###predictive five:

errors.train <- NULL
errors.test<-NULL

for (k in 21:25){
  set.seed(k)
  size_test <- 250
  test_ind <- sample(seq_len(nrow(nonhealthy.all)),
                     size = size_test, 
                     replace = FALSE)
  nonhealthy.train <- nonhealthy.all[test_ind, ]
  nonhealthy.test <- nonhealthy.all[-test_ind, ]
  
  nonhealthy.model <- lm(nonhealthy~V1+V2+V3+V4+V5 ,data = nonhealthy.train) # train
  
  pred.train.nonhealthy <- predict(nonhealthy.model, newdata=nonhealthy.train)
  .errors.train<-R2(nonhealthy.train$nonhealthy,pred.train.nonhealthy)
  errors.train <- c(errors.train, .errors.train)
  
  pred.test.nonhealthy <- predict(nonhealthy.model, newdata=nonhealthy.test)
  .errors.test<-R2(nonhealthy.test$nonhealthy,pred.test.nonhealthy)
  errors.test <- c(errors.test, .errors.test) 
  
}

r2<- c(r2,  (errors.test),  (errors.train))
sd<- c(sd, sd(errors.test), sd(errors.train))

#all 100q:

nonhealthy.100q<-as.data.table(cbind(nonhealthy.all[,q1:q100],nonhealthy=nonhealthy.all$nonhealthy))

errors.train <- NULL
errors.test<-NULL

for (k in 1:5){
  
  set.seed(k)
  size_test <- 250
  test_ind <- sample(seq_len(nrow(nonhealthy.100q)),
                     size = size_test, 
                     replace = FALSE)
  nonhealthy.train <- nonhealthy.100q[test_ind, ]
  nonhealthy.test <- nonhealthy.100q[-test_ind, ]
  
  nonhealthy.model <- lm(nonhealthy~., data = nonhealthy.train) # train
  
  pred.train.nonhealthy <- predict(nonhealthy.model, newdata=nonhealthy.train)
  .errors.train<-R2(nonhealthy.train$nonhealthy,pred.train.nonhealthy)
  errors.train <- c(errors.train, .errors.train)
  
  pred.test.nonhealthy <- predict(nonhealthy.model, newdata=nonhealthy.test)
  .errors.test<-R2(nonhealthy.test$nonhealthy,pred.test.nonhealthy)
  errors.test <- c(errors.test, .errors.test) 
  
}

r2<- c(r2,  (errors.test),  (errors.train))
sd<- c(sd, sd(errors.test), sd(errors.train))

######big5:

errors.train <- NULL
errors.test<-NULL

for (k in 1:5){
  set.seed(k)
  size_test <- 250
  test_ind <- sample(seq_len(nrow(nonhealthy.all)),
                     size = size_test, 
                     replace = FALSE)
  nonhealthy.train <- nonhealthy.all[test_ind, ]
  nonhealthy.test <- nonhealthy.all[-test_ind, ]
  
  nonhealthy.model <- lm(nonhealthy~O+C+E+A+N ,data = nonhealthy.train) # train
  
  pred.train.nonhealthy <- predict(nonhealthy.model, newdata=nonhealthy.train)
  .errors.train<-R2(nonhealthy.train$nonhealthy,pred.train.nonhealthy)
  errors.train <- c(errors.train, .errors.train)
  
  pred.test.nonhealthy <- predict(nonhealthy.model, newdata=nonhealthy.test)
  .errors.test<-R2(nonhealthy.test$nonhealthy,pred.test.nonhealthy)
  errors.test <- c(errors.test, .errors.test) 
  
}

r2<- c(r2,  (errors.test),  (errors.train))
sd<- c(sd, sd(errors.test), sd(errors.train))

#######################################svq.trans#################################

#try to predict svq.trans with the new 5 and with 100 Q:

q.100<- as.data.table(
  as.matrix(test.svq.trans[,q1:q100]) %*% rotation.mat)
svq.trans.all<- as.data.table(cbind(test.svq.trans, q.100))
dim(svq.trans.all)

###predictive five:

errors.train <- NULL
errors.test<-NULL

for (k in 1:5){
  set.seed(k)
  size_test <- 250
  test_ind <- sample(seq_len(nrow(svq.trans.all)),
                     size = size_test, 
                     replace = FALSE)
  svq.trans.train <- svq.trans.all[test_ind, ]
  svq.trans.test <- svq.trans.all[-test_ind, ]
  
  svq.trans.model <- lm(sv.trans~V1+V2+V3+V4+V5 ,data = svq.trans.train) # train
  
  pred.train.svq.trans <- predict(svq.trans.model, newdata=svq.trans.train)
  .errors.train<-R2(svq.trans.train$sv.trans,pred.train.svq.trans)
  errors.train <- c(errors.train, .errors.train)
  
  pred.test.svq.trans <- predict(svq.trans.model, newdata=svq.trans.test)
  .errors.test<-R2(svq.trans.test$sv.trans,pred.test.svq.trans)
  errors.test <- c(errors.test, .errors.test) 
  
}

r2<- c(r2,  (errors.test),  (errors.train))
sd<- c(sd, sd(errors.test), sd(errors.train))

#all 100q:

svq.trans.100q<-as.data.table(cbind(svq.trans.all[,q1:q100],svq.trans=svq.trans.all$sv.trans))

errors.train <- NULL
errors.test<-NULL

for (k in 1:5){
  
  set.seed(k)
  size_test <- 250
  test_ind <- sample(seq_len(nrow(svq.trans.100q)),
                     size = size_test, 
                     replace = FALSE)
  svq.trans.train <- svq.trans.100q[test_ind, ]
  svq.trans.test <- svq.trans.100q[-test_ind, ]
  
  svq.trans.model <- lm(svq.trans~., data = svq.trans.train) # train
  
  pred.train.svq.trans <- predict(svq.trans.model, newdata=svq.trans.train)
  .errors.train<-R2(svq.trans.train$svq.trans,pred.train.svq.trans)
  errors.train <- c(errors.train, .errors.train)
  
  pred.test.svq.trans <- predict(svq.trans.model, newdata=svq.trans.test)
  .errors.test<-R2(svq.trans.test$svq.trans,pred.test.svq.trans)
  errors.test <- c(errors.test, .errors.test) 
  
}

r2<- c(r2,  (errors.test),  (errors.train))
sd<- c(sd, sd(errors.test), sd(errors.train))

######big5:

errors.train <- NULL
errors.test<-NULL

for (k in 1:5){
  set.seed(k)
  size_test <- 250
  test_ind <- sample(seq_len(nrow(svq.trans.all)),
                     size = size_test, 
                     replace = FALSE)
  svq.trans.train <- svq.trans.all[test_ind, ]
  svq.trans.test <- svq.trans.all[-test_ind, ]
  
  svq.trans.model <- lm(sv.trans~O+C+E+A+N ,data = svq.trans.train) # train
  
  pred.train.svq.trans <- predict(svq.trans.model, newdata=svq.trans.train)
  .errors.train<-R2(svq.trans.train$sv.trans,pred.train.svq.trans)
  errors.train <- c(errors.train, .errors.train)
  
  pred.test.svq.trans <- predict(svq.trans.model, newdata=svq.trans.test)
  .errors.test<-R2(svq.trans.test$sv.trans,pred.test.svq.trans)
  errors.test <- c(errors.test, .errors.test) 
  
}

r2<- c(r2,  (errors.test),  (errors.train))
sd<- c(sd, sd(errors.test), sd(errors.train))


#######################################svq.openness#################################

#try to predict svq.openness with the new 5 and with 100 Q:

q.100<- as.data.table(
  as.matrix(test.svq.openness[,q1:q100]) %*% rotation.mat)
svq.openness.all<- as.data.table(cbind(test.svq.openness, q.100))
dim(svq.openness.all)

###predictive five:

errors.train <- NULL
errors.test<-NULL

for (k in 1:5){
  set.seed(k)
  size_test <- 250
  test_ind <- sample(seq_len(nrow(svq.openness.all)),
                     size = size_test, 
                     replace = FALSE)
  svq.openness.train <- svq.openness.all[test_ind, ]
  svq.openness.test <- svq.openness.all[-test_ind, ]
  
  svq.openness.model <- lm(sv.openness~V1+V2+V3+V4+V5 ,data = svq.openness.train) # train
  
  pred.train.svq.openness <- predict(svq.openness.model, newdata=svq.openness.train)
  .errors.train<-R2(svq.openness.train$sv.openness,pred.train.svq.openness)
  errors.train <- c(errors.train, .errors.train)
  
  pred.test.svq.openness <- predict(svq.openness.model, newdata=svq.openness.test)
  .errors.test<-R2(svq.openness.test$sv.openness,pred.test.svq.openness)
  errors.test <- c(errors.test, .errors.test) 
  
}

r2<- c(r2,  (errors.test),  (errors.train))
sd<- c(sd, sd(errors.test), sd(errors.train))

#all 100q:

svq.openness.100q<-as.data.table(cbind(svq.openness.all[,q1:q100],svq.openness=svq.openness.all$sv.openness))

errors.train <- NULL
errors.test<-NULL

for (k in 1:5){
  
  set.seed(k)
  size_test <- 250
  test_ind <- sample(seq_len(nrow(svq.openness.100q)),
                     size = size_test, 
                     replace = FALSE)
  svq.openness.train <- svq.openness.100q[test_ind, ]
  svq.openness.test <- svq.openness.100q[-test_ind, ]
  
  svq.openness.model <- lm(svq.openness~., data = svq.openness.train) # train
  
  pred.train.svq.openness <- predict(svq.openness.model, newdata=svq.openness.train)
  .errors.train<-R2(svq.openness.train$svq.openness,pred.train.svq.openness)
  errors.train <- c(errors.train, .errors.train)
  
  pred.test.svq.openness <- predict(svq.openness.model, newdata=svq.openness.test)
  .errors.test<-R2(svq.openness.test$svq.openness,pred.test.svq.openness)
  errors.test <- c(errors.test, .errors.test) 
  
}

r2<- c(r2,  (errors.test),  (errors.train))
sd<- c(sd, sd(errors.test), sd(errors.train))

######big5:

errors.train <- NULL
errors.test<-NULL

for (k in 1:5){
  set.seed(k)
  size_test <- 250
  test_ind <- sample(seq_len(nrow(svq.openness.all)),
                     size = size_test, 
                     replace = FALSE)
  svq.openness.train <- svq.openness.all[test_ind, ]
  svq.openness.test <- svq.openness.all[-test_ind, ]
  
  svq.openness.model <- lm(sv.openness~O+C+E+A+N ,data = svq.openness.train) # train
  
  pred.train.svq.openness <- predict(svq.openness.model, newdata=svq.openness.train)
  .errors.train<-R2(svq.openness.train$sv.openness,pred.train.svq.openness)
  errors.train <- c(errors.train, .errors.train)
  
  pred.test.svq.openness <- predict(svq.openness.model, newdata=svq.openness.test)
  .errors.test<-R2(svq.openness.test$sv.openness,pred.test.svq.openness)
  errors.test <- c(errors.test, .errors.test) 
  
}

r2<- c(r2,  (errors.test),  (errors.train))
sd<- c(sd, sd(errors.test), sd(errors.train))

#######################################empathy#################################

#try to predict empathy with the new 5 and with 100 Q:

q.100<- as.data.table(
  as.matrix(test.empathy[,q1:q100]) %*% rotation.mat)
empathy.all<- as.data.table(cbind(test.empathy, q.100))
dim(empathy.all)

###predictive five:

errors.train <- NULL
errors.test<-NULL

for (k in 21:25){
  set.seed(k)
  size_test <- 250
  test_ind <- sample(seq_len(nrow(empathy.all)),
                     size = size_test, 
                     replace = FALSE)
  empathy.train <- empathy.all[test_ind, ]
  empathy.test <- empathy.all[-test_ind, ]
  
  empathy.model <- lm(empathy~V1+V2+V3+V4+V5 ,data = empathy.train) # train
  
  pred.train.empathy <- predict(empathy.model, newdata=empathy.train)
  .errors.train<-R2(empathy.train$empathy,pred.train.empathy)
  errors.train <- c(errors.train, .errors.train)
  
  pred.test.empathy <- predict(empathy.model, newdata=empathy.test)
  .errors.test<-R2(empathy.test$empathy,pred.test.empathy)
  errors.test <- c(errors.test, .errors.test) 
  
}

r2<- c(r2,  (errors.test),  (errors.train))
sd<- c(sd, sd(errors.test), sd(errors.train))

#all 100q:

empathy.100q<-as.data.table(cbind(empathy.all[,q1:q100],empathy=empathy.all$empathy))

errors.train <- NULL
errors.test<-NULL

for (k in 1:5){
  
  set.seed(k)
  size_test <- 250
  test_ind <- sample(seq_len(nrow(empathy.100q)),
                     size = size_test, 
                     replace = FALSE)
  empathy.train <- empathy.100q[test_ind, ]
  empathy.test <- empathy.100q[-test_ind, ]
  
  empathy.model <- lm(empathy~., data = empathy.train) # train
  
  pred.train.empathy <- predict(empathy.model, newdata=empathy.train)
  .errors.train<-R2(empathy.train$empathy,pred.train.empathy)
  errors.train <- c(errors.train, .errors.train)
  
  pred.test.empathy <- predict(empathy.model, newdata=empathy.test)
  .errors.test<-R2(empathy.test$empathy,pred.test.empathy)
  errors.test <- c(errors.test, .errors.test) 
  
}

r2<- c(r2,  (errors.test),  (errors.train))
sd<- c(sd, sd(errors.test), sd(errors.train))

######big5:

errors.train <- NULL
errors.test<-NULL

for (k in 1:5){
  set.seed(k)
  size_test <- 250
  test_ind <- sample(seq_len(nrow(empathy.all)),
                     size = size_test, 
                     replace = FALSE)
  empathy.train <- empathy.all[test_ind, ]
  empathy.test <- empathy.all[-test_ind, ]
  
  empathy.model <- lm(empathy~O+C+E+A+N ,data = empathy.train) # train
  
  pred.train.empathy <- predict(empathy.model, newdata=empathy.train)
  .errors.train<-R2(empathy.train$empathy,pred.train.empathy)
  errors.train <- c(errors.train, .errors.train)
  
  pred.test.empathy <- predict(empathy.model, newdata=empathy.test)
  .errors.test<-R2(empathy.test$empathy,pred.test.empathy)
  errors.test <- c(errors.test, .errors.test) 
  
}

r2<- c(r2,  (errors.test),  (errors.train))
sd<- c(sd, sd(errors.test), sd(errors.train))

################################-12-DV##########################################

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

#Create the five new factors using a rotation matrix,
#and add them to the original table:

only.100<- as.matrix(new.data.100[,q1:q100])
new5<- only.100 %*% rotation.mat
all.new<-  as.data.table(cbind(new.data.100, new5))
dim(all.new)

##########################################DV1############################

###predictive five:

errors.train <- NULL
errors.test<-NULL

for (k in 21:25){
  set.seed(k)
  size_test <- 250
  test_ind <- sample(seq_len(nrow(all.new)),
                     size = size_test, 
                     replace = FALSE)
  DV.1.train <- all.new[test_ind, ]
  DV.1.test <- all.new[-test_ind, ]
  
  DV.1.model <- lm(DV.1~V1+V2+V3+V4+V5 ,data = DV.1.train) # train
  
  pred.train.DV.1 <- predict(DV.1.model, newdata=DV.1.train)
  .errors.train<-R2(DV.1.train$DV.1,pred.train.DV.1)
  errors.train <- c(errors.train, .errors.train)
  
  pred.test.DV.1 <- predict(DV.1.model, newdata=DV.1.test)
  .errors.test<-R2(DV.1.test$DV.1,pred.test.DV.1)
  errors.test <- c(errors.test, .errors.test) 
  
}

r2<- c(r2, (errors.test), (errors.train))
sd<- c(sd, sd(errors.test), sd(errors.train))

#all 100q:

DV.1.100q<-as.data.table(cbind(all.new[,q1:q100],DV.1=all.new$DV.1))

errors.train <- NULL
errors.test<-NULL

for (k in 1:5){
  
  set.seed(k)
  size_test <- 250
  test_ind <- sample(seq_len(nrow(DV.1.100q)),
                     size = size_test, 
                     replace = FALSE)
  DV.1.train <- DV.1.100q[test_ind, ]
  DV.1.test <- DV.1.100q[-test_ind, ]
  
  DV.1.model <- lm(DV.1~., data = DV.1.train) # train
  
  pred.train.DV.1 <- predict(DV.1.model, newdata=DV.1.train)
  .errors.train<-R2(DV.1.train$DV.1,pred.train.DV.1)
  errors.train <- c(errors.train, .errors.train)
  
  pred.test.DV.1 <- predict(DV.1.model, newdata=DV.1.test)
  .errors.test<-R2(DV.1.test$DV.1,pred.test.DV.1)
  errors.test <- c(errors.test, .errors.test) 
  
}
r2<- c(r2, (errors.test), (errors.train))
sd<- c(sd, sd(errors.test), sd(errors.train))

######big5:

errors.train <- NULL
errors.test<-NULL

for (k in 1:5){
  set.seed(k)
  size_test <- 250
  test_ind <- sample(seq_len(nrow(all.new)),
                     size = size_test, 
                     replace = FALSE)
  DV.1.train <- all.new[test_ind, ]
  DV.1.test <- all.new[-test_ind, ]
  
  DV.1.model <- lm(DV.1~O+C+E+A+N ,data = DV.1.train) # train
  
  pred.train.DV.1 <- predict(DV.1.model, newdata=DV.1.train)
  .errors.train<-R2(DV.1.train$DV.1,pred.train.DV.1)
  errors.train <- c(errors.train, .errors.train)
  
  pred.test.DV.1 <- predict(DV.1.model, newdata=DV.1.test)
  .errors.test<-R2(DV.1.test$DV.1,pred.test.DV.1)
  errors.test <- c(errors.test, .errors.test) 
  
}
r2<- c(r2, (errors.test), (errors.train))
sd<- c(sd, sd(errors.test), sd(errors.train))


##########################################DV.2############################

###predictive five:

errors.train <- NULL
errors.test<-NULL

for (k in 21:25){
  set.seed(k)
  size_test <- 250
  test_ind <- sample(seq_len(nrow(all.new)),
                     size = size_test, 
                     replace = FALSE)
  DV.2.train <- all.new[test_ind, ]
  DV.2.test <- all.new[-test_ind, ]
  
  DV.2.model <- lm(DV.2~V1+V2+V3+V4+V5 ,data = DV.2.train) # train
  
  pred.train.DV.2 <- predict(DV.2.model, newdata=DV.2.train)
  .errors.train<-R2(DV.2.train$DV.2,pred.train.DV.2)
  errors.train <- c(errors.train, .errors.train)
  
  pred.test.DV.2 <- predict(DV.2.model, newdata=DV.2.test)
  .errors.test<-R2(DV.2.test$DV.2,pred.test.DV.2)
  errors.test <- c(errors.test, .errors.test) 
  
}

r2<- c(r2, (errors.test), (errors.train))
sd<- c(sd, sd(errors.test), sd(errors.train))

#all 100q:

DV.2.100q<-as.data.table(cbind(all.new[,q1:q100],DV.2=all.new$DV.2))

errors.train <- NULL
errors.test<-NULL

for (k in 1:5){
  
  set.seed(k)
  size_test <- 250
  test_ind <- sample(seq_len(nrow(DV.2.100q)),
                     size = size_test, 
                     replace = FALSE)
  DV.2.train <- DV.2.100q[test_ind, ]
  DV.2.test <- DV.2.100q[-test_ind, ]
  
  DV.2.model <- lm(DV.2~., data = DV.2.train) # train
  
  pred.train.DV.2 <- predict(DV.2.model, newdata=DV.2.train)
  .errors.train<-R2(DV.2.train$DV.2,pred.train.DV.2)
  errors.train <- c(errors.train, .errors.train)
  
  pred.test.DV.2 <- predict(DV.2.model, newdata=DV.2.test)
  .errors.test<-R2(DV.2.test$DV.2,pred.test.DV.2)
  errors.test <- c(errors.test, .errors.test) 
  
}
r2<- c(r2, (errors.test), (errors.train))
sd<- c(sd, sd(errors.test), sd(errors.train))

######big5:

errors.train <- NULL
errors.test<-NULL

for (k in 1:5){
  set.seed(k)
  size_test <- 250
  test_ind <- sample(seq_len(nrow(all.new)),
                     size = size_test, 
                     replace = FALSE)
  DV.2.train <- all.new[test_ind, ]
  DV.2.test <- all.new[-test_ind, ]
  
  DV.2.model <- lm(DV.2~O+C+E+A+N ,data = DV.2.train) # train
  
  pred.train.DV.2 <- predict(DV.2.model, newdata=DV.2.train)
  .errors.train<-R2(DV.2.train$DV.2,pred.train.DV.2)
  errors.train <- c(errors.train, .errors.train)
  
  pred.test.DV.2 <- predict(DV.2.model, newdata=DV.2.test)
  .errors.test<-R2(DV.2.test$DV.2,pred.test.DV.2)
  errors.test <- c(errors.test, .errors.test) 
  
}
r2<- c(r2, (errors.test), (errors.train))
sd<- c(sd, sd(errors.test), sd(errors.train))


##########################################DV.3############################

###predictive five:

errors.train <- NULL
errors.test<-NULL

for (k in 21:25){
  set.seed(k)
  size_test <- 250
  test_ind <- sample(seq_len(nrow(all.new)),
                     size = size_test, 
                     replace = FALSE)
  DV.3.train <- all.new[test_ind, ]
  DV.3.test <- all.new[-test_ind, ]
  
  DV.3.model <- lm(DV.3~V1+V2+V3+V4+V5 ,data = DV.3.train) # train
  
  pred.train.DV.3 <- predict(DV.3.model, newdata=DV.3.train)
  .errors.train<-R2(DV.3.train$DV.3,pred.train.DV.3)
  errors.train <- c(errors.train, .errors.train)
  
  pred.test.DV.3 <- predict(DV.3.model, newdata=DV.3.test)
  .errors.test<-R2(DV.3.test$DV.3,pred.test.DV.3)
  errors.test <- c(errors.test, .errors.test) 
  
}

r2<- c(r2, (errors.test), (errors.train))
sd<- c(sd, sd(errors.test), sd(errors.train))

#all 100q:

DV.3.100q<-as.data.table(cbind(all.new[,q1:q100],DV.3=all.new$DV.3))

errors.train <- NULL
errors.test<-NULL

for (k in 1:5){
  
  set.seed(k)
  size_test <- 250
  test_ind <- sample(seq_len(nrow(DV.3.100q)),
                     size = size_test, 
                     replace = FALSE)
  DV.3.train <- DV.3.100q[test_ind, ]
  DV.3.test <- DV.3.100q[-test_ind, ]
  
  DV.3.model <- lm(DV.3~., data = DV.3.train) # train
  
  pred.train.DV.3 <- predict(DV.3.model, newdata=DV.3.train)
  .errors.train<-R2(DV.3.train$DV.3,pred.train.DV.3)
  errors.train <- c(errors.train, .errors.train)
  
  pred.test.DV.3 <- predict(DV.3.model, newdata=DV.3.test)
  .errors.test<-R2(DV.3.test$DV.3,pred.test.DV.3)
  errors.test <- c(errors.test, .errors.test) 
  
}
r2<- c(r2, (errors.test), (errors.train))
sd<- c(sd, sd(errors.test), sd(errors.train))

######big5:

errors.train <- NULL
errors.test<-NULL

for (k in 1:5){
  set.seed(k)
  size_test <- 250
  test_ind <- sample(seq_len(nrow(all.new)),
                     size = size_test, 
                     replace = FALSE)
  DV.3.train <- all.new[test_ind, ]
  DV.3.test <- all.new[-test_ind, ]
  
  DV.3.model <- lm(DV.3~O+C+E+A+N ,data = DV.3.train) # train
  
  pred.train.DV.3 <- predict(DV.3.model, newdata=DV.3.train)
  .errors.train<-R2(DV.3.train$DV.3,pred.train.DV.3)
  errors.train <- c(errors.train, .errors.train)
  
  pred.test.DV.3 <- predict(DV.3.model, newdata=DV.3.test)
  .errors.test<-R2(DV.3.test$DV.3,pred.test.DV.3)
  errors.test <- c(errors.test, .errors.test) 
  
}
r2<- c(r2, (errors.test), (errors.train))
sd<- c(sd, sd(errors.test), sd(errors.train))


##########################################DV.4############################

###predictive five:

errors.train <- NULL
errors.test<-NULL

for (k in 21:25){
  set.seed(k)
  size_test <- 250
  test_ind <- sample(seq_len(nrow(all.new)),
                     size = size_test, 
                     replace = FALSE)
  DV.4.train <- all.new[test_ind, ]
  DV.4.test <- all.new[-test_ind, ]
  
  DV.4.model <- lm(DV.4~V1+V2+V3+V4+V5 ,data = DV.4.train) # train
  
  pred.train.DV.4 <- predict(DV.4.model, newdata=DV.4.train)
  .errors.train<-R2(DV.4.train$DV.4,pred.train.DV.4)
  errors.train <- c(errors.train, .errors.train)
  
  pred.test.DV.4 <- predict(DV.4.model, newdata=DV.4.test)
  .errors.test<-R2(DV.4.test$DV.4,pred.test.DV.4)
  errors.test <- c(errors.test, .errors.test) 
  
}

r2<- c(r2, (errors.test), (errors.train))
sd<- c(sd, sd(errors.test), sd(errors.train))

#all 100q:

DV.4.100q<-as.data.table(cbind(all.new[,q1:q100],DV.4=all.new$DV.4))

errors.train <- NULL
errors.test<-NULL

for (k in 1:5){
  
  set.seed(k)
  size_test <- 250
  test_ind <- sample(seq_len(nrow(DV.4.100q)),
                     size = size_test, 
                     replace = FALSE)
  DV.4.train <- DV.4.100q[test_ind, ]
  DV.4.test <- DV.4.100q[-test_ind, ]
  
  DV.4.model <- lm(DV.4~., data = DV.4.train) # train
  
  pred.train.DV.4 <- predict(DV.4.model, newdata=DV.4.train)
  .errors.train<-R2(DV.4.train$DV.4,pred.train.DV.4)
  errors.train <- c(errors.train, .errors.train)
  
  pred.test.DV.4 <- predict(DV.4.model, newdata=DV.4.test)
  .errors.test<-R2(DV.4.test$DV.4,pred.test.DV.4)
  errors.test <- c(errors.test, .errors.test) 
  
}
r2<- c(r2, (errors.test), (errors.train))
sd<- c(sd, sd(errors.test), sd(errors.train))

######big5:

errors.train <- NULL
errors.test<-NULL

for (k in 1:5){
  set.seed(k)
  size_test <- 250
  test_ind <- sample(seq_len(nrow(all.new)),
                     size = size_test, 
                     replace = FALSE)
  DV.4.train <- all.new[test_ind, ]
  DV.4.test <- all.new[-test_ind, ]
  
  DV.4.model <- lm(DV.4~O+C+E+A+N ,data = DV.4.train) # train
  
  pred.train.DV.4 <- predict(DV.4.model, newdata=DV.4.train)
  .errors.train<-R2(DV.4.train$DV.4,pred.train.DV.4)
  errors.train <- c(errors.train, .errors.train)
  
  pred.test.DV.4 <- predict(DV.4.model, newdata=DV.4.test)
  .errors.test<-R2(DV.4.test$DV.4,pred.test.DV.4)
  errors.test <- c(errors.test, .errors.test) 
  
}
r2<- c(r2, (errors.test), (errors.train))
sd<- c(sd, sd(errors.test), sd(errors.train))


##########################################DV.5############################

###predictive five:

errors.train <- NULL
errors.test<-NULL

for (k in 21:25){
  set.seed(k)
  size_test <- 250
  test_ind <- sample(seq_len(nrow(all.new)),
                     size = size_test, 
                     replace = FALSE)
  DV.5.train <- all.new[test_ind, ]
  DV.5.test <- all.new[-test_ind, ]
  
  DV.5.model <- lm(DV.5~V1+V2+V3+V4+V5 ,data = DV.5.train) # train
  
  pred.train.DV.5 <- predict(DV.5.model, newdata=DV.5.train)
  .errors.train<-R2(DV.5.train$DV.5,pred.train.DV.5)
  errors.train <- c(errors.train, .errors.train)
  
  pred.test.DV.5 <- predict(DV.5.model, newdata=DV.5.test)
  .errors.test<-R2(DV.5.test$DV.5,pred.test.DV.5)
  errors.test <- c(errors.test, .errors.test) 
  
}

r2<- c(r2, (errors.test), (errors.train))
sd<- c(sd, sd(errors.test), sd(errors.train))

#all 100q:

DV.5.100q<-as.data.table(cbind(all.new[,q1:q100],DV.5=all.new$DV.5))

errors.train <- NULL
errors.test<-NULL

for (k in 1:5){
  
  set.seed(k)
  size_test <- 250
  test_ind <- sample(seq_len(nrow(DV.5.100q)),
                     size = size_test, 
                     replace = FALSE)
  DV.5.train <- DV.5.100q[test_ind, ]
  DV.5.test <- DV.5.100q[-test_ind, ]
  
  DV.5.model <- lm(DV.5~., data = DV.5.train) # train
  
  pred.train.DV.5 <- predict(DV.5.model, newdata=DV.5.train)
  .errors.train<-R2(DV.5.train$DV.5,pred.train.DV.5)
  errors.train <- c(errors.train, .errors.train)
  
  pred.test.DV.5 <- predict(DV.5.model, newdata=DV.5.test)
  .errors.test<-R2(DV.5.test$DV.5,pred.test.DV.5)
  errors.test <- c(errors.test, .errors.test) 
  
}
r2<- c(r2, (errors.test), (errors.train))
sd<- c(sd, sd(errors.test), sd(errors.train))

######big5:

errors.train <- NULL
errors.test<-NULL

for (k in 1:5){
  set.seed(k)
  size_test <- 250
  test_ind <- sample(seq_len(nrow(all.new)),
                     size = size_test, 
                     replace = FALSE)
  DV.5.train <- all.new[test_ind, ]
  DV.5.test <- all.new[-test_ind, ]
  
  DV.5.model <- lm(DV.5~O+C+E+A+N ,data = DV.5.train) # train
  
  pred.train.DV.5 <- predict(DV.5.model, newdata=DV.5.train)
  .errors.train<-R2(DV.5.train$DV.5,pred.train.DV.5)
  errors.train <- c(errors.train, .errors.train)
  
  pred.test.DV.5 <- predict(DV.5.model, newdata=DV.5.test)
  .errors.test<-R2(DV.5.test$DV.5,pred.test.DV.5)
  errors.test <- c(errors.test, .errors.test) 
  
}
r2<- c(r2, (errors.test), (errors.train))
sd<- c(sd, sd(errors.test), sd(errors.train))

##########################################DV.6############################

###predictive five:

errors.train <- NULL
errors.test<-NULL

for (k in 21:25){
  set.seed(k)
  size_test <- 250
  test_ind <- sample(seq_len(nrow(all.new)),
                     size = size_test, 
                     replace = FALSE)
  DV.6.train <- all.new[test_ind, ]
  DV.6.test <- all.new[-test_ind, ]
  
  DV.6.model <- lm(DV.6~V1+V2+V3+V4+V5 ,data = DV.6.train) # train
  
  pred.train.DV.6 <- predict(DV.6.model, newdata=DV.6.train)
  .errors.train<-R2(DV.6.train$DV.6,pred.train.DV.6)
  errors.train <- c(errors.train, .errors.train)
  
  pred.test.DV.6 <- predict(DV.6.model, newdata=DV.6.test)
  .errors.test<-R2(DV.6.test$DV.6,pred.test.DV.6)
  errors.test <- c(errors.test, .errors.test) 
  
}

r2<- c(r2, (errors.test), (errors.train))
sd<- c(sd, sd(errors.test), sd(errors.train))

#all 100q:

DV.6.100q<-as.data.table(cbind(all.new[,q1:q100],DV.6=all.new$DV.6))

errors.train <- NULL
errors.test<-NULL

for (k in 1:5){
  
  set.seed(k)
  size_test <- 250
  test_ind <- sample(seq_len(nrow(DV.6.100q)),
                     size = size_test, 
                     replace = FALSE)
  DV.6.train <- DV.6.100q[test_ind, ]
  DV.6.test <- DV.6.100q[-test_ind, ]
  
  DV.6.model <- lm(DV.6~., data = DV.6.train) # train
  
  pred.train.DV.6 <- predict(DV.6.model, newdata=DV.6.train)
  .errors.train<-R2(DV.6.train$DV.6,pred.train.DV.6)
  errors.train <- c(errors.train, .errors.train)
  
  pred.test.DV.6 <- predict(DV.6.model, newdata=DV.6.test)
  .errors.test<-R2(DV.6.test$DV.6,pred.test.DV.6)
  errors.test <- c(errors.test, .errors.test) 
  
}
r2<- c(r2, (errors.test), (errors.train))
sd<- c(sd, sd(errors.test), sd(errors.train))

######big5:

errors.train <- NULL
errors.test<-NULL

for (k in 1:5){
  set.seed(k)
  size_test <- 250
  test_ind <- sample(seq_len(nrow(all.new)),
                     size = size_test, 
                     replace = FALSE)
  DV.6.train <- all.new[test_ind, ]
  DV.6.test <- all.new[-test_ind, ]
  
  DV.6.model <- lm(DV.6~O+C+E+A+N ,data = DV.6.train) # train
  
  pred.train.DV.6 <- predict(DV.6.model, newdata=DV.6.train)
  .errors.train<-R2(DV.6.train$DV.6,pred.train.DV.6)
  errors.train <- c(errors.train, .errors.train)
  
  pred.test.DV.6 <- predict(DV.6.model, newdata=DV.6.test)
  .errors.test<-R2(DV.6.test$DV.6,pred.test.DV.6)
  errors.test <- c(errors.test, .errors.test) 
  
}
r2<- c(r2, (errors.test), (errors.train))
sd<- c(sd, sd(errors.test), sd(errors.train))

##########################################DV.7############################

###predictive five:

errors.train <- NULL
errors.test<-NULL

for (k in 21:25){
  set.seed(k)
  size_test <- 250
  test_ind <- sample(seq_len(nrow(all.new)),
                     size = size_test, 
                     replace = FALSE)
  DV.7.train <- all.new[test_ind, ]
  DV.7.test <- all.new[-test_ind, ]
  
  DV.7.model <- lm(DV.7~V1+V2+V3+V4+V5 ,data = DV.7.train) # train
  
  pred.train.DV.7 <- predict(DV.7.model, newdata=DV.7.train)
  .errors.train<-R2(DV.7.train$DV.7,pred.train.DV.7)
  errors.train <- c(errors.train, .errors.train)
  
  pred.test.DV.7 <- predict(DV.7.model, newdata=DV.7.test)
  .errors.test<-R2(DV.7.test$DV.7,pred.test.DV.7)
  errors.test <- c(errors.test, .errors.test) 
  
}

r2<- c(r2, (errors.test), (errors.train))
sd<- c(sd, sd(errors.test), sd(errors.train))

#all 100q:

DV.7.100q<-as.data.table(cbind(all.new[,q1:q100],DV.7=all.new$DV.7))

errors.train <- NULL
errors.test<-NULL

for (k in 1:5){
  
  set.seed(k)
  size_test <- 250
  test_ind <- sample(seq_len(nrow(DV.7.100q)),
                     size = size_test, 
                     replace = FALSE)
  DV.7.train <- DV.7.100q[test_ind, ]
  DV.7.test <- DV.7.100q[-test_ind, ]
  
  DV.7.model <- lm(DV.7~., data = DV.7.train) # train
  
  pred.train.DV.7 <- predict(DV.7.model, newdata=DV.7.train)
  .errors.train<-R2(DV.7.train$DV.7,pred.train.DV.7)
  errors.train <- c(errors.train, .errors.train)
  
  pred.test.DV.7 <- predict(DV.7.model, newdata=DV.7.test)
  .errors.test<-R2(DV.7.test$DV.7,pred.test.DV.7)
  errors.test <- c(errors.test, .errors.test) 
  
}
r2<- c(r2, (errors.test), (errors.train))
sd<- c(sd, sd(errors.test), sd(errors.train))

######big5:

errors.train <- NULL
errors.test<-NULL

for (k in 1:5){
  set.seed(k)
  size_test <- 250
  test_ind <- sample(seq_len(nrow(all.new)),
                     size = size_test, 
                     replace = FALSE)
  DV.7.train <- all.new[test_ind, ]
  DV.7.test <- all.new[-test_ind, ]
  
  DV.7.model <- lm(DV.7~O+C+E+A+N ,data = DV.7.train) # train
  
  pred.train.DV.7 <- predict(DV.7.model, newdata=DV.7.train)
  .errors.train<-R2(DV.7.train$DV.7,pred.train.DV.7)
  errors.train <- c(errors.train, .errors.train)
  
  pred.test.DV.7 <- predict(DV.7.model, newdata=DV.7.test)
  .errors.test<-R2(DV.7.test$DV.7,pred.test.DV.7)
  errors.test <- c(errors.test, .errors.test) 
  
}
r2<- c(r2, (errors.test), (errors.train))
sd<- c(sd, sd(errors.test), sd(errors.train))


##########################################DV.8############################

###predictive five:

errors.train <- NULL
errors.test<-NULL

for (k in 21:25){
  set.seed(k)
  size_test <- 250
  test_ind <- sample(seq_len(nrow(all.new)),
                     size = size_test, 
                     replace = FALSE)
  DV.8.train <- all.new[test_ind, ]
  DV.8.test <- all.new[-test_ind, ]
  
  DV.8.model <- lm(DV.8~V1+V2+V3+V4+V5 ,data = DV.8.train) # train
  
  pred.train.DV.8 <- predict(DV.8.model, newdata=DV.8.train)
  .errors.train<-R2(DV.8.train$DV.8,pred.train.DV.8)
  errors.train <- c(errors.train, .errors.train)
  
  pred.test.DV.8 <- predict(DV.8.model, newdata=DV.8.test)
  .errors.test<-R2(DV.8.test$DV.8,pred.test.DV.8)
  errors.test <- c(errors.test, .errors.test) 
  
}

r2<- c(r2, (errors.test), (errors.train))
sd<- c(sd, sd(errors.test), sd(errors.train))

#all 100q:

DV.8.100q<-as.data.table(cbind(all.new[,q1:q100],DV.8=all.new$DV.8))

errors.train <- NULL
errors.test<-NULL

for (k in 1:5){
  
  set.seed(k)
  size_test <- 250
  test_ind <- sample(seq_len(nrow(DV.8.100q)),
                     size = size_test, 
                     replace = FALSE)
  DV.8.train <- DV.8.100q[test_ind, ]
  DV.8.test <- DV.8.100q[-test_ind, ]
  
  DV.8.model <- lm(DV.8~., data = DV.8.train) # train
  
  pred.train.DV.8 <- predict(DV.8.model, newdata=DV.8.train)
  .errors.train<-R2(DV.8.train$DV.8,pred.train.DV.8)
  errors.train <- c(errors.train, .errors.train)
  
  pred.test.DV.8 <- predict(DV.8.model, newdata=DV.8.test)
  .errors.test<-R2(DV.8.test$DV.8,pred.test.DV.8)
  errors.test <- c(errors.test, .errors.test) 
  
}
r2<- c(r2, (errors.test), (errors.train))
sd<- c(sd, sd(errors.test), sd(errors.train))

######big5:

errors.train <- NULL
errors.test<-NULL

for (k in 1:5){
  set.seed(k)
  size_test <- 250
  test_ind <- sample(seq_len(nrow(all.new)),
                     size = size_test, 
                     replace = FALSE)
  DV.8.train <- all.new[test_ind, ]
  DV.8.test <- all.new[-test_ind, ]
  
  DV.8.model <- lm(DV.8~O+C+E+A+N ,data = DV.8.train) # train
  
  pred.train.DV.8 <- predict(DV.8.model, newdata=DV.8.train)
  .errors.train<-R2(DV.8.train$DV.8,pred.train.DV.8)
  errors.train <- c(errors.train, .errors.train)
  
  pred.test.DV.8 <- predict(DV.8.model, newdata=DV.8.test)
  .errors.test<-R2(DV.8.test$DV.8,pred.test.DV.8)
  errors.test <- c(errors.test, .errors.test) 
  
}
r2<- c(r2, (errors.test), (errors.train))
sd<- c(sd, sd(errors.test), sd(errors.train))

##########################################DV.9############################

###predictive five:

errors.train <- NULL
errors.test<-NULL

for (k in 21:25){
  set.seed(k)
  size_test <- 250
  test_ind <- sample(seq_len(nrow(all.new)),
                     size = size_test, 
                     replace = FALSE)
  DV.9.train <- all.new[test_ind, ]
  DV.9.test <- all.new[-test_ind, ]
  
  DV.9.model <- lm(DV.9~V1+V2+V3+V4+V5 ,data = DV.9.train) # train
  
  pred.train.DV.9 <- predict(DV.9.model, newdata=DV.9.train)
  .errors.train<-R2(DV.9.train$DV.9,pred.train.DV.9)
  errors.train <- c(errors.train, .errors.train)
  
  pred.test.DV.9 <- predict(DV.9.model, newdata=DV.9.test)
  .errors.test<-R2(DV.9.test$DV.9,pred.test.DV.9)
  errors.test <- c(errors.test, .errors.test) 
  
}

r2<- c(r2, (errors.test), (errors.train))
sd<- c(sd, sd(errors.test), sd(errors.train))

#all 100q:

DV.9.100q<-as.data.table(cbind(all.new[,q1:q100],DV.9=all.new$DV.9))

errors.train <- NULL
errors.test<-NULL

for (k in 1:5){
  
  set.seed(k)
  size_test <- 250
  test_ind <- sample(seq_len(nrow(DV.9.100q)),
                     size = size_test, 
                     replace = FALSE)
  DV.9.train <- DV.9.100q[test_ind, ]
  DV.9.test <- DV.9.100q[-test_ind, ]
  
  DV.9.model <- lm(DV.9~., data = DV.9.train) # train
  
  pred.train.DV.9 <- predict(DV.9.model, newdata=DV.9.train)
  .errors.train<-R2(DV.9.train$DV.9,pred.train.DV.9)
  errors.train <- c(errors.train, .errors.train)
  
  pred.test.DV.9 <- predict(DV.9.model, newdata=DV.9.test)
  .errors.test<-R2(DV.9.test$DV.9,pred.test.DV.9)
  errors.test <- c(errors.test, .errors.test) 
  
}
r2<- c(r2, (errors.test), (errors.train))
sd<- c(sd, sd(errors.test), sd(errors.train))

######big5:

errors.train <- NULL
errors.test<-NULL

for (k in 1:5){
  set.seed(k)
  size_test <- 250
  test_ind <- sample(seq_len(nrow(all.new)),
                     size = size_test, 
                     replace = FALSE)
  DV.9.train <- all.new[test_ind, ]
  DV.9.test <- all.new[-test_ind, ]
  
  DV.9.model <- lm(DV.9~O+C+E+A+N ,data = DV.9.train) # train
  
  pred.train.DV.9 <- predict(DV.9.model, newdata=DV.9.train)
  .errors.train<-R2(DV.9.train$DV.9,pred.train.DV.9)
  errors.train <- c(errors.train, .errors.train)
  
  pred.test.DV.9 <- predict(DV.9.model, newdata=DV.9.test)
  .errors.test<-R2(DV.9.test$DV.9,pred.test.DV.9)
  errors.test <- c(errors.test, .errors.test) 
  
}
r2<- c(r2, (errors.test), (errors.train))
sd<- c(sd, sd(errors.test), sd(errors.train))


##########################################DV.10############################

###predictive five:

errors.train <- NULL
errors.test<-NULL

for (k in 21:25){
  set.seed(k)
  size_test <- 250
  test_ind <- sample(seq_len(nrow(all.new)),
                     size = size_test, 
                     replace = FALSE)
  DV.10.train <- all.new[test_ind, ]
  DV.10.test <- all.new[-test_ind, ]
  
  DV.10.model <- lm(DV.10~V1+V2+V3+V4+V5 ,data = DV.10.train) # train
  
  pred.train.DV.10 <- predict(DV.10.model, newdata=DV.10.train)
  .errors.train<-R2(DV.10.train$DV.10,pred.train.DV.10)
  errors.train <- c(errors.train, .errors.train)
  
  pred.test.DV.10 <- predict(DV.10.model, newdata=DV.10.test)
  .errors.test<-R2(DV.10.test$DV.10,pred.test.DV.10)
  errors.test <- c(errors.test, .errors.test) 
  
}

r2<- c(r2, (errors.test), (errors.train))
sd<- c(sd, sd(errors.test), sd(errors.train))

#all 100q:

DV.10.100q<-as.data.table(cbind(all.new[,q1:q100],DV.10=all.new$DV.10))

errors.train <- NULL
errors.test<-NULL

for (k in 1:5){
  
  set.seed(k)
  size_test <- 250
  test_ind <- sample(seq_len(nrow(DV.10.100q)),
                     size = size_test, 
                     replace = FALSE)
  DV.10.train <- DV.10.100q[test_ind, ]
  DV.10.test <- DV.10.100q[-test_ind, ]
  
  DV.10.model <- lm(DV.10~., data = DV.10.train) # train
  
  pred.train.DV.10 <- predict(DV.10.model, newdata=DV.10.train)
  .errors.train<-R2(DV.10.train$DV.10,pred.train.DV.10)
  errors.train <- c(errors.train, .errors.train)
  
  pred.test.DV.10 <- predict(DV.10.model, newdata=DV.10.test)
  .errors.test<-R2(DV.10.test$DV.10,pred.test.DV.10)
  errors.test <- c(errors.test, .errors.test) 
  
}
r2<- c(r2, (errors.test), (errors.train))
sd<- c(sd, sd(errors.test), sd(errors.train))

######big5:

errors.train <- NULL
errors.test<-NULL

for (k in 1:5){
  set.seed(k)
  size_test <- 250
  test_ind <- sample(seq_len(nrow(all.new)),
                     size = size_test, 
                     replace = FALSE)
  DV.10.train <- all.new[test_ind, ]
  DV.10.test <- all.new[-test_ind, ]
  
  DV.10.model <- lm(DV.10~O+C+E+A+N ,data = DV.10.train) # train
  
  pred.train.DV.10 <- predict(DV.10.model, newdata=DV.10.train)
  .errors.train<-R2(DV.10.train$DV.10,pred.train.DV.10)
  errors.train <- c(errors.train, .errors.train)
  
  pred.test.DV.10 <- predict(DV.10.model, newdata=DV.10.test)
  .errors.test<-R2(DV.10.test$DV.10,pred.test.DV.10)
  errors.test <- c(errors.test, .errors.test) 
  
}
r2<- c(r2, (errors.test), (errors.train))
sd<- c(sd, sd(errors.test), sd(errors.train))


##########################################DV.11############################

###predictive five:

errors.train <- NULL
errors.test<-NULL

for (k in 21:25){
  set.seed(k)
  size_test <- 250
  test_ind <- sample(seq_len(nrow(all.new)),
                     size = size_test, 
                     replace = FALSE)
  DV.11.train <- all.new[test_ind, ]
  DV.11.test <- all.new[-test_ind, ]
  
  DV.11.model <- lm(DV.11~V1+V2+V3+V4+V5 ,data = DV.11.train) # train
  
  pred.train.DV.11 <- predict(DV.11.model, newdata=DV.11.train)
  .errors.train<-R2(DV.11.train$DV.11,pred.train.DV.11)
  errors.train <- c(errors.train, .errors.train)
  
  pred.test.DV.11 <- predict(DV.11.model, newdata=DV.11.test)
  .errors.test<-R2(DV.11.test$DV.11,pred.test.DV.11)
  errors.test <- c(errors.test, .errors.test) 
  
}

r2<- c(r2, (errors.test), (errors.train))
sd<- c(sd, sd(errors.test), sd(errors.train))

#all 100q:

DV.11.100q<-as.data.table(cbind(all.new[,q1:q100],DV.11=all.new$DV.11))

errors.train <- NULL
errors.test<-NULL

for (k in 1:5){
  
  set.seed(k)
  size_test <- 250
  test_ind <- sample(seq_len(nrow(DV.11.100q)),
                     size = size_test, 
                     replace = FALSE)
  DV.11.train <- DV.11.100q[test_ind, ]
  DV.11.test <- DV.11.100q[-test_ind, ]
  
  DV.11.model <- lm(DV.11~., data = DV.11.train) # train
  
  pred.train.DV.11 <- predict(DV.11.model, newdata=DV.11.train)
  .errors.train<-R2(DV.11.train$DV.11,pred.train.DV.11)
  errors.train <- c(errors.train, .errors.train)
  
  pred.test.DV.11 <- predict(DV.11.model, newdata=DV.11.test)
  .errors.test<-R2(DV.11.test$DV.11,pred.test.DV.11)
  errors.test <- c(errors.test, .errors.test) 
  
}
r2<- c(r2, (errors.test), (errors.train))
sd<- c(sd, sd(errors.test), sd(errors.train))

######big5:

errors.train <- NULL
errors.test<-NULL

for (k in 1:5){
  set.seed(k)
  size_test <- 250
  test_ind <- sample(seq_len(nrow(all.new)),
                     size = size_test, 
                     replace = FALSE)
  DV.11.train <- all.new[test_ind, ]
  DV.11.test <- all.new[-test_ind, ]
  
  DV.11.model <- lm(DV.11~O+C+E+A+N ,data = DV.11.train) # train
  
  pred.train.DV.11 <- predict(DV.11.model, newdata=DV.11.train)
  .errors.train<-R2(DV.11.train$DV.11,pred.train.DV.11)
  errors.train <- c(errors.train, .errors.train)
  
  pred.test.DV.11 <- predict(DV.11.model, newdata=DV.11.test)
  .errors.test<-R2(DV.11.test$DV.11,pred.test.DV.11)
  errors.test <- c(errors.test, .errors.test) 
  
}
r2<- c(r2, (errors.test), (errors.train))
sd<- c(sd, sd(errors.test), sd(errors.train))


##########################################DV.12############################

###predictive five:

errors.train <- NULL
errors.test<-NULL

for (k in 21:25){
  set.seed(k)
  size_test <- 250
  test_ind <- sample(seq_len(nrow(all.new)),
                     size = size_test, 
                     replace = FALSE)
  DV.12.train <- all.new[test_ind, ]
  DV.12.test <- all.new[-test_ind, ]
  
  DV.12.model <- lm(DV.12~V1+V2+V3+V4+V5 ,data = DV.12.train) # train
  
  pred.train.DV.12 <- predict(DV.12.model, newdata=DV.12.train)
  .errors.train<-R2(DV.12.train$DV.12,pred.train.DV.12)
  errors.train <- c(errors.train, .errors.train)
  
  pred.test.DV.12 <- predict(DV.12.model, newdata=DV.12.test)
  .errors.test<-R2(DV.12.test$DV.12,pred.test.DV.12)
  errors.test <- c(errors.test, .errors.test) 
  
}

r2<- c(r2, (errors.test), (errors.train))
sd<- c(sd, sd(errors.test), sd(errors.train))

#all 100q:

DV.12.100q<-as.data.table(cbind(all.new[,q1:q100],DV.12=all.new$DV.12))

errors.train <- NULL
errors.test<-NULL

for (k in 1:5){
  
  set.seed(k)
  size_test <- 250
  test_ind <- sample(seq_len(nrow(DV.12.100q)),
                     size = size_test, 
                     replace = FALSE)
  DV.12.train <- DV.12.100q[test_ind, ]
  DV.12.test <- DV.12.100q[-test_ind, ]
  
  DV.12.model <- lm(DV.12~., data = DV.12.train) # train
  
  pred.train.DV.12 <- predict(DV.12.model, newdata=DV.12.train)
  .errors.train<-R2(DV.12.train$DV.12,pred.train.DV.12)
  errors.train <- c(errors.train, .errors.train)
  
  pred.test.DV.12 <- predict(DV.12.model, newdata=DV.12.test)
  .errors.test<-R2(DV.12.test$DV.12,pred.test.DV.12)
  errors.test <- c(errors.test, .errors.test) 
  
}
r2<- c(r2, (errors.test), (errors.train))
sd<- c(sd, sd(errors.test), sd(errors.train))

######big5:

errors.train <- NULL
errors.test<-NULL

for (k in 1:5){
  set.seed(k)
  size_test <- 250
  test_ind <- sample(seq_len(nrow(all.new)),
                     size = size_test, 
                     replace = FALSE)
  DV.12.train <- all.new[test_ind, ]
  DV.12.test <- all.new[-test_ind, ]
  
  DV.12.model <- lm(DV.12~O+C+E+A+N ,data = DV.12.train) # train
  
  pred.train.DV.12 <- predict(DV.12.model, newdata=DV.12.train)
  .errors.train<-R2(DV.12.train$DV.12,pred.train.DV.12)
  errors.train <- c(errors.train, .errors.train)
  
  pred.test.DV.12 <- predict(DV.12.model, newdata=DV.12.test)
  .errors.test<-R2(DV.12.test$DV.12,pred.test.DV.12)
  errors.test <- c(errors.test, .errors.test) 
  
}
r2<- c(r2, (errors.test), (errors.train))
sd<- c(sd, sd(errors.test), sd(errors.train))


###############################VIZUAL######################################################

dim(r2.sd.results)
length(sd)
r2.sd.results$r2<- r2
#r2.sd.results$sd<- sd

#View(r2.sd.results)
#write.csv(r2.sd.results, "o.f.250.r2.sd")

sum.table<- r2.sd.results[,.(Mean=mean(r2)), by=.(Type,Set)]

