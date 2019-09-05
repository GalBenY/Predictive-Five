library(plyr)
library(dplyr)
library(rrpack)
library(data.table)

#Consolidation of all the data tables (10) together:

DTlist <- list(train.svq.openness, train.svq.trans , train.network,
              train.empathy, train.cesd, train.liberal,
              train.risky, train.nonhealthy, train.iq, train.swl)
DFlong <- join_all(dfs = DTlist, by="userid", type = "full") %>% data.table
dim(DFlong)

respondents <- big5.100Q$userid %in% DFlong$userid
all.NA <- unique(big5.100Q[!respondents])

dim(all.NA)
names(all.NA)


# all.NA has Xs and Ys for subjects with no predictors.

pred.data <- all.NA[,-c("userid", "O","C","E","A","N")]
dim(pred.data)


# Predictions of each of the dependent variables:

pred.iq<- predict(iq.model, newdata =pred.data)

pred.swl<- predict(swl.model, newdata =pred.data)

pred.cesd<- predict(cesd.model,newdata =pred.data)

pred.empathy<- predict(empathy.model, newdata =pred.data)

pred.nonhealthy<- predict(nonhealthy.model,newdata =pred.data)

pred.risky<- predict(risky.model,newdata =pred.data)

pred.liberal<- predict(liberal.model,newdata =pred.data)

pred.network<- predict(network.model,newdata =pred.data)

pred.sv.openness<- predict(openness.model,newdata =pred.data)

pred.sv.trans<- predict(trans.model ,newdata =pred.data)

y.train<- cbind(pred.iq, pred.swl, pred.cesd, pred.empathy, 
                pred.nonhealthy, pred.risky, pred.liberal, pred.network,
               pred.sv.openness, pred.sv.trans)

x.train<- pred.data

#scaling the y data:
y.train.scale<- scale(y.train,center=T, scale=F)
x.train.scale<- scale(x.train,center=T, scale=F)

#RRR with RRpack: ##########

rrpack.model<- rrr.fit(y.train.scale,
                       x.train.scale,
                       nrank = 5, 
                       coefSVD = T)

rotation.mat <- as.matrix(rrpack.model$coef.ls %*% rrpack.model$A)

####correletion of the new5 and pc5

q100.train<- as.matrix(all.NA[,q1:q100])
rot.1<- q100.train%*%rotation.mat
rot.2<- q100.train%*%rotation.mat.2
rot.1.2<- cbind(rot.1,rot.2)
cor.rot.1.2<- round(cor(rot.1.2),2)
View(cor.rot.1.2)

####correletion of the new5 and pc5

q100.train<- as.matrix(all.NA[,q1:q100])
pc.5.all.na<- q100.train%*%pc5
new.5.all.na<- q100.train%*%rotation.mat
pc.new.all.na<-as.data.table(cbind(pc.5.all.na,new.5.all.na))
names(pc.new.all.na)<- c("PC1","PC2","PC3","PC4","PC5","NEW1",
                         "NEW2","NEW3","NEW4","NEW5")
cor.all.na<- round(cor(pc.new.all.na),2)
View(cor.all.na)


### create a table with the big5 and the new factor:

q100.all.na<- as.matrix(all.NA[,q1:q100])
new5.all.na<-q100.all.na %*% as.matrix(rotation.mat) 
pc5.all.na<- q100.all.na %*% pc5
all.factors<- cbind(new5.all.na, all.NA[,E:C],pc5.all.na)
names(all.factors)<- c("New1", "New2", "New3", "New4", "New5",
                       "Extraversion","Agreeableness",
                       "Openness To Experience","Neuroticism",
                       "Conscientiousness", "PC1", "PC2", "PC3",
                       "PC4", "PC5")
names(all.factors)
dim(all.factors)
t.cor<- cor(all.factors)

write.csv(all.factors,"all.factors")

#####Descriptive statistics :

D.S<- as.data.table(merge(all.NA, demog))
D.S.age<- na.omit(D.S[, c("userid", "age")])

table(D.S$locale)
table(D.S$age)

############################################################################

# #==============================================================#
# R2 <- function(y, y.hat){
#   numerator <- (y-y.hat)^2 %>% sum
#   denominator <- (y-mean(y))^2 %>% sum
#   1-(numerator/denominator)
# }
# 
# R2.no_inter<- function(y, y.hat){
#   numerator <- (y-y.hat)^2 %>% sum
#   denominator <- (y^2) %>% sum
#   1-(numerator/denominator)
# }
