
#Data:

demog <- fread("demog.csv", header = T)

network<- demog[,c("userid", "network_size")]
network<- na.omit(network)
dim(network)

network$scale<- scale(sqrt(network$network_size))

network.2<- network[scale>-1,]
dim(network.2)

plot(table(network.2$network_size), log='x')

final.network<- network.2[,c("userid", "network_size")]

# Combine with the big5:
network.big5<- merge(big5.100Q, final.network, by="userid")

#take only part of the set:

set.seed(1)
size_set <- 142000
set_ind <- sample(seq_len(nrow(network.big5)),
                  size = size_set)
network.big5.small <- data.table(network.big5[set_ind, ])

#Take for the test set:

set.seed(1)
size_test <- 2000
test_ind <- sample(seq_len(nrow(network.big5.small)),
                   size = size_test)
test.network <- data.table(network.big5.small[test_ind, ])
dim(test.network)

train.network<- data.table(network.big5.small[-test_ind, ])
dim(train.network)


# Regression model to predict on the all NA data:
network.train.100<- train.network[,-c("userid", "O","C","E","A","N")]


network.model<- lm(network_size~.,network.train.100)

coef(network.model)
length( coef(network.model))
summary(network.model)
