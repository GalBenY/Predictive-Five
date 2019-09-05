#The original data for the X's only users:

#library needed:

library(data.table)
library(magrittr)

#Data:

Q_big5_20_100 <- fread("big5_domains_item_level.csv", header = T)
big5_100Q <- Q_big5_20_100[qlength=="100",]
cols100 <- paste('q',1:100, sep='')
big5.100Q<- subset(big5_100Q, 
                   select =c('userid',cols100, "E","A","O","N","C")) 

big5.100Q<- na.omit(unique(big5.100Q, by='userid'))

dim(big5.100Q)


#==============================================================#
#New function:

R2 <- function(y, y.hat){
  numerator <- sum((y-y.hat)^2) 
  denominator <- sum((y-mean(y))^2)
  1-(numerator/denominator)
}
