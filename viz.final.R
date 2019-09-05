library(corrplot)
library(extrafont)
library(ggplot2)
library(data.table)
library(ggridges)
library(tidyverse)
library(hrbrthemes)
library(viridis)



#### plot the results of the 10 mypersonality DV:
##data:

results.10.dv<- fread("ten.dv.results1.csv", header = T)
head(results.10.dv)
results.10.dv$nchar<- nchar(results.10.dv$DV)

plot10 <- ggplot(data=results.10.dv, 
                aes(x= stringr::str_wrap(DV, 20), nchar, y=Explained_Variance, 
                    group=Type ,
                    color=Type,
                    shape=Type)) + 
  geom_point( size=3) +
              labs(colour = "Predictors")+
  labs(shape = "Predictors")+labs(group = "Predictors")+
  scale_x_discrete("Outcome Variable\n\n",position = "left") +
  scale_y_continuous("\nExplained Variance",labels = scales::percent) +
  coord_flip()+ theme_bw()+ 
  theme(text = element_text(size = 18,family = "Arial Narrow"))+
  scale_color_manual(
    values = c("seagreen", "tan2", "tomato3"))+
   theme(axis.text.y=element_text(colour="black"))+
  theme(axis.text.x=element_text(colour="black"))+
  theme(legend.position="bottom")
  

plot10  

####plot the results of the 12 new DVs:

#Data:
results.12.dv<- fread("12.dv.results.csv", header = T)
names(results.12.dv)
results.12.dv$nchar<- nchar(results.12.dv$DV)

plot.12<- ggplot(data=results.12.dv, 
                aes(x= stringr::str_wrap(DV, 34), nchar, y=R2, 
                group=Set,
                color=Set,
                shape=Set)) + 
  geom_point(size = 2) +
  scale_x_discrete("Outcome Variable\n", position = "left" ) +
  scale_y_continuous("Explained Variance",labels = scales::percent) +
  coord_flip()+
  theme_linedraw()+ theme(legend.position="bottom")+
  scale_color_manual(values = c("orange", "turquoise4"))+
  facet_grid(.~factor(Type, levels = 
                        c("Predictive-5", "Big-5", "100Q")))+
theme(axis.title.x = element_text(margin = margin(t = 30, r = 50, b = 10, l = 50)))+
  theme(axis.title.y = element_text(margin = margin(t = 30, r = 50, b = 10, l = 50)))

plot.12

####plot the results of the 12 new DVs NO 100Q
#### only test of Big5 and new5:

#Data:
new.big.5.12.dv<- fread("12.test.pred5.big5.csv", header = T)
names(new.big.5.12.dv)
new.big.5.12.dv$nchar<- nchar(new.big.5.12.dv$DV)

plot.12.only5<- ggplot(data=new.big.5.12.dv, 
                 aes(x= stringr::str_wrap(DV, 34), nchar, y=R2, 
                     group=Type ,
                     color=Type,
                     shape=Type)) + 
  geom_point(size = 3.5) +
  scale_x_discrete("Outcome Variable\n", position = "left" ) +
  scale_y_continuous("Explained Variance",labels = scales::percent) +
  coord_flip()+
  theme_linedraw()+ theme(legend.position="bottom")+
  scale_color_manual(values = c("orange", "turquoise4"))+
  theme(axis.title.x = element_text(margin = margin(t = 30, r = 50, b = 10, l = 50)))+
  theme(axis.title.y = element_text(margin = margin(t = 30, r = 50, b = 10, l = 50)))

plot.12.only5

### create a table with the big5 and the new factor:

q100.all.na<- as.matrix(all.NA[,q1:q100])
rotation.mat <- as.matrix(rrpack.model$coef.ls %*% rrpack.model$A)

new5.all.na<-q100.all.na %*% as.matrix(rotation.mat) 
all.factors<- cbind(new5.all.na, all.NA[,E:C])
names(all.factors)<- c("Predictive Factor 1", "Predictive Factor 2", 
                       "Predictive Factor 3","Predictive Factor 4",
                       "Predictive Factor 5","Extraversion","Agreeableness",
                       "Openness To Experience","Neuroticism",
                       "Conscientiousness")
cor(all.factors$"Predictive-1",all.factors$"Predictive-2")

names(all.factors)
t.cor<- round(cor(all.factors),1)
t.cor<- cor(all.factors, method ="pearson")

col <- colorRampPalette(c("#004D3A", "#007464", "#009C8E" ,"#60C1B6" ,"#ADE0D9" ,"#F1F1F1", "#EBD2BC" ,"#D0AA83" ,"#AD7F41", "#835100" ,"#562400"))
 corrplot(t.cor, method="color", col=col(200),  
         type="lower", order="original",
         addCoef.col = "black", # Add coefficient of correlation
         tl.col="black", tl.srt=45, #Text label color and rotation
         tl.cex=0.7,number.cex=0.4,cl.cex=0.4,
         # hide correlation coefficient on the principal diagonal
         diag=T 
)

################################################################
 results.all_factors<- fread("all.new.factors.cv.csv", header = T)

 good_one<- ggplot(results.all_factors, aes(x=as.factor(factor), 
                                            y=Explained_Variance, group=variable)) +
   geom_line(aes(col=variable),size=0.5)+
   scale_x_discrete()+ 
   scale_y_continuous("\nExplained Variance",labels = scales::percent) +
   theme(text = element_text(size = 18,family = "Arial Narrow"))+
   theme_bw()+ scale_color_brewer(palette="Paired")+
   labs(x="\nNumber of Predictive Factors", y= "Explained Variance\n")
 
 good_one
###################################################################
 
 results.all_factors<- fread("all.new.factors.cv.csv", header = T)
only.tot<-results.all_factors[results.all_factors$variable=='Total']
only.tot<-only.tot[,c("factor","Explained_Variance")]
 
tot.only.plot<- ggplot(only.tot, aes(x=as.factor(factor), 
                                            y=Explained_Variance)) +
  geom_step()+ 
  scale_x_discrete()+ 
   scale_y_continuous("\nExplained Variance",labels = scales::percent) +
   theme(text = element_text(size = 18, family = "Arial Narrow"))+
   theme_bw()+
   labs(x="\nNumber of Predictive Factors", y= "Explained Variance\n")
tot.only.plot


##################################################################
results.all_factors<- fread("all.new.factors.cv.csv", header = T)
no.tot<-results.all_factors[results.all_factors$variable!='Total']
dim(no.tot)

no_tot.plot<- ggplot(no.tot, aes(x=as.factor(factor), 
                                           y=Explained_Variance, group=variable)) +
  geom_step(aes(col=variable),size=0.5)+
  scale_x_discrete()+ 
  scale_y_continuous("\nExplained Variance",labels = scales::percent) +
  theme(text = element_text(size = 18,family = "Arial Narrow"))+
  theme_bw()+ scale_color_brewer(palette="Paired")+
  labs(x="\nNumber of Predictive Factors", y= "Explained Variance\n")
 
no_tot.plot

##################################OVERFITTING#######################
try1<-fread("try.new.figure.csv", header = T)
str(try1)
try1$`100q`<-as.numeric(try1$`100q`)
try1$p.5<-as.numeric(try1$p.5)
names(try1)<- c("dv","set","p.5","q100")
try<- ggplot(try1, aes(x=p.5, y=q100, color=dv,
                       shape=set)) +
  geom_point(size=03)+
  scale_x_continuous()+ 
  scale_y_continuous("\nAll 100Q") +
  theme(text = element_text(size = 18,family = "Arial Narrow"))+
  labs(x="The Predictive-5", y= "All 100 Q\n")

try

#########################################################################
#####Histograms plots:

ten.dv.no.cv<- fread("10.dv.no.cv.csv", header = T)
ten.dv.no.cv.100<- ten.dv.no.cv[type=='q100']
ten.dv.no.cv.5<- ten.dv.no.cv[type=='Predictive-5']

p1 <- hist(ten.dv.no.cv.5[set=='train']$r2,xlim = range(0,0.6,0.05), nclass = 10)                     # centered at 4
p2 <- hist(ten.dv.no.cv.5[set=='test']$r2, xlim = range(0,0.6,0.05),nclass = 10)                     # centered at 6
plot( p1, col=rgb(0,0,1,1/4), xlim=range(0,0.5,0.05))  # first histogram
plot( p2, col=rgb(1,0,0,1/4), xlim=range(0,0.5,0.05), add=T)  # second


p1 <- hist(ten.dv.no.cv.100[set=='train']$r2,xlim = range(0,0.6,0.05), nclass = 10)                     # centered at 4
p2 <- hist(ten.dv.no.cv.100[set=='test']$r2, xlim = range(0,0.6,0.05),nclass = 10)                     # centered at 6
plot( p1, col=rgb(0,0,1,1/4), xlim=range(-0.4,0.6,0.05))  # first histogram
plot( p2, col=rgb(1,0,0,1/4), xlim=range(-0.4,0.6,0.05), add=T)  # second

ten.no.cv<- ggplot(ten.dv.no.cv.100, aes(x=dv, y=r2, fill=set))+
       geom_bar(stat="identity", position="dodge")


######################OVERFITTING########################################################

ggplot( r2.sd.results,aes(y=Type, x=r2,  fill=Set)) +
  geom_density_ridges(alpha=0.5, bandwidth=0.06) +
  scale_fill_viridis(discrete=T) +
  scale_color_viridis(discrete=TRUE) +
  theme(
    legend.position="left",
    panel.spacing = unit(1, "lines"),
    strip.text.x = element_text(size = 16,family = "Arial Narrow" )
  ) +
  xlim(0,0.7)+
  xlab("\nExplained Variance") +
  ylab("")+
  theme_bw()+
  theme(
    axis.title=element_text(size="14",color="black", family = "Arial Narrow"),
    axis.text=element_text(size="12",color="black",family = "Arial Narrow"))

