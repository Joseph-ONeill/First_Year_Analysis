library(tidyverse)
library(ggplot2)
mydat<-read.csv(file.choose())

# choose the file name 19PSP_locations_carbon.csv
#add colums for location and land tenure

mydat$Area
# mydat$Region<-factor(unlist(lapply(strsplit(as.character(mydat$Area) , "_") , getElement ,1)))
mydat$Region<-factor(as.character(mydat$Area))
mydat$Tenure<-factor(unlist(lapply(strsplit(as.character(mydat$Area) , "_") , getElement ,2)))

#compare between regions
lm.1<-lm(Carbon~Region , mydat)
lm.1
anova(lm.1)
boxplot(Carbon~Region , mydat)

ggplot(mydat, aes(x= Region, y=Carbon, fill = Region)) + 
  geom_boxplot() + 
  xlab("Region") +
  ylab("Carbon (t/ha)")+
  theme_bw()+
  theme(text = element_text(size=15), axis.text.x = element_blank())

anov_analysis<- aov(Carbon~Region , mydat)
summary(anov_analysis)

TukeyHSD(aov(Carbon~Region , mydat))

#compare between tenures
lm.2<-lm(Carbon~Tenure , mydat)
lm.2
Anova_tenure <- anova(lm.2)
summary(Anova_tenure)
boxplot(Carbon~Tenure , mydat  , names=c("Reserved" , "Public"))
#clear significant difference between tenures in carbon sequestration p = 0.001929
summary
par(mfrow=c(2,2))
plot(lm.2)
#assumptions of homogenity of variance not well met, some lack of confidence in overall result
# more data collection or more sophisticated analysis of heterogenity of variance is required

# how to omitt the MMDF? How to black the label.


# Choose file name of All_data.csv

forests<-read.csv(file.choose())
forest_dat<-aggregate(C_Tree~Forest_Type+Plot_id , data=forests , sum)
lm.3<-lm(log(C_Tree)~Forest_Type , forest_dat)
anova(lm.3)
TukeyHSD(aov(log(C_Tree)~Forest_Type , forest_dat))

x11()
par(mfrow=c(1,1) , mar=c(12,4,4,1))
boxplot(log(C_Tree)~Forest_Type , data=forest_dat , las=2)

ggplot(forest_dat, aes(x= Forest_Type, y=log(C_Tree), fill = Forest_Type)) + 
  geom_boxplot() + 
  xlab("Forest Type") +
  ylab("Log Carbon (t/ha)")+
  theme_bw()+
  theme(text = element_text(size=15), axis.text.x = element_blank())


ggplot(forest_dat, aes(x=Forest_Type, y=log(C_Tree))) + geom_boxplot() +
  theme(text = element_text(size=20),
        axis.text.x = element_text(angle=90, hjust=1)) 
#vjust adjust the vertical justification of the labels, which is often useful

x11()
par(mfrow=c(2,2))
plot(lm.3)


