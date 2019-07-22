mydat<-read.csv(file.choose())

#add colums for location and land tenure

mydat$Area
mydat$Region<-factor(unlist(lapply(strsplit(as.character(mydat$Area) , "_") , getElement ,1)))
mydat$Tenure<-factor(unlist(lapply(strsplit(as.character(mydat$Area) , "_") , getElement ,2)))

#compare between regions
lm.1<-lm(Carbon~Region , mydat)
lm.1
anova(lm.1)
boxplot(Carbon~Region , mydat)
TukeyHSD(aov(Carbon~Region , mydat))

#compare between tenures
lm.2<-lm(Carbon~Tenure , mydat)
lm.2
anova(lm.2)
boxplot(Carbon~Tenure , mydat  , names=c("Reserved" , "Public"))
#clear significant difference between tenures in carbon sequestration p = 0.001929

par(mfrow=c(2,2))
plot(lm.2)
#assumptions of homogenity of variance not well met, some lack of confidence in overall result
# more data collection or more sophisticated analysis of heterogenity of variance is required




forests<-read.csv(file.choose())
forest_dat<-aggregate(C_Tree~Forest_Type+Plot_id , data=forests , sum)
lm.3<-lm(log(C_Tree)~Forest_Type , forest_dat)
anova(lm.3)
TukeyHSD(aov(log(C_Tree)~Forest_Type , forest_dat))

x11()
par(mfrow=c(1,1) , mar=c(12,4,4,1))
boxplot(log(C_Tree)~Forest_Type , data=forest_dat , las=2)

x11()
par(mfrow=c(2,2))
plot(lm.3)


