# Aland PCA Example data-----


`Field.Data.(8)`-> inverts
rm(`Field.Data.(8)`)

envVars<-inverts[,1:23]
abundances<-inverts[,-c(1:23)]
abundances[is.na(abundances)]<-0

library(vegan)
envVars$shannon<-diversity(abundances,index="shannon")
envVars$simpson<-diversity(abundances,index="simpson")
envVars$richness<-rowSums(abundances>0)
t.test(shannon~Location,data=envVars)
sh.vel.lm<-lm(shannon~Flow.Velocity..cm.s.,data=envVars)

ginvert.pca<-prcomp(abundances)
invert.scores<-predict(invert.pca)
plot(invert.scores[,1],invert.scores[,2],col=envVars$Location)
plot(invert.pca)
t.test(invert.scores[,1]~envVars$Location)

abundances.BC<-vegdist(abundances)
as.matrix(abundances.BC)[17,]