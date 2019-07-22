rm(list = ls())

# installation of the package from CRAN
# install.packages("gdm")
# install.packages("devtools")
library(devtools)
library(tidyverse)
library(gdm)

# reads the input data
library(vegan)
library(tidyverse)
library(gdm)
All_Forests <- read.csv("Data/All_data.csv")

sum(is.na(All_Forests$Species_names))

All_Forests <- All_Forests %>%
  filter(!is.na(Species_names))



sppTab <- All_Forests[, c("Plot_id", "Species_names")] %>% 
  group_by(Plot_id,Species_names) %>% 
  summarise(Tree=n()) %>% 
  spread(Species_names,Tree, fill=0)

SappTab <- as.matrix(sppTab)

# To keep just the same row/ unique funciton is used

envTab <- All_Forests[, c("Plot_id","Mean_temperature", "Mean_precipation","Lat", "Long")] %>% unique()

envTab.dup <- envTab[!duplicated(envTab$Plot_id),]

envTab <- envTab.dup


gdmTab <- formatsitepair(sppTab, bioFormat = 1, XColumn = "Long", YColumn = "Lat", sppColumn = "Species_names", siteColumn = "Plot_id",
                         predData = envTab)
gdmTab[1:3]

# Dealing with biases associated with presence-only data
# weight by site richness

gdmTab.rw <- formatsitepair(sppTab, bioFormat = 1, XColumn = "Long", YColumn = "Lat", sppColumn = "Species_names", siteColumn = "Plot_id", 
                            predData = envTab, weightType = "richness")
gdmTab.rw$weights[1:5]


# remove sites with < 2 species records

gdmTab.sf <- formatsitepair(sppTab, bioFormat=1, XColumn="Long", YColumn="Lat", sppColumn="Species_names", siteColumn="Plot_id", predData=envTab, sppFilter=2)

#  GDM analysis

gdm.1 <- gdm(gdmTab, geo=T)

# The summary function provides an overview of the model, including deviance explained and the values of the coeﬃcients for the I-spline for each predictor variable. Variables with all coeﬃcients=0 have no relationship with the biological pattern. A shorter summary can be obtained using str.

summary(gdm.1) 
str(gdm.1)

# gdm Plot
length(gdm.1$predictors)


plot(gdm.1, plot.layout=c(3,3))

gdm.1.splineDat <- isplineExtract(gdm.1) 
str(gdm.1.splineDat)


plot(gdm.1.splineDat$x[,"Geographic"], gdm.1.splineDat$y[,"Geographic"], lwd=3, type="l", xlab="Geographic distance", ylab="Partial ecological distance")


# Predicting biological distances between sites 


gdm.1.pred <- predict(gdm.1, gdmTab) 
head(gdm.1.pred)


plot(gdmTab$distance, gdm.1.pred, xlab="Observed dissimilarity", ylab="Predicted dissimilarity",
     xlim=c(0,1), ylim=c(0,1), pch=20, col=rgb(0,0,1,0.5), lines(c(-1,2), c(-1,2)))























# Transforming from environmental space to biological space
 
# reordering environmental data to create table for transformation 
envTrans <- envTab[, c(5,4,2,3)] # same order as gdmTab envTrans[1:3,]

tabTrans <- gdm.transform(gdm.1, envTrans) # now scaled to biological importance 
tabTrans[1:3,]












##site-species table with coordinates
coordsO <- unique(sppTabO[, 2:ncol(sppTabO)])
testData1bO <- merge(testData1aO, coordsO, by="Plot_id")

##site-species, table-table
exFormat1aO <- formatsitepair(testData1aO, 1, siteColumn="Plot_id", XColumn="Easterning", YColumn="Northing",
                             predData=envTabO)

##site xy spp list, table-table
exFormat2a <- formatsitepair(sppTabO, 2, XColumn="Easterning", YColumn="Northing", sppColumn="Binomial",
                             siteColumn="Plot_id", predData=envTabO)


##dissim matrix model
site <- unique(sppTabO$Plot_id)
gdmDissim <- cbind(site, gdmDissim)
exFormat3 <- formatsitepair(gdmDissimO, 3, XColumn="L", YColumn="Lat", predData=envTab,
                            siteColumn="site")

# Biological distance matrix example

site <- unique(sppTabO$Plot_id) 
gdmDissim <- cbind(site, gdmDissim)

gdmTab.dis <- formatsitepair(gdmDissim, bioFormat=2, XColumn="Easterning", YColumn="Northing", predData=envTab, siteColumn="Plot_id")

gdm.1 <- gdm(gdmTab, geo=T)

# get idea of number of panels
length(gdm.1$predictors)

plot(gdm.1, plot.layout=c(4,3))

# To allow easy customization of I-spline plots, the isplineExtract function will extract the plotted values for
#each I-spline.

gdm.1.splineDat <- isplineExtract(gdm.1) 
str(gdm.1.splineDat)

plot(gdm.1.splineDat$x[,"Geographic"], gdm.1.splineDat$y[,"Geographic"], lwd=3, type="l", xlab="Geographic distance", ylab="Partial ecological distance")

gdm.1.pred <- predict(gdm.1, gdmTab) 
head(gdm.1.pred)

plot(gdmTab$distance, gdm.1.pred, xlab="Observed dissimilarity",
     ylab="Predicted dissimilarity", xlim=c(0,1), ylim=c(0,1), pch=20, col=rgb(0,0,1,0.5))+ lines(c(-1,2), c(-1,2))

# reordering environmental data to create table for transformation 
envTrans <- envTab[, c(13,12,2:11)] # same order as gdmTab
envTrans[1:3,]

tabTrans <- gdm.transform(gdm.1, envTrans) # now scaled to biological importance 
tabTrans[1:3,]

##sets up site-pair table
load(system.file("./data/gdm.RData", package="gdm"))
sppData <- gdmExpData[, c(1,2,14,13)]
envTab <- gdmExpData[, c(2:ncol(gdmExpData))]
sitePairTab <- formatsitepair(sppData, 2, XColumn="Long", YColumn="Lat", sppColumn="species", 
                              siteColumn="site", predData=envTab)

##create GDM
gdmMod <- gdm(sitePairTab, geo=TRUE)

##summary of GDM
summary(gdmMod)

# Beta------------------------------------------------------------------------------

library(betapart)
## distance matrix
mad.spp.dist <-beta.pair(myabundance.matrix, index.family="sorensen")

SIM <- mad.spp.dist$beta.sim
SNE <- mad.spp.dist$beta.sne
SOR <- mad.spp.dist$beta.sor


# store computed indices in a new data frame called 'indices'---------------------------------------
indices <- mydata[,1:13]
indices$Richness <- specnumber(myabundance.matrix) ## Species richness (S), rowSums(BCI > 0) does the same...
indices$Shannon <- diversity(myabundance.matrix) # shannon is default
indices$Evenness <- indices$Shannon/log(indices$Richness) #Pielou's evenness J
indices$Rarefied <- c(rarefy(myabundance.matrix[1:179,], sample=15))
