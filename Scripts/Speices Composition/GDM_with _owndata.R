rm(list = ls())

# installation of the package from CRAN
install.packages("gdm")
# installation from GitHub
install.packages("devtools")
library(devtools)
library(tidyverse)

library(gdm)
# reads in example input data
gdmData <- read.csv("Data/Main_data.csv")
gdmData[1:3,]

# Site_by_Species Dataframe

site_by_species <- gdmData %>%
  filter(!is.na(Binomial)) %>%
  group_by(Plot_id, Binomial) %>%
  summarise(Tree = n()) %>%
  spread(key=Binomial, value = Tree, fill=0)

# get columns with xy, site ID, and species data
sppTab <- gdmData[, c("Binomial", "Plot_id", "E", "N")]

# get columns with env. data and xy-coordinates
envTab <- gdmData[, c("Binomial", "Plot_id", "E", "N","Mean_temperature", "Mean_precipation")]


# x-y species list example
gdmTab <- formatsitepair(site_by_species, bioFormat=1, dist = "bray", abundance = FALSE,
                         XColumn="E", YColumn="N", sppColumn= "Binomial", 
                         siteColumn="Plot_id", predData=envTab)
gdmTab[1:3,]

# Biological distance matrix example

site <- unique(sppTabO$Plot_id) 
gdmDissim <- cbind(site, gdmDissim)

gdmTab.dis <- formatsitepair(gdmDissim, bioFormat=3, XColumn="Long", YColumn="Lat", predData=envTab, siteColumn="site")

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

