
##table data, species and environmental
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

envTab <- All_Forests[, c("Plot_id","Mean_temperature", "Mean_precipation","Lat", "Long")] %>% unique()

envTab.dup <- envTab[!duplicated(envTab$Plot_id),]

##environmental raster data
##commented out to reduce example run time
rastFile <- system.file("./extdata/stackedVars.grd", package="gdm")
envRast <- stack(rastFile)

#########table type 1
##site-species table without coordinates
testData1a <- reshape2::dcast(sppTab, Plot_id~Species_names)

##site-species table with coordinates
coords <- unique(sppTab[, 2:ncol(sppTab)])
testData1b <- merge(testData1a, coords, by="Plot_id")

##site-species, table-table
exFormat1a <- formatsitepair(testData1a, 1, siteColumn="Plot_id", XColumn="Long", YColumn="Lat",
                             predData=envTab)

##site-species, table-raster
##not run
exFormat1b <- formatsitepair(testData1b, 1, siteColumn="Plot_id", XColumn="Long", YColumn="Lat",
predData=envRast)

#########table type 2
##site xy spp list, table-table
exFormat2a <- formatsitepair(sppTab, 2, XColumn="Long", YColumn="Lat", sppColumn="Species_names",
                             siteColumn="Plot_id", predData=envTab)

##site xy spp list, table-raster
##commented out to reduce example run time
exFormat2b <- formatsitepair(sppTab, 2, XColumn="Long", YColumn="Lat", sppColumn="Species_names",
siteColumn="Plot_id", predData=envRast)

#########table type 3
##dissim matrix model
site <- unique(sppTab$Plot_id)
gdmDissim <- cbind(Plot_id, gdmDissim)
exFormat3 <- formatsitepair(gdmDissim, 3, XColumn="Long", YColumn="Lat", predData=envTab,
                            siteColumn="Plot_id")

#########table type 4
##adds a predictor matrix to an existing site-pair table, in this case, predData needs to be
##filled, but is not actually used
exFormat4 <- formatsitepair(exFormat2a, 4, predData=envTab, siteColumn="Plot_id",
                            distPreds=list(as.matrix(gdmDissim)))