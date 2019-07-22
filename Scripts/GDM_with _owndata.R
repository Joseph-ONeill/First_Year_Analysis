rm(list = ls())

# installation of the package from CRAN
# install.packages("gdm")
# install.packages("devtools")
library(devtools)
library(tidyverse)
library(gdm)
library(tidyr)
library(dplyr)

# reads the input data

# All_Forests <- read.csv("Data/Forests_All_info.csv")

All_Forests <- read.csv("Data/Final_Dataframe_trees.csv")


# sum(is.na(All_Forests$Species_names))
# sum(is.na(All_Forests$Mean_temperature))
# sum(is.na(All_Forests$Mean_precipitation))
# sum(is.na(All_Forests$precipitation_of_wettest_month))
# sum(is.na(All_Forests$pecipiation_of_dryiest_month))

All_Forests <- All_Forests %>%
  filter(!is.na(Species_names))



sppTab <- All_Forests[, c("Plot_id", "Species_names")] %>% 
  group_by(Plot_id,Species_names) %>% 
  dplyr::summarise(Tree=n()) %>% 
  spread(Species_names,Tree, fill=0)

SppTab <- as.matrix(sppTab)

# To keep just the same row/ unique funciton is used

envTab <- All_Forests[, c("Plot_id", "Mean_temperature", "Mean_precipitation", "precipitation_of_wettest_month", "pecipiation_of_dryiest_month","Lat", "Long")] %>% unique()

envTab <- All_Forests[, c("Plot_id", "Mean_temperature", "Mean_precipitation", "precipitation_of_wettest_month", "pecipiation_of_dryiest_month","Northing", "Easting")] %>% unique()


envTab.dup <- envTab[!duplicated(envTab$Plot_id),]

envTab <- envTab.dup


gdmTab <- formatsitepair(sppTab, bioFormat = 1, XColumn = "Easting", YColumn = "Northing", sppColumn = "Species_names", siteColumn = "Plot_id",
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


plot(gdm.1, plot.layout=c(2,2))

gdm.1.splineDat <- isplineExtract(gdm.1) 
str(gdm.1.splineDat)


plot(gdm.1.splineDat[[1]][,"Mean_temperature"], gdm.1.splineDat[[2]][,"Mean_temperature"], type="l", 
     lwd=3, xlab=xlab <- expression('Annual mean temperature ('*~degree*C*')'), ylab= expression("Importance of annual mean temperature gradient"))


plot(gdm.1.splineDat[[1]][,"Mean_precipitation"], gdm.1.splineDat[[2]][,"Mean_precipitation"], type="l", 
     lwd=3, xlab="Annual mean precipitation (mm)", ylab="Importance of annual mean precipitation")

plot(gdm.1.splineDat[[1]][,"precipitation_of_wettest_month"], gdm.1.splineDat[[2]][,"precipitation_of_wettest_month"], type="l", 
    lwd=3, xlab="precipitation_of_wettest_month (mm)", ylab="Partial precipitation_of_wettest_month (mm)")

plot(gdm.1.splineDat[[1]][,"precipitation_of_dryiest_month"], gdm.1.splineDat[[2]][,"precipitation_of_dryiest_month"], type="l", 
     lwd=3, xlab="precipitation_of_driest_month (mm)", ylab="Partial precipitation_of_driest_month (mm)")

plot(gdm.1.splineDat[[1]][,"precipitation_of_wettest_month"], gdm.1.splineDat[[2]][,"precipitation_of_wettest_month"], type="l", 
     lwd=3, xlim =xlab="precipitation_of_wettest_month (mm)", ylab="Partial precipitation_of_wettest_month (mm)")



plot(gdm.1.splineDat$x[,"Geographic"], gdm.1.splineDat$y[,"Geographic"], lwd=3, type="l", xlab="Geographic distance (m)", ylab="Importance of ecological distance (m)")+
    xlim(1,10000000)
  






# Predicting biological distances between sites 


gdm.1.pred <- predict(gdm.1, gdmTab) 
head(gdm.1.pred)


plot(gdmTab$distance, gdm.1.pred, xlab="Observed dissimilarity", ylab="Predicted dissimilarity",
     xlim=c(0,1), ylim=c(0,1), pch=20, col=rgb(0,0,1,0.5), lines(c(-1,2), c(-1,2)))





# # Transforming from environmental space to biological space
#  
# # reordering environmental data to create table for transformation 
# envTrans <- envTab[, c(2,3,4,5)] # same order as gdmTab envTrans[1:3,]
# 
# tabTrans <- gdm.transform(gdm.1, envTrans) # now scaled to biological importance 
# tabTrans[1:3,]

# Dipterocarpus_Forest prediction.-----------------------------------------------------------------------------------------------------

All_Forests <- read.csv("Data/Final_Dataframe_trees.csv")

All_Forests <- All_Forests %>%
  filter(!is.na(Species_names))

Dipterocarpus_Forest <- All_Forests %>%
  filter(Forest_type == "Dipterocarpus_Forest")

SppTabDIP <- Dipterocarpus_Forest %>%
  filter(!is.na(Species_names))%>%
  group_by(Plot_id, Species_names) %>%
  dplyr::summarise(Tree = n()) %>%
  spread(key=Species_names, value = Tree, fill=0)

SppTabDIP <- as.matrix(SppTabDIP)

# To keep just the same row/ unique funciton is used

envTabDIP <- Dipterocarpus_Forest[, c("Plot_id", "Mean_temperature", "Mean_precipitation", "precipitation_of_wettest_month", "pecipiation_of_dryiest_month","Lat", "Long")] %>% unique()

envTab.dup <- envTab[!duplicated(envTabDIP$Plot_id),]

envTabDIP <- envTab.dup


gdmTabDIP <- formatsitepair(SppTabDIP, bioFormat = 1, XColumn = "Long", YColumn = "Lat", sppColumn = "Species_names", siteColumn = "Plot_id",
                            predData = envTabDIP)
gdmTabDIP[1:3]

gdm.1.pred <- predict(gdm.1, gdmTabDIP) 

  