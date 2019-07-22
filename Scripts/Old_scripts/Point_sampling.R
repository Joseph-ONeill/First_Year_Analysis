# load required libraries

rm(list = ls())

# install.packages("sp")
# install.packages("rgdal")
# install.packages("raster")
# install.packages("tidyverse")
library(sp)
library(rgdal)
library(raster)
library(tidyverse)


# load the raster grids e.g. bio1 = annual mean temperature, bio2 = annual mean precipitation
bio1 <- raster("Data/Worldclim_Data/wc2.0_bio_30s_01.tif")
bio12 <- raster("Data/Worldclim_Data/wc2.0_bio_30s_12.tif")
# etc. etc.

# stack the rasters
world.stk <- stack(bio1, bio12)

# import species locations
Points_of_forests <- read.csv("Data/Combined_Data-csv.csv", header =  TRUE)

# remove rows with na coordinates
pof <- Points_of_forests %>%
  filter(!is.na(E))

# assign coordinates
coordinates(pof) <- c("E", "N")

# define CRS _ Coordinate Reference System. Creating a new spatial point subject with 

pof_utm <- SpatialPoints(pof, proj4string=CRS("+proj=utm +zone=46Q +datum=WGS84"))

# transform CRS UTM 46Q to WGS84                        
pof_wgs <- spTransform(pof_utm, CRS("+proj=longlat +datum=WGS84"))

# As extract funcion is using tidyverse...it is not right. So, have to unload package dplyr so that "Extract function will use Raster package

.rs.unloadPackage("tidyr")

# extract raster values from stack as data frame.
pts.clim <- extract(world.stk, pof_wgs, method="bilinear", df=TRUE)

# write to data frame
pof.clim <- data.frame(cbind(coordinates(pof_wgs),pts.clim,pof@data))

# write a csv
write.csv(pof.clim, "Data/pof_extract.csv", row.names = FALSE)

Forests_world_clim <- read.csv("Data/pof_extract.csv")

names(Forests_world_clim)

Forests_WC <- Forests_world_clim %>% 
  rename(Mean_temperature = wc2.0_bio_30s_01, Mean_precipation = wc2.0_bio_30s_12 )

# wirte a final file

library(tidyverse)
names(Forests_WC)

Forests_WCdata <- select(Forests_WC,Plot_id, Forest_Type,Binomial,Genus,Species,Family,Dbh_cm, H_m,N, E, Mean_temperature, Mean_precipation)


write.csv(Forests_WCdata, file = "Data/Forests_WCdata.csv",row.names = FALSE)

Main_data <- read.csv("Data/Forests_WCdata.csv")
