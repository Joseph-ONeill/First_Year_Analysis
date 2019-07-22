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
# Import the raster files-----

# load the raster grids e.g. bio1 = annual mean temperature, bio2 = annual mean precipitation. I can add more...
# BIO1 = Annual Mean Temperature
# BIO2 = Mean Diurnal Range (Mean of monthly (max temp - min temp))
# BIO3 = Isothermality (BIO2/BIO7) (* 100)
# BIO4 = Temperature Seasonality (standard deviation *100)
# BIO5 = Max Temperature of Warmest Month
# BIO6 = Min Temperature of Coldest Month
# BIO7 = Temperature Annual Range (BIO5-BIO6)
# BIO8 = Mean Temperature of Wettest Quarter
# BIO9 = Mean Temperature of Driest Quarter
# BIO10 = Mean Temperature of Warmest Quarter
# BIO11 = Mean Temperature of Coldest Quarter
# BIO12 = Annual Precipitation
# BIO13 = Precipitation of Wettest Month
# BIO14 = Precipitation of Driest Month
# BIO15 = Precipitation Seasonality (Coefficient of Variation)
# BIO16 = Precipitation of Wettest Quarter
# BIO17 = Precipitation of Driest Quarter
# BIO18 = Precipitation of Warmest Quarter
# BIO19 = Precipitation of Coldest Quarter

bio1 <- raster("Data/Worldclim_Data/wc2.0_bio_30s_01.tif")
bio12 <- raster("Data/Worldclim_Data/wc2.0_bio_30s_12.tif")
# etc. etc.

# Stack the rasters-----
world.stk <- stack(bio1, bio12)

# Import species locations----
Points_of_forests <- read.csv("Data/Combined_Data.csv", header =  TRUE)

# Remove rows with Na coordinates and find out 47Q----

pof_47Q <- Points_of_forests %>%
  filter((Forest_Type=="Moist_Mixed_Deciduous_Forest")) %>% 
  filter(Plot_id %in% c(11:14))%>% na.omit()

pof_46Q <- Points_of_forests %>% 
  filter(!(Forest_Type=="Moist_Mixed_Deciduous_Forest"& Plot_id %in% c(11:14))) %>% na.omit()


# Assign coordinates------
coordinates(pof_46Q) <- c("E", "N")
coordinates(pof_47Q) <- c("E", "N")

# Define CRS _ Coordinate Reference System. Creating a new spatial point subject with 47Q and 46 Q------

pof_46Q_utm <- SpatialPoints(pof_46Q, proj4string=CRS("+proj=utm +zone=46Q +datum=WGS84"))
pof_47Q_utm <- SpatialPoints(pof_47Q, proj4string=CRS("+proj=utm +zone=47Q +datum=WGS84"))

# Transform CRS UTM 46Q and 47Q to WGS84------                        
pof_wgs_46Q <- spTransform(pof_46Q_utm, CRS("+proj=longlat +datum=WGS84"))
pof_wgs_47Q <- spTransform(pof_47Q_utm, CRS("+proj=longlat +datum=WGS84"))

# As extract funcion is using tidyverse...it is not right. So, have to unload package dplyr so that "Extract function will use Raster package

.rs.unloadPackage("tidyr")

# Extract raster values from stack as data frame.-------
pts.clim_46Q <- extract(world.stk, pof_wgs_46Q, method="bilinear", df=TRUE)
pts.clim_47Q <- extract(world.stk, pof_wgs_47Q, method="bilinear", df=TRUE)

# Write to data frame-------
pof.clim.46Q <- data.frame(cbind(coordinates(pof_wgs_46Q),pts.clim_46Q,pof_46Q@data))
pof.clim.47Q <- data.frame(cbind(coordinates(pof_wgs_47Q),pts.clim_47Q,pof_47Q@data))

# Write a csv------
write.csv(pof.clim.46Q, "Data/pof_extract_46Q.csv", row.names = FALSE)
write.csv(pof.clim.47Q, "Data/pof_extract_47Q.csv", row.names = FALSE)


# Combining 46Q and 47 Q together into one data.frame------

Forests_WCdata <- rbind.data.frame(read.csv("./Data/pof_extract_46Q.csv"),
                                   read.csv("./Data/pof_extract_47Q.csv"))

Forests_WClim <- Forests_WCdata%>% 
  rename(Mean_temperature = wc2.0_bio_30s_01, Mean_precipitation = wc2.0_bio_30s_12 )

Forests_WCdata <- select(Forests_WClim,Plot_id, Forest_Type,Binomial,Genus,Species,Family,Dbh_cm, H_m,N, E, Mean_temperature, Mean_precipitation)

# Plotting------
ggplot(data = Forests_WCdata, mapping = aes(x = Forest_Type, y = Mean_temperature)) +
  geom_boxplot(alpha = 0) +
  geom_jitter(alpha = 0.3, color = "tomato")+
  theme_bw() 


ggplot(data = Forests_WCdata, mapping = aes(x = Forest_Type, y = Mean_precipitation)) +
  geom_boxplot(alpha = 0) +
  geom_jitter(alpha = 0.3, color = "tomato")+
  theme_bw() 

