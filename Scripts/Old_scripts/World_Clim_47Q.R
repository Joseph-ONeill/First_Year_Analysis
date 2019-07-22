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

bio1 <- raster("Data/WorldClim_Data/wc2.0_bio_30s_01.tif")
bio12 <- raster("Data/WorldClim_Data/wc2.0_bio_30s_12.tif")
# etc. etc.

# Stack the rasters-----
world.stk <- stack(bio1, bio12)

# Import species locations----
POF47 <- read.csv("Data/47Q_points.csv", header =  TRUE)


# Assign coordinates------
coordinates(POF47) <- c("E", "N")


# Define CRS _ Coordinate Reference System. Creating a new spatial point subject with 47Q and 46 Q------

POF47Q_UTM <- SpatialPoints(POF47, proj4string=CRS("+proj=utm +zone=47Q +datum=WGS84"))

# Transform CRS UTM 46Q and 47Q to WGS84------                        
POF_WGS_47Q <- spTransform(POF47Q_UTM, CRS("+proj=longlat +datum=WGS84"))

# As extract funcion is using tidyverse...it is not right. So, have to unload package dplyr so that "Extract function will use Raster package

.rs.unloadPackage("tidyr")

# Extract raster values from stack as data frame.-------
PTS.Wclim_47Q <- extract(world.stk, POF_WGS_47Q, method="bilinear", df=TRUE)

# Write to data frame-------
POF.Wclim.47Q <- data.frame(cbind(coordinates(POF_WGS_47Q),PTS.Wclim_47Q, POF47@data))

POF.Wclim.47Q <- POF.Wclim.47Q %>% 
     rename(Mean_temperature = wc2.0_bio_30s_01, Mean_precipitation = wc2.0_bio_30s_12 )
# Write a csv------
write.csv(POF.Wclim.47Q, "Data/POF_47.csv", row.names = FALSE)


# # Combining 46Q and 47 Q together into one data.frame------
# 
# Forests_WCdata <- rbind.data.frame(read.csv("./Data/pof_extract_46Q.csv"),
#                                    read.csv("./Data/pof_extract_47Q.csv"))
# 
# Forests_WClim <- Forests_WCdata%>% 
#   rename(Mean_temperature = wc2.0_bio_30s_01, Mean_precipitation = wc2.0_bio_30s_12 )
# 
# Forests_WCdata <- select(Forests_WClim,Plot_id, Forest_Type,Binomial,Genus,Species,Family,Dbh_cm, H_m,N, E, Mean_temperature, Mean_precipitation)

# Plotting------
ggplot(data = POF.Wclim.47Q, mapping = aes(y = Forest_Type, x = Mean_temperture)) +
  geom_boxplot(alpha = 0) +
  geom_jitter(alpha = 0.3, color = "tomato")+
  theme_bw() 


ggplot(data = POF.Wclim.47Q, mapping = aes(y = Forest_Type, x = Mean_precipitation)) +
  geom_boxplot(alpha = 0) +
  geom_jitter(alpha = 0.3, color = "tomato")+
  theme_bw() 

