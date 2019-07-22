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
# bio2 <- raster("Data/WorldClim_Data/wc2.0_bio_30s_02.tif")
# bio3 <- raster("Data/WorldClim_Data/wc2.0_bio_30s_03.tif")
# bio4 <- raster("Data/WorldClim_Data/wc2.0_bio_30s_04.tif")
# bio5 <- raster("Data/WorldClim_Data/wc2.0_bio_30s_05.tif")
# bio6 <- raster("Data/WorldClim_Data/wc2.0_bio_30s_06.tif")
# bio7 <- raster("Data/WorldClim_Data/wc2.0_bio_30s_07.tif")
# bio8 <- raster("Data/WorldClim_Data/wc2.0_bio_30s_08.tif")
# bio9 <- raster("Data/WorldClim_Data/wc2.0_bio_30s_09.tif")
# bio10 <- raster("Data/WorldClim_Data/wc2.0_bio_30s_10.tif")
# bio11 <- raster("Data/WorldClim_Data/wc2.0_bio_30s_11.tif")
bio12 <- raster("Data/WorldClim_Data/wc2.0_bio_30s_12.tif")
bio13 <- raster("Data/WorldClim_Data/wc2.0_bio_30s_13.tif")
bio14 <- raster("Data/WorldClim_Data/wc2.0_bio_30s_14.tif")
# bio15 <- raster("Data/WorldClim_Data/wc2.0_bio_30s_15.tif")
# bio16 <- raster("Data/WorldClim_Data/wc2.0_bio_30s_16.tif")
# bio17 <- raster("Data/Worldclim_Data/wc2.0_bio_30s_17.tif")
# bio18 <- raster("Data/WorldClim_Data/wc2.0_bio_30s_18.tif")
# bio19 <- raster("Data/Worldclim_Data/wc2.0_bio_30s_19.tif")

# etc. etc.

# Stack the rasters-----
world.stk <- stack(bio1,bio12,bio13, bio14)
# world.stk <- stack(bio1,bio2)

# Import species locations----
Data <- read.csv("Data/All_data_Final.csv", header =  TRUE)

colnames(Data)[1] <- "Forest_type"
# Remove rows with Na coordinates and find out 47Q----

plot_sp<-Data %>%
  group_by(Plot_id) %>% 
  summarise(Lat = mean(Lat), Long = mean(Long))


ggplot(plot_sp, aes(x = Long, y = Lat)) +
  geom_point()

coordinates(plot_sp) <- c("Long", "Lat")
plot(plot_sp)

# pof_47Q <- Data %>%
#   filter((Forest_type=="Moist_Mixed_Deciduous_Forest")) %>% 
#   filter(Plot_id %in% c(116:119))%>% na.omit()

Data.clim <- raster::extract(world.stk, plot_sp, method="bilinear", df=TRUE)
hist(Data.clim$wc2.0_bio_30s_14)

Data <- read.csv("Data/Final.csv", header =  TRUE)

Final_Dataframe <- cbind.data.frame(Data,Data.clim)

Final_Dataframe <- Final_Dataframe %>% dplyr::select(-ID)

class(Final_Dataframe)

write.table(Final_Dataframe, file = "Final_Dataframe.csv", append = FALSE, quote = FALSE, sep = ",",
            eol = "\n", na = "NA", dec = ".", row.names = FALSE,
            col.names = TRUE, qmethod = c("escape", "double"),
            fileEncoding = "") 

write.csv(Final_Dataframe,file = "Final_Dataframe.csv")

# pof_46Q <- Data %>% 
#   filter(!(Forest_type=="Moist_Mixed_Deciduous_Forest"& Plot_id %in% c(116:119))) %>% na.omit()
# 
# 
# # Assign coordinates------
# coordinates(pof_46Q) <- c("Easting", "Northing")
# coordinates(pof_47Q) <- c("Easting", "Northing")
# 
# # Define CRS _ Coordinate Reference System. Creating a new spatial point subject with 47Q and 46 Q------
# 
# pof_46Q_utm <- SpatialPoints(pof_46Q, proj4string=CRS("+proj=utm +zone=46Q +datum=WGS84"))
# pof_47Q_utm <- SpatialPoints(pof_47Q, proj4string=CRS("+proj=utm +zone=47Q +datum=WGS84"))
# 
# # Transform CRS UTM 46Q and 47Q to WGS84------                        
# pof_wgs_46Q <- spTransform(pof_46Q_utm, CRS("+proj=longlat + datum=WGS84"))
# pof_wgs_47Q <- spTransform(pof_47Q_utm, CRS("+proj=longlat + datum=WGS84"))
# 
# # As extract funcion is using tidyverse...it is not right. So, have to unload package dplyr so that "Extract function will use Raster package
# 
# .rs.unloadPackage("tidyr")
# 
# # Extract raster values from stack as data frame.-------
# pts.clim_46Q <- raster::extract(world.stk, pof_wgs_46Q, method="bilinear", df=TRUE)
# pts.clim_47Q <- raster::extract(world.stk, pof_wgs_47Q, method="bilinear", df=TRUE)
# 
# # Write to data frame-------
# pof.clim.46Q <- data.frame(cbind(coordinates(pof_wgs_46Q),pof_46Q@data,pts.clim_46Q))
# pof.clim.47Q <- data.frame(cbind(coordinates(pof_wgs_47Q),pof_47Q@data,pts.clim_47Q))
# 
# # Write a csv------
# write.csv(pof.clim.46Q, "Data/pof_extract_46Q.csv", row.names = FALSE)
# write.csv(pof.clim.47Q, "Data/pof_extract_47Q.csv", row.names = FALSE)
# 
# 
# # Combining 46Q and 47 Q together into one data.frame------
# 
# Forests_WCdata <- rbind.data.frame(read.csv("Data/pof_extract_46Q.csv"),
#                                    read.csv("Data/pof_extract_47Q.csv"))
# 
# Forests_All_info <- Forests_WCdata%>% 
#   rename(Mean_temperature = wc2.0_bio_30s_01, Mean_precipitation = wc2.0_bio_30s_12, precipitation_of_wettest_month = wc2.0_bio_30s_13, pecipiation_of_dryiest_month = wc2.0_bio_30s_14)
# 
# 
# write.csv(Forests_All_info, "Data/Forests_All_info.csv", row.names = FALSE)
# 
# # Plotting------
# ggplot(data = Forests_All_info, mapping = aes(x = Forest_type, y = Mean_temperature)) +
#   geom_boxplot(alpha = 0) +
#   geom_jitter(alpha = 0.3, color = "tomato")+
#   theme_bw() 
# 
# 
# Forests_All_info$Forest_type <- as.factor(Forests_All_info$Forest_type)
# cbPalette=c("forestgreen","blue", "darkred", "red", "tomato")
# ggplot(Forests_All_info, aes(x = Forest_type, y = Mean_precipitation)) +
#   geom_boxplot(aes(fill = Forest_type, color = Forest_type), alpha = 0.5)+
#   scale_fill_manual(values=cbPalette)+ # Boxplot fill color
#   scale_color_manual(values = cbPalette)+# Jitter color palette
#   ylab("Mean annual precipitation (mm)") +
#   xlab("Forest Type")+
#   scale_y_continuous(breaks = c(0,200,400,600,800,1000, 1200, 1400))+ 
#   ylim(1,1500)+
#   theme_bw() + 
#   theme(text = element_text(size=15), axis.text.x = element_blank(),
#         axis.title.x = element_text(margin = margin(t = 18, r = 0, b = 0, l = 0)),
#         axis.title.y = element_text(margin = margin(t = 0, r = 18, b = 0, l = 0)))
# 
# 
# ggplot(Forests_All_info, aes(x = Forest_type, y = Mean_temperature)) +
#   geom_boxplot(aes(fill = Forest_type, color = Forest_type), alpha = 0.5)+
#   scale_fill_manual(values=cbPalette)+ # Boxplot fill color
#   scale_color_manual(values = cbPalette)+# Jitter color palette
#   ylab(expression(paste('Annual Mean Temperature (',~degree,'C)',sep=''))) +
#   xlab("Forest Type")+
#   scale_y_continuous(breaks = c(0,5,10,20,30,40))+ 
#   ylim(1,50)+
#   theme_bw() + 
#   theme(text = element_text(size=15), axis.text.x = element_blank(),
#         axis.title.x = element_text(margin = margin(t = 18, r = 0, b = 0, l = 0)),
#         axis.title.y = element_text(margin = margin(t = 0, r = 18, b = 0, l = 0)))
# 
# 
# 
# ggplot(Forests_All_info, aes(x = Forest_type, y = precipitation_of_wettest_month)) +
#   geom_boxplot(aes(fill = Forest_type, color = Forest_type), alpha = 0.5)+
#   scale_fill_manual(values=cbPalette)+ # Boxplot fill color
#   scale_color_manual(values = cbPalette)+# Jitter color palette
#   ylab("Precipitation of Wettest Month (mm)") +
#   xlab("Forest Type")+
#   scale_y_continuous(breaks = c(0,50,100,150, 200, 250, 300))+ 
#   ylim(1, 300)+
#   theme_bw() + 
#   theme(text = element_text(size=15), axis.text.x = element_blank(),
#         axis.title.x = element_text(margin = margin(t = 18, r = 0, b = 0, l = 0)),
#         axis.title.y = element_text(margin = margin(t = 0, r = 18, b = 0, l = 0)))
# 
# 
# 
# 
# ggplot(Forests_All_info, aes(x = Forest_type, y = pecipiation_of_dryiest_month)) +
#   geom_boxplot(aes(fill = Forest_type, color = Forest_type), alpha = 0.5)+
#   scale_fill_manual(values=cbPalette)+ # Boxplot fill color
#   scale_color_manual(values = cbPalette)+# Jitter color palette
#   ylab("Precipitation of Driest Month (mm)") +
#   xlab("Forest Type")+
#   scale_y_continuous(breaks = c(0,2,4,6,8,10))+ 
#   ylim(1, 10)+
#   theme_bw() + 
#   theme(text = element_text(size=15), axis.text.x = element_blank(),
#         axis.title.x = element_text(margin = margin(t = 18, r = 0, b = 0, l = 0)),
#         axis.title.y = element_text(margin = margin(t = 0, r = 18, b = 0, l = 0)))
# 
# 
# 
