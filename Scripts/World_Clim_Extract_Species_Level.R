rm(list = ls())

library(sp)
library(rgdal)
library(raster)
library(tidyverse)

bio1 <- raster("Data/Worldclim_Data/wc2.0_bio_30s_01.tif")
bio12 <- raster("Data/WorldClim_Data/wc2.0_bio_30s_12.tif")
bio13 <- raster("Data/WorldClim_Data/wc2.0_bio_30s_13.tif")
bio14 <- raster("Data/WorldClim_Data/wc2.0_bio_30s_14.tif")

# Stack the rasters-----
world.stk <- stack(bio1,bio12,bio13, bio14)

# Import species locations----
Data <- read.csv("Data/All_data_Final.csv", header =  TRUE)

coordinates(Data) <- c("Long", "Lat")

Data.clim.trees <- raster::extract(world.stk, Data, method="bilinear", df=TRUE)

Final_Dataframe_trees <- cbind.data.frame(Data,Data.clim.trees)

Final_Dataframe_trees <- Final_Dataframe_trees %>% dplyr::select(-ID)

class(Final_Dataframe_trees)

write.table(Final_Dataframe_trees, file = "Final_Dataframe_trees.csv", append = FALSE, quote = FALSE, sep = ",",
            eol = "\n", na = "NA", dec = ".", row.names = FALSE,
            col.names = TRUE, qmethod = c("escape", "double"),
            fileEncoding = "") 

