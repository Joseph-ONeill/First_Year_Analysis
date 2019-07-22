rm(list = ls())

library(plyr)
library(tidyverse)
library(vegan)
library(ape)
library(tidyverse)
library(lubridate)
library(BIOMASS)
library(vegan)

The_PSP <- read_csv("Data/19PSP.csv")

The_PSP<-The_PSP %>% mutate(land_tenure = ifelse(Location  == "Yoma_Unclassfied_Forest", "Unclassified", "Reserve")) 

# Plots_1 <- filter(The_PSP$Plot==1)
#                                                 
# class(The_PSP$Plot)
# 
# Average_tree_Number_PSP <- round((nrow(The_PSP)/19), digits = 0)
# 
# Tree_Number_PSP_small <- round((40*0.04/0.25), digits = 0)
# 
# PSP_Smallsize <- Average_tree_Number[sample(Average_tree_Number, Tree_PSP_small,replace = F)]

sf<-0.04/0.25 # scale factor

myboot_species<-matrix(NA, nrow = 19, ncol = 100)
for(i in 1:100){
  nspecies <- The_PSP %>% group_by(Plot) %>%
    filter(Subplot_area == "0.25") %>%
    filter(1:length(Species) %in% sample(1:length(Species), round(length(Species) *
                                                                    sf))) %>%
    summarise(nspecies = length(unique(Species)))
  myboot_species[,i]<-nspecies$nspecies
}
apply(myboot_species, 1, FUN = mean)


sfs<-0.04/0.0625 # scale factor small

myboot_species_small <-matrix(NA, nrow = 19, ncol = 100)
for(i in 1:100){
  nspecies_small <- The_PSP %>% group_by(Plot) %>%
    filter(Subplot_area == "0.0625") %>%
    filter(1:length(Species) %in% sample(1:length(Species), round(length(Species) *
                                                                    sfs))) %>%
    summarise(nspecies_small= length(unique(Species)))
  myboot_species_small[,i]<-nspecies$nspecies
}
apply(myboot_species_small, 1, FUN = mean)

