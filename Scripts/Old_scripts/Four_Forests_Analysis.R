rm(list = ls())

library(tidyverse) 
library(lubridate)
library(BIOMASS)
# library(plyr)
library(ape)
library(vegan)

summarise = dplyr::summarise


# Combined Forests Analysis---------------------------------------------------------------------------------------

Forests <- read.csv("Data/All_data.csv")

# Dipterocarpus_Forest----------------------------------------------------------------------------------------------
Dipterocarpus_Forest <- Forests %>%
  filter(Forest_Type == "Dipterocarpus_Forest")

DIPF <- Dipterocarpus_Forest %>%
  group_by(Forest_Type, Plot_id) %>%
  dplyr::summarise(C_Tree_total = sum(C_Tree)) %>%
  dplyr::summarise(mean_C_Tree=mean(C_Tree_total)) %>%
  mutate(C_ha=mean_C_Tree/0.04)


# Dry_Forest---------------------------------------------------------------------------------------
Dry_Forest <- Forests %>%
  filter(Forest_Type == "Dry_Forest")

DF <- Dry_Forest %>%
  group_by(Forest_Type,Plot_id) %>%
  dplyr::summarise(C_Tree_total = sum(C_Tree)) %>%
  dplyr::summarise(mean_C_Tree=mean(C_Tree_total)) %>%
  mutate(C_ha=mean_C_Tree/0.04)


# Dry_Hill_Forest-----------------------------------------------------------------------------------
Dry_Hill_Forest <- Forests %>%
  filter(Forest_Type == "Dry_Hill_Forest")

DHF <- Dry_Hill_Forest %>%
  group_by(Forest_Type,Plot_id) %>%
  dplyr::summarise(C_Tree_total = sum(C_Tree)) %>%
  dplyr::summarise(mean_C_Tree=mean(C_Tree_total)) %>%
  mutate(C_ha=mean_C_Tree/0.04)

# Dry_Mixed_Deciduous_Forest------------------------------------------------------------------------
Dry_Mixed_Deciduous_Forest <- Forests %>%
  filter(Forest_Type == "Dry_Mixed_Deciduous_Forest")

DMDF <- Dry_Mixed_Deciduous_Forest %>%
  group_by(Forest_Type,Plot_id) %>%
  dplyr::summarise(C_Tree_total = sum(C_Tree)) %>%
  dplyr::summarise(mean_C_Tree=mean(C_Tree_total)) %>%
  mutate(C_ha=mean_C_Tree/0.04)



# Statistical_Analysis------------------------------------------------------------------

boxplot(C_Tree~Forest_Type , data=Forests)
boxplot(log(C_Tree)~Forest_Type , data=Forests)
lm(log(C_Tree)~Forest_Type , data=Forests)

anova(lm(log(C_Tree)~Forest_Type , data=Forests))
TukeyHSD(lm(log(C_Tree)~Forest_Type , data=Forests))
