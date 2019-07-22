rm(list = ls())
library(tidyverse) 
library(lubridate)
library(BIOMASS)
library(ape)

summarise = dplyr::summarise


# Carbon assessment for the Permanent Sample Plots (PSP)------

# Forests <- read_csv("Data/All_data_Final.csv")
Forests <- read_csv("Data/Final_Dataframe.csv")

# DIP--------------------------------------------------------------------------------------------------------------
Dipterocarpus_Forest <- Forests %>%
  filter(Forest_type == "Dipterocarpus_Forest")


Dipterocarpus_Forest_Carbon <- Dipterocarpus_Forest %>%
  group_by(Forest_type, Plot_id) %>%
  dplyr::summarise(C_Tree_total = sum(C_Tree)) %>%
  dplyr::summarise(mean_C_Tree=mean(C_Tree_total)) %>%
  mutate(C_ha=mean_C_Tree/0.04)


# DHL-------------------------------------------------------------------------------------------

Dry_Hill_Forest <- Forests %>%
  filter(Forest_type == "Dry_Hill_Forest")

Dry_Hill_Forest_Carbon <- Dry_Hill_Forest %>%
  group_by(Forest_type, Plot_id) %>%
  dplyr::summarise(C_Tree_total = sum(C_Tree)) %>%
  dplyr::summarise(mean_C_Tree=mean(C_Tree_total)) %>%
  mutate(C_ha=mean_C_Tree/0.04)

# Dry_Forest-------------------------------------------------------------------------------------

Dry_Forest <- Forests %>%
  filter(Forest_type == "Dry_Forest")

Dry_Forest_Carbon <- Dry_Forest %>%
  group_by(Forest_type, Plot_id) %>%
  dplyr::summarise(C_Tree_total = sum(C_Tree)) %>%
  dplyr::summarise(mean_C_Tree=mean(C_Tree_total)) %>%
  mutate(C_ha=mean_C_Tree/0.04)


# Dry_Mixed_Deciduous_Forest
Dry_Mixed_Deciduous_Forest <- Forests %>%
  filter(Forest_type == "Dry_Mixed_Deciduous_Forest")

Dry_Mixed_Deciduous_Carbon <- Dry_Mixed_Deciduous_Forest %>%
  group_by(Forest_type, Plot_id) %>%
  dplyr::summarise(C_Tree_total = sum(C_Tree)) %>%
  dplyr::summarise(mean_C_Tree=mean(C_Tree_total)) %>%
  mutate(C_ha=mean_C_Tree/0.04)

# if the sample size is assumed as 0.25 
Moist_Mixed_Deciduous_Forest <- Forests %>%
  filter(Forest_type == "Moist_Mixed_Deciduous_Forest")


anov_analysis<- aov(C_ha~Tenure , Moist_Mixed_Deciduous_Forest)
summary(anov_analysis)


  filter(Tenure=="Unclassifed_Forest")
mean(Moist_Mixed_Deciduous_Forest$Dbh_cm,na.rm = T)



mean(Moist_Mixed_Deciduous_Forest$Dbh_cm,na.rm = T)


Moist_Mixed_Deciduous_Carbon <- Moist_Mixed_Deciduous_Forest %>%
  group_by(Forest_type, Plot_id) %>%
  dplyr::summarise(C_Tree_total = sum(C_Tree)) %>% na.omit() %>% 
  dplyr::summarise(mean_C_Tree=mean(C_Tree_total)) %>%
  mutate(C_ha=mean_C_Tree/0.25)


# Three_Locations_C--------------------------------------------------------

All_forests_Carbon <- rbind(Dipterocarpus_Forest_Carbon,Dry_Forest_Carbon,Dry_Hill_Forest_Carbon, Dry_Mixed_Deciduous_Carbon, Moist_Mixed_Deciduous_Carbon)

rownames(All_forests_Carbon) <- 1:nrow(All_forests_Carbon)

All_forests_Carbon = data.frame(All_forests_Carbon)

All_forests_Carbon$C_Ha <- as.numeric(All_forests_Carbon$C_Ha )

write.table(All_forests_Carbon, file = "Data/All_forests_Carbon.csv", append = FALSE, quote = FALSE, sep = ",",
            eol = "\n", na = "NA", dec = ".", row.names = FALSE,
            col.names = TRUE, qmethod = c("escape", "double"),
            fileEncoding = "")


# Statistical_Analysis------------------------------------------------------------------

boxplot(C_Tree~Forest_Type , data=Forests)
boxplot(log(C_Tree)~Forest_Type , data=Forests)
lm(log(C_Tree)~Forest_Type , data=Forests)

anova(lm(log(C_Tree)~Forest_Type , data=Forests))
TukeyHSD(lm(log(C_Tree)~Forest_Type , data=Forests))

# # To produce the final data frame
# 
# Combined_data <- read_csv("Data/Forests_All_info.csv")
# 
# Four_Forests <- Combined_data %>%
#   filter(!Forest_type=="Moist_Mixed_Deciduous_Forest") %>% 
#   group_by(Forest_type, Plot_id, Mean_temperature, Mean_precipitation, precipitation_of_wettest_month, pecipiation_of_dryiest_month, Tenure) %>% 
#   summarise(C_Tree_total = sum(C_Tree)) %>% 
#   mutate(C_ha=C_Tree_total/0.04)
# 
# 
# 
#   MMDF <- Combined_data %>%
#   filter(Forest_type=="Moist_Mixed_Deciduous_Forest") %>% 
#   group_by(Forest_type, Plot_id,Mean_temperature, Mean_precipitation, precipitation_of_wettest_month, pecipiation_of_dryiest_month, Tenure) %>%
#   summarise(C_Tree_total = sum(C_Tree)) %>% 
#   mutate(C_ha=C_Tree_total/0.25)
#   
#   Combine_dataframe <- rbind(Four_Forests,MMDF)
#   
# To add shannon and simpson-----
  
All_Forests <- read.csv("Data/All_data_Final.csv")
  
  sum(is.na(All_Forests$Species_names))
  Abundance <- All_Forests %>%
    filter(!is.na(Species_names))%>%
    group_by(Plot_id, Species_names) %>%
    dplyr::summarise(Tree = n()) %>%
    tidyr::spread(key=Species_names, value = Tree, fill=0)
  
  
  
  Site_species <- Abundance[,c(2:nrow(Abundance))]
  
  # All plots without the GPS locations are neglected in the Beta-diversity analysis and GDM analysis
  
  # Shannon index
  H <- diversity(Site_species, index = "shannon")
  mean(H)
  Shannon_index <- data.frame(Abundance$Plot_id,H)
  colnames(Shannon_index[]) <- "Plot_id"
  plot(Shannon_index)
  class(Shannon_index)
  arrange(Shannon_index,H)
  # simpson index
  SI <- diversity(Site_species, index = "simpson")
  mean(SI)
  
  Simpson_index <- data.frame(SI)
  # Final_Data_Frame <- All_Forests %>%
  #   group_by(Forest_type, Plot_id, Mean_Precipitation, Mean_Temperature, Tenure, Precipiation_of_dryiest_month, Precipiation_of_dryiest_month)
  #   summarise(C_Tree_total = sum(C_Tree)) %>% 
  #   mutate(C_ha=C_Tree_total/0.04)

  
  Final <- All_Forests %>% group_by(Plot_id, Forest_type,Tenure) %>%
    summarise(C_Tree_total = sum(C_Tree, na.rm = TRUE)) %>% 
    mutate(Scale_factor = ifelse(Forest_type=="Moist_Mixed_Deciduous_Forest", 0.25, 0.04)) %>% 
    mutate(C_ha = C_Tree_total/Scale_factor) 
    
  
  Final$Shannon_index <- Shannon_index$H
  
  Final$Simpson_index <- Simpson_index$SI
  
  
  write.table(Final, file = "Data/Final.csv", append = FALSE, quote = FALSE, sep = ",",
              eol = "\n", na = "NA", dec = ".", row.names = FALSE,
              col.names = TRUE, qmethod = c("escape", "double"),
              fileEncoding = "") 
  



  # Statistical_Analysis------------------------------------------------------------------
  
  Forests <- read_csv("Data/Final_Dataframe.csv")
  boxplot(C_ha~Forest_type , data=Forests)
  
  lm(log(C_ha)~Forest_type , data=Forests)
  
  anova(lm(log(C_ha)~Forest_type , data=Forests))
  
  TukeyHSD(lm(log(C_Tree)~Forest_type , data=Forests))
  
  anova(lm(log(C_ha)~Pieolou_evenness , data=Forests))
  
  
  anova(lm(Shannon_index~Forest_type, data = Forests))
 
  
  TukeyHSD(aov(Shannon_index~Forest_type, data=Forests))
  
  anova(lm(Simpson_index~Forest_type, data = Forests))
  
  TukeyHSD(aov(Simpson_index~Forest_type , data=Forests))
  
  
  anova(lm(Pieolou_evenness~Forest_type, data = Forests))
  
  TukeyHSD(aov(Pieolou_evenness~Forest_type , data=Forests))
  
 summary(lm(Shannon_index~Mean_precipitation, data = Forests))
 
 summary(lm(Simpson_index~Mean_precipitation, data = Forests))  
 
 
  


# Forest v Carbon plot---------------------------
  ggplot(Forests, aes(x= Forest_type, y = log(C_ha), fill = Forest_type)) + 
    geom_boxplot() +
    xlab("Forest Type") +
    ylab("Log transformed carbon (tC/ha)")+
   theme_bw()+
   theme(text = element_text(size=15), axis.text.x = element_blank(),
         axis.title.x = element_text(margin = margin(t = 18, r = 0, b = 0, l = 0)),
         axis.title.y = element_text(margin = margin(t = 0, r = 18, b = 0, l = 0)))
 
  


  Carbon_ha %>% ggplot(aes(x= area, y = log(Carbon), fill = area)) + geom_boxplot()
  
# Forest v Species index (Shannon)----------------------------
  

  ggplot(Forests, aes(x= Forest_type, y = log(Shannon_index), fill = Forest_type)) + 
    geom_boxplot() +
    xlab("Forest Type") +
    ylab("Log Shannon-Weiner Index")+
    theme_bw()+
    theme(text = element_text(size=15), axis.text.x = element_blank(),
          axis.title.x = element_text(margin = margin(t = 18, r = 0, b = 0, l = 0)),
          axis.title.y = element_text(margin = margin(t = 0, r = 18, b = 0, l = 0)))
  
  
 # Simpson
  
  ggplot(Forests, aes(x= Forest_type, y = Simpson_index, fill = Forest_type)) + 
    geom_boxplot() +
    xlab("Forest Type") +
    ylab("Log Simpson Index")+
    theme_bw()+
    theme(text = element_text(size=15), axis.text.x = element_blank(),
          axis.title.x = element_text(margin = margin(t = 18, r = 0, b = 0, l = 0)),
          axis.title.y = element_text(margin = margin(t = 0, r = 18, b = 0, l = 0)))
# eveness
  
  ggplot(Forests, aes(x= Forest_type, y = Pieolou_evenness, fill = Forest_type)) + 
    geom_boxplot() +
    xlab("Forest Type") +
    ylab("Pieolou_evenness")+
    theme_bw()+
    theme(text = element_text(size=15), axis.text.x = element_blank(),
          axis.title.x = element_text(margin = margin(t = 18, r = 0, b = 0, l = 0)),
          axis.title.y = element_text(margin = margin(t = 0, r = 18, b = 0, l = 0)))
  
  
  
  
  
  
  # For the PSP--------------------------------
  
  
  
  
  
Heights <- read.csv("Data/PSP_data_Final.csv")


Kabaung_Reserve_Forest <- Heights %>%
  filter(Location == "Kabaung_Reserve_Forest")

KBR_Height <- Kabaung_Reserve_Forest %>%
  mutate(Mean_Height = mean(H_m, na.rm = TRUE))



Yoma_Unclassfied_Forest <- Heights %>%
  filter(Location == "Yoma_Unclassfied_Forest")

YoMa_Height <- Yoma_Unclassfied_Forest %>%
  mutate(Mean_Height = mean(H_m, na.rm = TRUE))



Kyaukmasin_Reserve_Forest <- Heights %>%
  filter(Location == "Kyaukmasin_Reserve_Forest")

KMSR_Height <- Kyaukmasin_Reserve_Forest %>%
  mutate(Mean_Height = mean(H_m, na.rm = TRUE))

# 
# 
# 
# # Relationship------------------------------------------------
# # Diameter distribution in three areas
# 
# The_PSP %>%
#   na.omit %>%
#   ggplot(aes(y = Dbh_cm, x = Location)) +
#   geom_point(position = "jitter", color = "green", size = 3)+
#   ggtitle("Diameter Distribution")+
#   xlab("Locations")+
#   ylab("Diameter in Centimeter")+
#   theme_bw()
# 
# # Height distribution in three areas
# The_PSP %>%
#   na.omit %>%
#   ggplot(aes(y = H_m, x = Location)) +
#   geom_point(position = "jitter", color = "green", size = 3)+
#   ggtitle("Tree Height Distribution")+
#   xlab("Locations")+
#   ylab("Height in meter")+
#   theme_bw()
# 
# # Height and diameter relationship
# The_PSP %>%
#   na.omit %>%
#   ggplot(aes(y = H_m, x = Dbh_cm)) +
#   geom_point(position = "jitter", color = "green", size = 3)+
#   ggtitle("Height and diameter relationship")+
#   xlab("Diameter at breath Height (DBH)")+
#   ylab("Height in meter")+
#   theme_bw()+ stat_smooth()
# 
# 
# 
# # write.table(The_PSP, file = "Data/PSP_adjustment.csv", append = FALSE, quote = FALSE, sep = ",",
# #             eol = "\n", na = "NA", dec = ".", row.names = FALSE,
# #             col.names = TRUE, qmethod = c("escape", "double"),
# #             fileEncoding = "")
# 
# # Statistical_Analysis------------------------------------------------------------------
# 
# boxplot(C_Tree~Location , data=The_PSP)
# boxplot(log(C_Tree)~Location , data=The_PSP)
# lm(log(C_Tree)~Location , data=The_PSP)
# 
# anova(lm(log(C_Tree)~Location , data=The_PSP))
# 
# # pair-wise Test
# TukeyHSD(aov(log(C_Tree)~Location , data=The_PSP))
# 
