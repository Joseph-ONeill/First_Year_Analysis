rm(list = ls())
library(tidyverse) 
library(lubridate)
library(BIOMASS)
library(ape)

summarise = dplyr::summarise


# Carbon assessment for the Permanent Sample Plots (PSP)------

The_PSP <- read_csv("Data/PSP.csv")

# Taxo <- correctTaxo(genus=The_PSP$Genus, species = The_PSP$Species)
# 
# The_PSP$genusCorr <- Taxo$genusCorrected
# 
# The_PSP$speciesCorr <- Taxo$speciesCorrected
# 
# APG <- getTaxonomy(The_PSP$genusCorr, findOrder =T)
# 
# The_PSP$familyAPG <- APG$family
# 
# The_PSP$orderAPG <- APG$order
# 
# dataWD <- getWoodDensity(genus=The_PSP$genusCorr,
#                          species=The_PSP$speciesCorr,
#                          stand=The_PSP$Subplot_area)
# 
# sum(dataWD$levelWD == "species")
# 
# sum(dataWD$levelWD == "genus")
# 
# sum(!dataWD$levelWD%in%c("genus","species"))
# 
# The_PSP <- The_PSP %>% 
#   mutate(subplot_id = paste(Location, Compartment, Plot, sep="_"))
# 
# The_PSP$WD=dataWD$meanWD
# The_PSP <- data.frame(The_PSP)
# The_PSP$genusCorr=unlist(The_PSP$genusCorr)
# 
# View(The_PSP)
# sapply(The_PSP,class)
# 
# The_PSP <- The_PSP[!is.na(The_PSP$Dbh_cm),]
# The_PSP <- The_PSP %>% mutate(AGB=computeAGB(D=Dbh_cm,WD=WD,H=H_m)) %>% mutate(C_Tree=AGB*0.47)

# KBR--------------------------------------------------------------------------------------------------------------
Kabaung_Reserved_Forest <- The_PSP %>%
  filter(Location == "Kabaung_Reserved_Forest")

KBR_without_small <- Kabaung_Reserved_Forest %>% filter(Subplot_area != 0.015625)
KBR_med <- KBR_without_small%>% filter(Subplot_area == 0.0625)
KBR_large <- KBR_without_small %>% filter(Subplot_area == 0.25)


KBR_med_C <- KBR_med %>%
  group_by(Location, Plot) %>%
  dplyr::summarise(C_Tree_total = sum(C_Tree)) %>%
  dplyr::summarise(mean_C_Tree=mean(C_Tree_total)) %>%
  mutate(C_ha=mean_C_Tree/0.0625)

KBR_large_C <- KBR_large %>%
  group_by(Location, Plot) %>%
  dplyr::summarise(C_Tree_total = sum(C_Tree)) %>%
  dplyr::summarise(mean_C_Tree=mean(C_Tree_total)) %>%
  mutate(C_ha=mean_C_Tree/0.25)

KBR_C_combined <- rbind(KBR_med_C,KBR_large_C)

KBR_C <- t(data.frame(c("Kabaung_Reserved_Forest", sum(KBR_large_C$C_ha,KBR_med_C$C_ha))))
colnames(KBR_C) = c('Location', 'C_Ha')
rownames(KBR_C) = 1


# KMSR-----------------------------------------------------------------------------------------

Kyaukmasin_Reserve<- The_PSP %>%
  filter(Location == "Kyaukmasin_Reserved_Forest")

KMSR_without_small <- Kyaukmasin_Reserve %>% filter(Subplot_area != 0.015625)
KMSR_med <- KMSR_without_small%>% filter(Subplot_area == 0.0625)
KMSR_large <- KMSR_without_small %>% filter(Subplot_area == 0.25)


KMSR_med_C <- KMSR_med %>%
  group_by(Location, Plot) %>%
  dplyr::summarise(C_Tree_total = sum(C_Tree)) %>%
  dplyr::summarise(mean_C_Tree=mean(C_Tree_total)) %>%
  mutate(C_ha=mean_C_Tree/0.0625)

KMSR_large_C <- KMSR_large %>%
  group_by(Location, Plot) %>%
  dplyr::summarise(C_Tree_total = sum(C_Tree)) %>%
  dplyr::summarise(mean_C_Tree=mean(C_Tree_total)) %>%
  mutate(C_ha=mean_C_Tree/0.25)

KMSR_C_combined <- rbind(KMSR_med_C,KMSR_large_C)

KMSR_C <- t(data.frame(c("Kyaukmasin_Reserved_Forest", sum(KMSR_large_C$C_ha,KMSR_med_C$C_ha))))
colnames(KMSR_C) = c('Location', 'C_Ha')
rownames(KMSR_C) = 1

# YOMA-----------------------------------------------------------------

Yoma_Unclassified <- The_PSP %>%
  filter(Location == "Yoma_Unclassfied_Forest")

YOMA_without_small <- Yoma_Unclassified %>% filter(Subplot_area != 0.015625)
YOMA_med <- YOMA_without_small%>% filter(Subplot_area == 0.0625)
YOMA_large <- YOMA_without_small %>% filter(Subplot_area == 0.25)


YOMA_med_C <- YOMA_med %>%
  group_by(Location, Plot) %>%
  dplyr::summarise(C_Tree_total = sum(C_Tree)) %>%
  dplyr::summarise(mean_C_Tree=mean(C_Tree_total)) %>%
  mutate(C_ha=mean_C_Tree/0.0625)

YOMA_large_C <- YOMA_large %>%
  group_by(Location, Plot) %>%
  dplyr::summarise(C_Tree_total = sum(C_Tree)) %>%
  dplyr::summarise(mean_C_Tree=mean(C_Tree_total)) %>%
  mutate(C_ha=mean_C_Tree/0.25)

YOMA_C_combined <- rbind(YOMA_med_C,YOMA_large_C)

YOMA_C <- t(data.frame(c("Yoma_Unclassfied_Forest", sum(YOMA_large_C$C_ha,YOMA_med_C$C_ha))))
colnames(YOMA_C) = c('Location', 'C_Ha')
rownames(YOMA_C) = 1

# Three_Locations_C--------------------------------------------------------

The_19_PSP_C <- rbind(KBR_C, KMSR_C, YOMA_C)

rownames(The_19_PSP_C) <- 1:nrow(The_19_PSP_C)

The_19_PSP_C = data.frame(The_19_PSP_C)
The_19_PSP_C$C_Ha <- as.numeric(The_19_PSP_C$C_Ha )

write.table(The_19_PSP_C, file = "Data/The_19_PSP_CM.csv", append = FALSE, quote = FALSE, sep = ",",
            eol = "\n", na = "NA", dec = ".", row.names = FALSE,
            col.names = TRUE, qmethod = c("escape", "double"),
            fileEncoding = "")


# Relationship------------------------------------------------
# Diameter distribution in three areas

The_PSP %>%
  na.omit %>%
  ggplot(aes(y = Dbh_cm, x = Location)) +
  geom_point(position = "jitter", color = "green", size = 3)+
  ggtitle("Diameter Distribution")+
  xlab("Locations")+
  ylab("Diameter in Centimeter")+
  theme_bw()

# Height distribution in three areas
The_PSP %>%
  na.omit %>%
  ggplot(aes(y = H_m, x = Location)) +
  geom_point(position = "jitter", color = "green", size = 3)+
  ggtitle("Tree Height Distribution")+
  xlab("Locations")+
  ylab("Height in meter")+
  theme_bw()

# Height and diameter relationship
The_PSP %>%
  na.omit %>%
  ggplot(aes(y = H_m, x = Dbh_cm)) +
  geom_point(position = "jitter", color = "green", size = 3)+
  ggtitle("Height and diameter relationship")+
  xlab("Diameter at breath Height (DBH)")+
  ylab("Height in meter")+
  theme_bw()+ stat_smooth()



# write.table(The_PSP, file = "Data/PSP_adjustment.csv", append = FALSE, quote = FALSE, sep = ",",
#             eol = "\n", na = "NA", dec = ".", row.names = FALSE,
#             col.names = TRUE, qmethod = c("escape", "double"),
#             fileEncoding = "")

# Statistical_Analysis------------------------------------------------------------------

boxplot(C_Tree~Location , data=The_PSP)
boxplot(log(C_Tree)~Location , data=The_PSP)
lm(log(C_Tree)~Location , data=The_PSP)

anova(lm(log(C_Tree)~Location , data=The_PSP))

# pair-wise Test
TukeyHSD(aov(log(C_Tree)~Location , data=The_PSP))


