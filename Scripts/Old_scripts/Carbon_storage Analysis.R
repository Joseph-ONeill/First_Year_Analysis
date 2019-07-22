
# Forests---------------------------------------

rm(list = ls())

# install.packages("tidyverse")
# install.packages("lubridate")
# install.packages("BIOMASS")
# install.packages("vegan")
# install.packages("plyr")
# install.packages("ape")

library(tidyverse) 
library(lubridate)
library(BIOMASS)
# library(plyr)
library(ape)

summarise = dplyr::summarise


# The 19 PSP------------------------------------------------------------

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


# Diameter distribution in three areas---------
# The_PSP_area <- The_PSP %>% mutate(area = gsub("_[[:digit:]]*_[[:digit:]]*$", "", subplot_id))
# The_PSP %>%
#   na.omit %>%
#   ggplot(aes(y = Dbh_cm, x = Location)) +
#   geom_point(position = "jitter", color = "green", size = 3)+
#   ggtitle("Diameter Distribution")+
#   xlab("Region")+
#   ylab("DBH(cm)")+
#   theme_bw()


The_PSP$Location <- as.factor(The_PSP$Location)
cbPalette=c("forestgreen","blue", "darkred")
ggplot(The_PSP, aes(x = Location, y = Dbh_cm)) +
  geom_boxplot(aes(fill = Location), alpha = 0.5)+
  geom_jitter(aes(color = Location, width = 0.01))+
  scale_fill_manual(values=cbPalette)+ # Boxplot fill color
  scale_color_manual(values = cbPalette)+# Jitter color palette
  ylab("DBH (cm)") +
  xlab("Location")+
  scale_y_continuous(breaks = c(0,20,40,60,80,100))+ 
  ylim(1,100)+
  theme_bw()+
  theme(text = element_text(size=15), axis.text.x = element_blank())

# Height distribution in three areas
ggplot(The_PSP, aes(x = Location, y = H_m)) +
  geom_boxplot(aes(fill = Location), alpha = 0.5)+
  geom_jitter(aes(color = Location))+
  scale_fill_manual(values=cbPalette)+ # Boxplot fill color
  scale_color_manual(values = cbPalette)+# Jitter color palette
  ylab("Height(m)") +
  xlab("Location")+
  scale_y_continuous(breaks = c(0,10,20,30,40,50))+ 
  ylim(1,50)+
  theme_bw()+
  theme(text = element_text(size=15), axis.text.x = element_blank())


The_PSP %>%
  na.omit %>%
  ggplot(aes(y = H_m, x = Location)) +
  geom_point(position = "jitter", color = "green", size = 3)+
  ggtitle("Tree Height Distribution")+
  xlab("Region")+
  ylab("Height (m)")+
  theme_bw()+
  theme(text = element_text(size=15), axis.text.x = element_blank())

# Height and diameter relationship
The_PSP %>%
  na.omit %>%
  ggplot(aes(y = H_m, x = Dbh_cm)) +
  geom_point(position = "jitter", color = "green", size = 3)+
  xlab("Diameter at Breast Height-DBH (cm)")+
  ylab("Height-H (m)")+
  scale_x_continuous(breaks = c(0,20,40,60,80,100, 120))+
  theme_bw()+ geom_smooth(method = "lm")+
  theme(text = element_text(size=15),axis.text.x  = element_text(angle=0, hjust=2),
        axis.title.x = element_text(margin = margin(t = 20, r = 0, b = 0, l = 0)),
        axis.title.y = element_text(margin = margin(t = 0, r = 20, b = 0, l = 0)))

LmHD <- lm(The_PSP$H_m~The_PSP$Dbh_cm)
summary(LmHD)

# Estimate Biomass------------------------

The_PSP$WD=dataWD$meanWD

The_PSP_without_small <- The_PSP %>% filter(Subplot_area != 0.015625)
The_PSP_med <- The_PSP_without_small%>% filter(Subplot_area == 0.0625)
The_PSP_large <- The_PSP_without_small %>% filter(Subplot_area == 0.25)

AGBPlotList <- by(The_PSP_med, The_PSP_med$subplot_id,
                  function(x) computeAGB(D=x$Dbh_cm,WD=x$WD,H=x$H_m),
                  simplify=F)

AGBplot <-sapply(AGBPlotList,sum) 


AGBPlotList_med <-by(The_PSP_med, The_PSP_med$subplot_id,
                    function(x) computeAGB(D=x$Dbh_cm,WD=x$WD,H=x$H_m),
                    simplify=F)

AGBplot_med <- sapply(AGBPlotList_med,sum) 

AGBPlotList_large <- by(The_PSP_large, The_PSP_large$subplot_id,
                      function(x) computeAGB(D=x$Dbh_cm,WD=x$WD,H=x$H_m),
                      simplify=F)

AGBplot_large <- sapply(AGBPlotList_large,sum) 

AGBplot_med <- data.frame(plot = names(AGBplot_med), agb = AGBplot_med, row.names = NULL)
AGBplot_large <- data.frame(plot = names(AGBplot_large), agb = AGBplot_large, row.names = NULL)
AGBplot <- merge(AGBplot_med, AGBplot_large, by = "plot", all.y = TRUE)
AGBplot$agb.x[is.na(AGBplot$agb.x)] <- 0

# Change ha value--------------------------------------------

Carbon_ha <- mutate(AGBplot, agb_X_ha = agb.x*(1/0.0625), agb_y_ha = agb.y*(1/0.25)) %>%
  mutate(AGB_Total = agb_X_ha + agb_y_ha) %>%
  mutate(Carbon = AGB_Total*0.47) %>% 
  mutate(Area = gsub("_[[:digit:]]*_[[:digit:]]*$", "", plot))

Three_locations_Carbon <- Carbon_ha %>% select(Area, Carbon) %>% group_by(Area) %>% 
  dplyr::summarise(Carbon_area=mean(Carbon))

write.table(Carbon_ha,"Data/PSP_carbon.csv", quote = FALSE,sep = ",", row.names = FALSE)

Avg_C_PSP_ha <- data.frame(Area=Carbon_ha$area, Carbon=Carbon_ha$Carbon) %>% 
  summarise(Avg_C_ha=mean(Carbon))


ggplot(Carbon_ha, aes(x= area, y = Carbon)) + 
geom_boxplot() + ggtitle("Average Carbon(t/ha) Distribution")+
  xlab("Locations") +
  ylab("Carbon (t/ha)")

Carbon_ha %>% ggplot(aes(x= area, y = log(Carbon), fill = area)) + geom_boxplot()

# Combined Forests Analysis-------------------------------------------

Forests <- read.csv("Data/All_data.csv")
# colnames(Forests)
# head(Forests)
# str(Forests)
# 
# Taxo <- correctTaxo(genus=Forests$Genus,species = Forests$Species)
# 
# Forests$genusCorr <- Taxo$genusCorrected
# Forests$speciesCorr <- Taxo$speciesCorrected
# 
# APG <- getTaxonomy(Forests$genusCorr, findOrder =T)
# Forests$familyAPG<-APG$family
# Forests$orderAPG<-APG$order
# 
# 
# dataWD <- getWoodDensity(genus=Forests$genusCorr,
#                          species=Forests$speciesCorr,
#                          stand=Forests$Plot_id)
# 
# sum(dataWD$levelWD == "species")
# 
# sum(dataWD$levelWD == "genus")
# 
# sum(!dataWD$levelWD%in%c("genus","species"))
# 
# Forests$WD <- dataWD$meanWD 
# 
# View(Forests)
# 
# 
# # Develop HD model---------------------------------------------------------------------------------------
# 
# HDmodels <- modelHD(D=Forests$Dbh_cm, 
#                     H = Forests$H_m,
#                     drawGraph=TRUE,
#                     useWeight=TRUE)
# 
# Best_HDmodel<- modelHD(D=Forests$Dbh_cm,
#                        H=Forests$H_m,
#                        method="log3",
#                        useWeight =TRUE)
# 
# 
# # Getting missing tree Height-----------------------------------------------------------------------------
# 
# dataHlocal<-retrieveH(D=Forests$Dbh_cm[is.na(Forests$H_m)],
#                       model = Best_HDmodel)
# 
# Forests$H_m[is.na(Forests$H_m)] <- dataHlocal$H
# 
# View(Forests)
# 
# 
# # Estimate All Biomass-------------------------------------------------------------------------------------- 
# 
# Forests <- Forests[!is.na(Forests$Dbh_cm),]
# Forests$AGB <- computeAGB(D=Forests$Dbh_cm,
#                                        WD=Forests$WD,
#                                        H=Forests$H_m)
# 
# # Different Forests, Biomass and carbon (t/ha) -------------------------------------------------------------------
# # Carbon content default value (CC) 0.47 was used to evaluate the carbon content (IPCC, 2006). 
# # The ha convertion factor for the forests is 0.04 as they are 400 meter square plots

# Dipterocarpus_Forest------------------------------------------------------------------------------------
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

# Moist_Mixed_Deciduous_Forest-----------------------------------------------------------------------------
# There is another calculation for this forest type which takes into account the sampling method, nested sampling
# method. Big subplots and medium supblots. However,in this case, I assume that the sample plot is 50*50 m plot.

# Moist_Mixed_Deciduous_Forest <- Forests %>%
#   filter(Forest_Type == "Moist_Mixed_Deciduous_Forest")
# 
# 
# AGB_MMDF <- Moist_Mixed_Deciduous_Forest %>%
#   group_by(Plot_id) %>%
#   summarise(AGB_total = sum(AGB, na.rm = TRUE)) %>%
#   mutate(AGB_ha = AGB_total*(1/0.25))%>%
#   mutate(C_ha=AGB_ha*0.47)
# MMDFC <- mean(AGB_MMDF$C_ha, na.rm = T)
# 
# hist(AGB_MMDF$AGB_ha)
# mean(AGB_DF$AGB_ha); median(AGB_MMDF$AGB_ha,na.rm = TRUE)
# mean(AGB_MMDF$C_ha, na.rm = T); median(AGB_MMDF$C_ha, na.rm = T)

# Average Carbon storage comparison by plotting---------
Forest_Type <- c("Dry Forest", "Dry Hill Forest", " Dipterocarpus Forest", " Dry Mixed Deciduous Forest")
Carbon <- rbind(DF,DHF,DIPF, DMDF)

barplot(Carbon, col="darkgreen")

barplot(Carbon, main="Average Carbon Storage", xlab="Forest Type", ylab="Carbon Storage (t/ha)", names.arg=c("DF","DHF","DIPF", "DMDF"),
        border="darkgreen", density=c(90, 70, 50, 40))

# Carbon Table
PSP_Carbon_ha <- read.csv("Data/PSP_plots_carbon.csv")

Carbon_Table <-  Carbon

Carbon_Table<- Carbon_Table %>% select(Forest_Type,C_ha)

Carbon_Table[5,] <- c("Moist_Mixed_Deciduous_Forest", mean(PSP_Carbon_ha$Carbon))

round(as.numeric(Carbon_Table$C_ha),digits = 2)

Carbon_Table$C_ha <- round(as.numeric(Carbon_Table$C_ha),digits = 2)


write.table(Carbon_Table, file = "Data/Combined_Carbon.csv", append = FALSE, quote = FALSE, sep = ",",
            eol = "\n", na = "NA", dec = ".", row.names = FALSE,
            col.names = TRUE, qmethod = c("escape", "double"),
            fileEncoding = "")

# Statistical_Analysis------------------------------------------------------------------

boxplot(C_Tree~Forest_Type , data=Forests)
boxplot(log(C_Tree)~Forest_Type , data=Forests)
lm(log(C_Tree)~Forest_Type , data=Forests)

anova(lm(log(C_Tree)~Forest_Type , data=Forests))
TukeyHSD(lm(log(C_Tree)~Forest_Type , data=Forests))

# pair-wise Test
TukeyHSD(aov(log(C_Tree)~Forest_Type , data=Forests))
lm(log(C_Tree)~Forest_Type+Mean_precipation , data=Forests)
anova(lm(log(C_Tree)~Forest_Type+Mean_precipation , data=Forests))


