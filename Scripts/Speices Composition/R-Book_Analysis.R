rm(list = ls())

# install.packages("tidyverse")
# install.packages("lubridate")
# install.packages("BIOMASS")

library(tidyverse) 
library(lubridate)
library(BIOMASS)

Forests <- read.csv("Data/Main_data.csv")

names(Forests)
head(Forests)
str(Forests)

Taxo <- correctTaxo(genus=Forests$Genus,species = Forests$Species)

Forests$genusCorr <- Taxo$genusCorrected
Forests$speciesCorr <- Taxo$speciesCorrected

APG <- getTaxonomy(Forests$genusCorr, findOrder =T)
Forests$familyAPG<-APG$family
Forests$orderAPG<-APG$order


dataWD <- getWoodDensity(genus=Forests$genusCorr,
                         species=Forests$speciesCorr,
                         stand=Forests$Plot_id)

sum(dataWD$levelWD == "species")

sum(dataWD$levelWD == "genus")

sum(!dataWD$levelWD%in%c("genus","species"))

Forests$WD <- dataWD$meanWD 

View(Forests)


# Develop HD model---------------------------------------------------------------------------------------

HDmodels <- modelHD(D=Forests$Dbh_cm, 
                    H = Forests$H_m,
                    drawGraph=TRUE,
                    useWeight=TRUE)

Best_HDmodel<- modelHD(D=Forests$Dbh_cm,
                       H=Forests$H_m,
                       method="log3",
                       useWeight =TRUE)


# Getting missing tree Height-----------------------------------------------------------------------------

dataHlocal<-retrieveH(D=Forests$Dbh_cm[is.na(Forests$H_m)],
                      model = Best_HDmodel)

Forests$H_m[is.na(Forests$H_m)] <- dataHlocal$H

sum(is.na(Forests$Dbh_cm))

# Estimate All Biomass-------------------------------------------------------------------------------------- 


Forests$AGB <- computeAGB(D=Forests$Dbh_cm,
                          WD=Forests$WD,
                          H=Forests$H_m)

# Different Forests, Biomass and carbon (t/ha) -------------------------------------------------------------------
# Carbon content default value (CC) 0.47 was used to evaluate the carbon content (IPCC, 2006). 

# Dipterocarpus_Forest------------------------------------------------------------------------------------
Dipterocarpus_Forest <- Forests %>%
  filter(Forest_Type == "Dipterocarpus_Forest")

AGB_DIPF <- Dipterocarpus_Forest %>%
  group_by(Plot_id) %>%
  summarise(AGB_total = sum(AGB)) %>%
  mutate(AGB_ha = AGB_total*(1/0.16)) %>%
  mutate(C_ha=AGB_ha*0.47)


DIPFC<- mean(AGB_DIPF$C_ha)

hist(AGB_DIPF$AGB_ha)
mean(AGB_DIPF$AGB_ha); median(AGB_DIPF$AGB_ha)
mean(AGB_DIPF$C_ha); median(AGB_DIPF$C_ha, na.rm = T)

# Dry_Forest---------------------------------------------------------------------------------------
Dry_Forest <- Forests %>%
  filter(Forest_Type == "Dry_Forest")

AGB_DF <- Dry_Forest %>%
  group_by(Plot_id) %>%
  summarise(AGB_total = sum(AGB)) %>%
  mutate(AGB_ha = AGB_total*(1/0.16))%>%
  mutate(C_ha=AGB_ha*0.47)
DFC <- mean(AGB_DF$C_ha)

hist(AGB_DF$AGB_ha)
mean(AGB_DF$AGB_ha); median(AGB_DF$AGB_ha)
mean(AGB_DF$C_ha); median(AGB_DF$C_ha, na.rm = T)

# Dry_Hill_Forest-----------------------------------------------------------------------------------
Dry_Hill_Forest <- Forests %>%
  filter(Forest_Type == "Dry_Hill_Forest")

AGB_DHF <- Dry_Hill_Forest %>%
  group_by(Plot_id) %>%
  summarise(AGB_total = sum(AGB)) %>%
  mutate(AGB_ha = AGB_total*(1/0.16))%>%
  mutate(C_ha=AGB_ha*0.47)

DHFC <- mean(AGB_DHF$C_ha)

hist(AGB_DHF$AGB_ha)
mean(AGB_DHF$AGB_ha); median(AGB_DHF$AGB_ha)
mean(AGB_DIPF$C_ha); median(AGB_DIPF$C_ha, na.rm = T)

# Dry_Mixed_Deciduous_Forest------------------------------------------------------------------------
Dry_Mixed_Deciduous_Forest <- Forests %>%
  filter(Forest_Type == "Dry_Mixed_Deciduous_Forest")

AGB_DMDF <- Dry_Mixed_Deciduous_Forest %>%
  group_by(Plot_id) %>%
  summarise(AGB_total = sum(AGB)) %>%
  mutate(AGB_ha = AGB_total*(1/0.16))%>%
  mutate(C_ha=AGB_ha*0.47)
DMDFC <- mean(AGB_DMDF$C_ha)

hist(AGB_DMDF$AGB_ha)
mean(AGB_DMDF$AGB_ha); median(AGB_DMDF$AGB_ha)
mean(AGB_DMDF$C_ha); median(AGB_DMDF$C_ha, na.rm = T)

# Moist_Mixed_Deciduous_Forest-----------------------------------------------------------------------------
# There is another calculation for this forest type which takes into account the sampling method, nested sampling
# method. Big subplots and medium supblots. However,in this case, I assume that the sample plot is 50*50 m plot.

Moist_Mixed_Deciduous_Forest <- Forests %>%
  filter(Forest_Type == "Moist_Mixed_Deciduous_Forest")


AGB_MMDF <- Moist_Mixed_Deciduous_Forest %>%
  group_by(Plot_id) %>%
  summarise(AGB_total = sum(AGB, na.rm = TRUE)) %>%
  mutate(AGB_ha = AGB_total*(1/0.25))%>%
  mutate(C_ha=AGB_ha*0.47)
MMDFC <- mean(AGB_MMDF$C_ha, na.rm = T)


# Average Carbon storage comparison by plotting---------
Forest_Type <- c("Dry Forest", "Dry Hill Forest", " Dipterocarpus Forest", " Dry Mixed Deciduous Forest", "Moist Mixed Deciduous Forest")
Carbon <- c(DFC,DHFC,DIPFC, DMDFC, MMDFC)
barplot(Carbon, col="darkgreen")

barplot(Carbon, main="Average Carbon Storage", xlab="Forest Type", ylab="Carbon Storage (t/ha)", names.arg=c("DF","DHF","DIPF", "DMDF", "MMDF"),
        border="darkgreen", density=c(90, 70, 50, 40, 30))

# Carbon TAble
Carbon_Table <-  data.frame(Forest_Type, Carbon)

#Testing.For the whole forests assuming they are same plot size-----------------------------

Test_data <- Forests[ ,c("Forest_Type","Plot_id","Binomial","Genus","Species", "Family","Dbh_cm", "H_m", "BA","WD","AGB","E", "N", "Mean_precipation","Mean_temperature")] %>% 
  mutate(C_Tree=AGB*0.47)

write.csv(Test_data, file = "Data/All_data.csv")


Test_data$Size <- as.character(Test_data$Forest_Type)
Test_data[Test_data$Size!="Moist_Mixed_Deciduous_Forest", c("Size")] <- 0.16
Test_data[Test_data$Size=="Moist_Mixed_Deciduous_Forest", c("Size")] <- 0.25


Test_data$Size <- as.double(Test_data$Size)

Carbon_Storage <- Test_data %>%
  group_by(Plot_id, Forest_Type, Size) %>%
  summarise(AGB_total = sum(AGB, na.rm = TRUE)) %>%
  mutate(Carbon_perHA= AGB_total / Size*0.47)

names(Carbon_Storage)
with(Carbon_Storage, tapply(Carbon_perHA, Forest_Type, mean))

summary(Carbon)
plot(Carbon$Carbon_perHA)


plot(Dbh_cm,H_m, pch=21,bg="green")


