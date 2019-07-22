rm(list = ls())
install.packages("dplyr")
install.packages("BIOMASS")
install.packages("ape")
install.packages("lubridate")


library(tidyverse) 
library(lubridate)
library(BIOMASS)
library(ape)
library(dplyr)

summarise = dplyr::summarise


# Carbon assessment for the Permanent Sample Plots (PSP)------------------------------------------------------------

The_PSP <- read_csv("Data/PSP_data_Final.csv")


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

# YOMA----------------------------------------------------------------------------------------

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

# Diameter distribution in three areas---------------------------------------------------------------------------

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
  theme(text = element_text(size=15), axis.text.x = element_blank(),
        axis.title.x = element_text(margin = margin(t = 18, r = 0, b = 0, l = 0)),
        axis.title.y = element_text(margin = margin(t = 0, r = 18, b = 0, l = 0)))

# Height distribution in three areas---------------------------------------------------------------------------------
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
  theme(text = element_text(size=15), axis.text.x = element_blank(),
        axis.title.x = element_text(margin = margin(t = 18, r = 0, b = 0, l = 0)),
        axis.title.y = element_text(margin = margin(t = 0, r = 18, b = 0, l = 0)))


# Height and diameter relationship------------------------------------------------------------------------------------
The_PSP %>%
  na.omit %>%
  ggplot(aes(y = H_m, x = Dbh_cm)) +
  geom_point(position = "jitter", color = "green", size = 3)+
  xlab("DBH (cm)")+
  ylab("Height-H (m)")+
  scale_x_continuous(breaks = c(0,20,40,60,80,100, 120))+
  geom_smooth(method = "lm")+
  theme_bw() +
  theme(text = element_text(size=15),axis.text.x  = element_text(angle=0, hjust=2),
        axis.title.x = element_text(margin = margin(t = 18, r = 0, b = 0, l = 0)),
        axis.title.y = element_text(margin = margin(t = 0, r = 18, b = 0, l = 0)))

The_PSP %>% lm(H_m~Dbh_cm)

LmHD <- lm(The_PSP$H_m~The_PSP$Dbh_cm)
summary(LmHD)


# Estimate Biomass---------------------------------------------------------------------------------------------


The_PSP_without_small <- The_PSP %>% filter(Subplot_area != 0.015625)
The_PSP_med <- The_PSP_without_small%>% filter(Subplot_area == 0.0625)
The_PSP_large <- The_PSP_without_small %>% filter(Subplot_area == 0.25)

AGBPlotList <- by(The_PSP_med, The_PSP_med$subplot_id,
                  function(x) computeAGB(D=x$Dbh_cm,WD=x$WD,H=x$H_m),
                  simplify=F)

AGBplot <-sapply(AGBPlotList,sum) 

print(The_PSP)


AGBPlotList_med<-by(The_PSP_med, The_PSP_med$subplot_id,
                    function(x) computeAGB(D=x$Dbh_cm,WD=x$WD,H=x$H_m),
                    simplify=F)
AGBplot_med<-sapply(AGBPlotList_med,sum) 

AGBPlotList_large<-by(The_PSP_large, The_PSP_large$subplot_id,
                      function(x) computeAGB(D=x$Dbh_cm,WD=x$WD,H=x$H_m),
                      simplify=F)
AGBplot_large<-sapply(AGBPlotList_large,sum) 

AGBplot_med<-data.frame(plot = names(AGBplot_med), agb = AGBplot_med, row.names = NULL)
AGBplot_large<-data.frame(plot = names(AGBplot_large), agb = AGBplot_large, row.names = NULL)
AGBplot<-merge(AGBplot_med, AGBplot_large, by = "plot", all.y = TRUE)
AGBplot$agb.x[is.na(AGBplot$agb.x)]<-0

# Change ha value

Carbon_ha <- mutate(AGBplot, agb_X_ha = agb.x*(1/0.0625), agb_y_ha = agb.y*(1/0.25)) %>%
  mutate(AGB_Total = agb_X_ha + agb_y_ha) %>%
  mutate(Carbon = AGB_Total*0.47) %>% 
  mutate(area = gsub("_[[:digit:]]*_[[:digit:]]*$", "", plot))

Average_carbon_ha <- data.frame(Area=Carbon_ha$area, Carbon=Carbon_ha$Carbon) %>% group_by(Area) %>% summarise(Avg_Carbon_ha=mean(Carbon))


ggplot(Average_carbon_ha, aes(x= Area, y = Avg_Carbon_ha)) + 
  geom_point() + ggtitle("Carbon(tC/ha) Distribution")+
  xlab("Locations") +
  ylab("Carbon (t/ha)")

Carbon_ha %>% ggplot(aes(x= area, y = log(Carbon), fill = area)) + geom_boxplot()+
  xlab("Location")+
  ylab("Log(Carbon - tC/ha)")+
  theme_bw() +
  theme(text = element_text(size=15), axis.text.x = element_blank(),
      axis.title.x = element_text(margin = margin(t = 18, r = 0, b = 0, l = 0)),
      axis.title.y = element_text(margin = margin(t = 0, r = 18, b = 0, l = 0)))


