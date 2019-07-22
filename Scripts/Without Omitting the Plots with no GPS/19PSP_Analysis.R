rm(list = ls())
library(tidyverse) 
library(lubridate)
library(BIOMASS)
library(ape)

summarise = dplyr::summarise


# Carbon assessment for the Permanent Sample Plots (PSP)------------------------------------------------------------

The_PSP <- read_csv("Data/19PSP_data.csv")


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

LmHD <- lm(The_PSP$H_m~The_PSP$Dbh_cm)
summary(LmHD)

