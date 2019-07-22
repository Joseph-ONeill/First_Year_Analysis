
# Load the Raw data file
rm(list = ls())
library(plyr)
library(tidyverse)
library(vegan)
library(ape)

Raw_data <- read.csv("data/Main_data.csv")
str(Raw_data)
summary(Raw_data)
nrow(Raw_data)


forest.type <- unique(Raw_data[,c(5,12)])
forest.type$Plot_id <- 1:nrow(forest.type)
# unique(forest.type$Forest_Type)


forest.type$colour <- mapvalues(forest.type$Forest_Type, 
                                levels(forest.type$Forest_Type),
                                c("red", "blue", "#000000", "green", "brown"))

# basal.area.fn <- function(x){ (pi*(x)^2)/40000 } # calculate basal area in m^2
# Raw_data <- mutate(Raw_data, BA=basal.area.fn(Raw_data$Dbh_cm))
# Transforming the data.frame into matrix form
Data <- Raw_data %>%
  filter(!is.na(Binomial)) %>%
  group_by(Plot_id, Binomial) %>%
  summarise(Tree = n()) %>%
  spread(key=Binomial, value = Tree, fill=0)

pcoa.object <-  vegdist(Data[,-1]) %>% pcoa()

# biplot(pcoa.object$vectors$Axis.1, col=forest.type$group)

biplot.dat <- data.frame(comp1 = pcoa.object$vectors[,1],
                         comp2 = pcoa.object$vectors[,2],
                         forest_type = forest.type$Forest_Type)

ggplot(biplot.dat, aes(comp1, comp2, color = forest_type)) +
  geom_point(alpha = 0.5)+ ggtitle ("Principal Component Analysis (PCoA-PC1 Vs PC2)")+
  xlab("PC1 - Percent Variation Explained")+ 
  ylab("PC2 - Percent Variation Explained")

