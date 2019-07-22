#PCA Analysis for all forests


# Clean R brain and install library

rm(list = ls())
# install.packages("tidyverse")
# install.packages("vegan")
install.packages("ape")
library(tidyverse)
library(vegan)
library(ape)


# Load the Raw data file
Raw_data <- read.csv("data/PCA_main_data.csv")
str(Raw_data)
summary(Raw_data)


# Transforming the data.frame into matrix form
Data <- Raw_data %>%
  filter(!is.na(Binomial)) %>%
  group_by(Plot_id, Binomial) %>%
  summarise(Tree = n()) %>%
  spread(key=Binomial, value = Tree, fill=0)

## Example 1: Unconstrained ordination
## NMDS
Forests.NMDS <- vegdist(Data[,-1]) %>% metaMDS(k = 2, distfun = betadiver, distance = "sim",trymax=100,zerodist="add" )
plot(Forests.NMDS, type = "t")

# species as symbols
points(Forests.NMDS, display = 'species', pch = '+', cex = 0.6)

# Plots as text
  text(Forests.NMDS, display = 'sites')

# Example 2: Distance measures except Euclidean are appropriate. But, it assumed as linerar. 
pcoa.object <-  vegdist(Data[,-1]) %>% pcoa()
biplot(pcoa.object)











