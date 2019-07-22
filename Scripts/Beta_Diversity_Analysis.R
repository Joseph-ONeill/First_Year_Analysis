rm(list = ls())

library(vegan)
library(tidyverse)
library(dplyr)

# All_Forests <- read.csv("Data/Forests_All_info.csv")
All_Forests <- read.csv("Data/Final_Dataframe_trees.csv")

All_Forests
sum(is.na(All_Forests$Species_names))
Abundance <- All_Forests %>%
  filter(!is.na(Species_names))%>%
  group_by(Plot_id, Species_names, Forest_type) %>%
  dplyr::summarise(Tree = n()) %>%
  spread(key=Species_names, value = Tree, fill=0)

Site_species <- Abundance[,c(3:nrow(Abundance))]
H<- diversity(Site_species, index = "shannon")
H <- data.frame(H)
SI <- diversity(Site_species, index = "simpson")
SI <- data.frame(SI)
J <- H/log(specnumber(Site_species))
J <- data.frame(J)
beta <- vegdist(Site_species, binary = TRUE)
beta <- data.frame(beta)

Abundance$Shannon <- H$H
Abundance$Simpson <- SI$SI
Abundance$Pieolou_evenness <- J$J

Species_Index <- Abundance[,c(1,2,178,179,180)]

All_Forest_data <- read.csv("Data/Final_Dataframe.csv")

All_Forest_data$Pieolou_evenness <- Species_Index$Pieolou_evenness

write.table(All_Forest_data, file = "Data/Final_Dataframe.csv", append = FALSE, quote = FALSE, sep = ",",
            eol = "\n", na = "NA", dec = ".", row.names = FALSE,
            col.names = TRUE, qmethod = c("escape", "double"),
            fileEncoding = "") 
# Dipterocarpus_Forests---------------------------------------------------------------

Dipterocarpus_Forest <- All_Forests %>%
  filter(Forest_type == "Dipterocarpus_Forest")
mean(Dipterocarpus_Forest$H_m)
mean(Dipterocarpus_Forest$Dbh_cm)

AbundanceDIP <- Dipterocarpus_Forest %>%
  filter(!is.na(Species_names))%>%
  group_by(Plot_id, Species_names) %>%
  dplyr::summarise(Tree = n()) %>%
  spread(key=Species_names, value = Tree, fill=0)

Site_speciesDIP <- AbundanceDIP[,c(2:nrow(AbundanceDIP))]

# All plots without the GPS locations are neglected in the Beta-diversity analysis and GDM analysis

# Shannon index
H_DIP <- diversity(Site_speciesDIP, index = "shannon")

mean(H_DIP)
# simpson index
SI_DIP <- diversity(Site_speciesDIP, index = "simpson")
mean(SI_DIP)
# Pieolou's evenness 
J_DIP <- H_DIP/log(specnumber(Site_speciesDIP))
mean(J_DIP)
#  The Sorensen index of dismillarity is used for the data

beta_DIP <- vegdist(Site_speciesDIP, binary = TRUE)

mean(beta_DIP)


# Dry_Forests---------------------------------------------------------------

Dry_Forest <- All_Forests %>%
  filter(Forest_type == "Dry_Forest")
mean(Dry_Forest$H_m)
mean(Dry_Forest$Dbh_cm)

AbundanceDF <- Dry_Forest %>%
  filter(!is.na(Species_names))%>%
  group_by(Plot_id, Species_names) %>%
  dplyr::summarise(Tree = n()) %>%
  spread(key=Species_names, value = Tree, fill=0)

Site_speciesDF <- AbundanceDF[,c(2:nrow(AbundanceDF))]

# All plots without the GPS locations are neglected in the Beta-diversity analysis and GDM analysis

# Shannon index
H_DF <- diversity(Site_speciesDF, index = "shannon")
mean(H_DF)

# simpson index
SI_DF <- diversity(Site_speciesDF, index = "simpson")
mean(SI_DF)

# Pieolou's evenness 
J_DF <- H_DF/log(specnumber(Site_speciesDF))
mean(J_DF, na.rm = T)
#  The Sorensen index of dismillarity is used for the data.
beta_DF <- vegdist(Site_speciesDF, binary = TRUE)
mean(beta_DF, na.rm = T)

anova(beta_DF, beta_DIP)
# Dry_Hill_Forests---------------------------------------------------------------

Dry_Hill_Forest <- All_Forests %>%
  filter(Forest_type == "Dry_Hill_Forest")
mean(Dry_Hill_Forest$H_m)
mean(Dry_Hill_Forest$Dbh_cm)


AbundanceDHF <- Dry_Hill_Forest %>%
  filter(!is.na(Species_names))%>%
  group_by(Plot_id, Species_names) %>%
  dplyr::summarise(Tree = n()) %>%
  spread(key=Species_names, value = Tree, fill=0)

Site_speciesDHF <- AbundanceDHF[,c(2:nrow(AbundanceDHF))]

# All plots without the GPS locations are neglected in the Beta-diversity analysis and GDM analysis

# Shannon index
H_DHF <- diversity(Site_speciesDHF, index = "shannon")
mean(H_DHF)

# simpson index
SI_DHF <- diversity(Site_speciesDHF, index = "simpson")
mean(SI_DHF)
# Pieolou's evenness 
J_DHF <- H_DHF/log(specnumber(Site_speciesDHF))
mean(J_DHF, na.rm = T)

#  The Sorensen index of dismillarity is used for the data.
beta_DHF <- vegdist(Site_speciesDHF, binary = TRUE)
mean(beta_DHF)

# Dry_Mixed_Deciduous_Forests---------------------------------------------------------------

Dry_Mixed_Deciduous_Forest <- All_Forests %>%
  filter(Forest_type == "Dry_Mixed_Deciduous_Forest")

mean(Dry_Mixed_Deciduous_Forest$H_m)
mean(Dry_Mixed_Deciduous_Forest$Dbh_cm)

AbundanceDMDF <- Dry_Mixed_Deciduous_Forest %>%
  filter(!is.na(Species_names))%>%
  group_by(Plot_id, Species_names) %>%
  dplyr::summarise(Tree = n()) %>%
  spread(key=Species_names, value = Tree, fill=0)

Site_speciesDMDF <- AbundanceDMDF[,c(2:nrow(AbundanceDMDF))]

# All plots without the GPS locations are neglected in the Beta-diversity analysis and GDM analysis

# Shannon index
H_DMDF <- diversity(Site_speciesDMDF, index = "shannon")
mean(H_DMDF)

# simpson index
SI_DMDF <- diversity(Site_speciesDMDF, index = "simpson")
mean(SI_DMDF)
# Pieolou's evenness 
J_DMDF <- H_DMDF/log(specnumber(Site_speciesDMDF))
mean(J_DMDF)

#  The Sorensen index of dismillarity is used for the data.
beta_DMDF <- vegdist(Site_speciesDMDF, binary = TRUE)
mean(beta_DMDF)

# Moist_Mixed_Deciduous_Forest---------------------------------------------------------------

Moist_Mixed_Deciduous_Forest <- All_Forests %>%
  filter(Forest_type == "Moist_Mixed_Deciduous_Forest")

mean(Moist_Mixed_Deciduous_Forest$H_m, na.rm = T)

mean(Moist_Mixed_Deciduous_Forest$Dbh_cm, na.rm = T)



AbundanceMMDF <- Moist_Mixed_Deciduous_Forest %>%
  filter(!is.na(Species_names))%>%
  group_by(Plot_id, Species_names) %>%
  dplyr::summarise(Tree = n()) %>%
  spread(key=Species_names, value = Tree, fill=0)

Site_speciesMMDF <- AbundanceMMDF[,c(2:nrow(AbundanceMMDF))]

# All plots without the GPS locations are neglected in the Beta-diversity analysis and GDM analysis

# Shannon index
H_MMDF <- diversity(Site_speciesMMDF, index = "shannon")
MMDF_data <- Moist_Mixed_Deciduous_Forest %>% group_by(Plot_id) %>% mutate(Shannon = H_MMDF)
mean(H_MMDF)
# simpson index
SI_MMDF <- diversity(Site_speciesMMDF, index = "simpson")
mean(SI_MMDF)
# Pieolou's evenness 
J_MMDF <- H_MMDF/log(specnumber(Site_speciesMMDF))
mean(J_MMDF, na.rm = T)
#  The Sorensen index of dismillarity is used for the data.
beta_MMDF <- vegdist(Site_speciesMMDF, binary = TRUE)
mean(beta_MMDF)


# Statistical analysis---------------------------

hist(H_DF)
hist(H_DHF)
hist(H_DIP)
hist(H_DMDF)
hist(H_MMDF)


anova(H_DF,H_DHF)


























# # Renyi diversities
# 
# k <- sample(nrow(Site_species),6)
# 
# R <- renyi(Site_species[k,])
# 
# plot(R)
# 
# 
# 
# 
# # the Alpha parameter of Fisher's log series as a diversity index. 
# 
# 
# alpha <- fisher.alpha(Site_species, MARGIN = 1)
# 
# # Function fisherfit fits Fisher's logseries to abundance data
# fisherfit(Site_species)
# 
# #  The number of stems per plot in our data set
# 
# quantile(rowSums(Site_species))
# 
# # To express richness for the same number of individuals
# 
# Srar <- rarefy(Site_species, min(rowSums(Site_species)))
# 
# # Rarefy sample size to two individuals (As an extreme case)
# 
# S2 <- rarefy(Site_species, 2)
# 
# # This rarefaction richness does not give equal rank order with the previous rarefetion richness
# 
# all(rank(Srar)) == rank(S2)
# 
# #  in this rarefied richness fro tow individuals is a finite sample variant of Simpson's diversity index. These two are almost identical in BCI
# range(diversity(Site_species, "simp") - (S2 -1))
# 
# #  Rarefction is sometimes presented as an ecological meaningful alternative to dubious diversity indices. But the differences really seem to be small.
# 
# # Species abundance models--------------------------------------------------------------
# 
# #  The variance measures of species abundance distribution are divresity indices.
# 
# #  Fihser and Preston. Fisher's log-series for a randomly selected plot is
# 
# k <- sample(nrow(Site_species),1)
# k <- as.numeric(k)
# fish <- fisherfit(Site_species[k,])
# #  fisher's log-series fitted to one randomly selected site
# fish
# plot(fish)
# 
# 
# #  Preston's log-normal is the main challenges to Fihers' log-sereis. Instead of plotting species by frequencies, it bins species into frequency classes of increasing size. As a result, upper bins with high range of frequencies become more common. 
# # Prestondistr is the recommmende log normal model and it maximizes truncated log-normal likelihood without binning data. 
# 
# prestondistr(Site_species[k,])
# 
# # Ranked abundance distribution---------------------------
# # Alternative approach to species abundance distribution is to plot logrithmic abundances in dereasing orrder know as Whitaker plots or abundance distribution curves. 
# #  it is fit with most popular models using maximum likelihood estimation. Fiver model; Brokenstick, preemption, log-ormal, Zipf, Zipf-Mandelbrot.
# #  This function compares the model using Akaike's or Schwartz's Bayesian information criteria (BCI). Eventhough it is based on log-likelihood, it penalized by the 
# # number of estimated parameters. 2 in AIC and logS in BIC.  Log-normal model rarel is the choice, but it is regarded as a the canonical model. 
# rad <- radfit(Site_species[k,])
# plot(rad)
# AIC(rad)
# # Species accumulation-------------------
# # This model study collection of sites and their species richness or estimate the number of unseen species. It is similar to rarefaction becuase it can accumulate more spices when the 
# # the number of sites increases. 
# #  Kindt's exact accumulator resembles rarefaction is the recommended one. 
# sac <- specaccum(Site_species)
# plot(sac, ci.type="polygon", ci.col="green")
# 
# 
# # Beta Diversityy-------------------------------
# # Diversity can be divided into various components. Alpha diversity, and the diversity along gradients, beta diversity. Beta diversity should be studied together with
# # gradiets as it is a measure of heterogenity (tuomisto) (i.e. the relative abundance of species in a collection of sites to an average site.
# # This index is based on the total number of speices in a collection of sites S and the average richness per one site. 
# ncol(Site_species)/mean(specnumber(Site_species))-1
# 
# # as total number of species can increase along with the site number even they are parts of the same community, Whttaker suggested that pairwise comparison of sites is required.
# #  The Sorensen index of dismillarity is used for the data.
# beta <- vegdist(Site_species, binary = TRUE)
# mean(beta)
# 
# plot(beta)
