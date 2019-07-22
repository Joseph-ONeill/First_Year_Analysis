rm(list = ls())

library(vegan)
library(tidyverse)

All_Forests <- read.csv("Data/All_data.csv")

sum(is.na(All_Forests$Species_names))
Abundance <- All_Forests %>%
  filter(!is.na(Species_names))%>%
  group_by(Plot_id, Species_names) %>%
  summarise(Tree = n()) %>%
  spread(key=Species_names, value = Tree, fill=0)



Site_species <- Abundance[,c(2:nrow(Abundance))]

# All plots without the GPS locations are neglected in the Beta-diversity analysis and GDM analysis

# Shannon index
H <- diversity(Site_species, index = "shannon")

# simpson index
SI <- diversity(Site_species, index = "simpson")

# Pieolou's evenness 
J <- H/log(specnumber(Site_species))

# Renyi diversities

k <- sample(nrow(Site_species),6)

R <- renyi(Site_species[k,])

plot(R)




# the Alpha parameter of Fisher's log series as a diversity index. 


alpha <- fisher.alpha(Site_species, MARGIN = 1)

# Function fisherfit fits Fisher's logseries to abundance data
fisherfit(Site_species)

#  The number of stems per plot in our data set

quantile(rowSums(Site_species))

# To express richness for the same number of individuals

Srar <- rarefy(Site_species, min(rowSums(Site_species)))

# Rarefy sample size to two individuals (As an extreme case)

S2 <- rarefy(Site_species, 2)

# This rarefaction richness does not give equal rank order with the previous rarefetion richness

all(rank(Srar)) == rank(S2)

#  in this rarefied richness fro tow individuals is a finite sample variant of Simpson's diversity index. These two are almost identical in BCI
range(diversity(Site_species, "simp") - (S2 -1))

#  Rarefction is sometimes presented as an ecological meaningful alternative to dubious diversity indices. But the differences really seem to be small.

# Species abundance models--------------------------------------------------------------

#  The variance measures of species abundance distribution are divresity indices.

#  Fihser and Preston. Fisher's log-series for a randomly selected plot is

k <- sample(nrow(Site_species),1)
k <- as.numeric(k)
fish <- fisherfit(Site_species[k,])
#  fisher's log-series fitted to one randomly selected site
fish
plot(fish)


#  Preston's log-normal is the main challenges to Fihers' log-sereis. Instead of plotting species by frequencies, it bins species into frequency classes of increasing size. As a result, upper bins with high range of frequencies become more common. 
# Prestondistr is the recommmende log normal model and it maximizes truncated log-normal likelihood without binning data. 

prestondistr(Site_species[k,])

# Ranked abundance distribution---------------------------
# Alternative approach to species abundance distribution is to plot logrithmic abundances in dereasing orrder know as Whitaker plots or abundance distribution curves. 
#  it is fit with most popular models using maximum likelihood estimation. Fiver model; Brokenstick, preemption, log-ormal, Zipf, Zipf-Mandelbrot.
#  This function compares the model using Akaike's or Schwartz's Bayesian information criteria (BCI). Eventhough it is based on log-likelihood, it penalized by the 
# number of estimated parameters. 2 in AIC and logS in BIC.  Log-normal model rarel is the choice, but it is regarded as a the canonical model. 
rad <- radfit(Site_species[k,])
plot(rad)
AIC(rad)
# Species accumulation-------------------
# This model study collection of sites and their species richness or estimate the number of unseen species. It is similar to rarefaction becuase it can accumulate more spices when the 
# the number of sites increases. 
#  Kindt's exact accumulator resembles rarefaction is the recommended one. 
sac <- specaccum(Site_species)
plot(sac, ci.type="polygon", ci.col="green")


# Beta Diversityy-------------------------------
# Diversity can be divided into various components. Alpha diversity, and the diversity along gradients, beta diversity. Beta diversity should be studied together with
# gradiets as it is a measure of heterogenity (tuomisto) (i.e. the relative abundance of species in a collection of sites to an average site.
# This index is based on the total number of speices in a collection of sites S and the average richness per one site. 
ncol(Site_species)/mean(specnumber(Site_species))-1

# as total number of species can increase along with the site number even they are parts of the same community, Whttaker suggested that pairwise comparison of sites is required.
#  The Sorensen index of dismillarity is used for the data.
beta <- vegdist(Site_species, binary = TRUE)
mean(beta)

