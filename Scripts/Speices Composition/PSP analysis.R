title: "Kyaw_Sein_Win_Tun_Biomass_for_19PSP"
rm(list = ls())

# install.packages("vegan")
# install.packages("plyr")
# install.packages("ape")
# install.packages("lubridate")
# install.packages("BIOMASS")
# install.packages("tidyverse")


library(plyr)
library(tidyverse)
library(vegan)
library(ape)
library(tidyverse)
library(lubridate)
library(BIOMASS)
library(vegan)

# Carbon assessment------

The_PSP <- read_csv("Data/19PSP.csv")
head(The_PSP)
str(The_PSP)
The_PSP<-The_PSP %>% mutate(land_tenure = ifelse(Location  == "Yoma_Unclassfied_Forest", "Unclassified", "Reserve"))
The_PSP$land_tenure

Taxo <- correctTaxo(genus=The_PSP$Genus, species = The_PSP$Species)

The_PSP$genusCorr <- Taxo$genusCorrected
The_PSP$speciesCorr <- Taxo$speciesCorrected

APG <- getTaxonomy(The_PSP$genusCorr, findOrder =T)
The_PSP$familyAPG <- APG$family
The_PSP$orderAPG <- APG$order

dataWD <- getWoodDensity(genus=The_PSP$genusCorr,
                         species=The_PSP$speciesCorr,
                         stand=The_PSP$Subplot_area)

sum(dataWD$levelWD == "species")

sum(dataWD$levelWD == "genus")

sum(!dataWD$levelWD%in%c("genus","species"))

The_PSP <- The_PSP %>% 
  mutate(subplot_id = paste(Location, Compartment, Plot, sep="_"))

# Diameter distribution in three areas
The_PSP_area <- The_PSP %>% mutate(area = gsub("_[[:digit:]]*_[[:digit:]]*$", "", subplot_id))
The_PSP_area %>%
  na.omit %>%
  ggplot(aes(y = Dbh_cm, x = area)) +
  geom_point(position = "jitter", color = "green", size = 3)+
  ggtitle("Diameter Distribution")+
  xlab("Locations")+
  ylab("Diameter in Centimeter")+
  theme_bw()

# Height distribution in three areas
The_PSP_area %>%
  na.omit %>%
  ggplot(aes(y = H_m, x = area)) +
  geom_point(position = "jitter", color = "green", size = 3)+
  ggtitle("Tree Height Distribution")+
  xlab("Locations")+
  ylab("Height in meter")+
  theme_bw()

# Height and diameter relationship
The_PSP_area %>%
  na.omit %>%
  ggplot(aes(y = H_m, x = Dbh_cm)) +
  geom_point(position = "jitter", color = "green", size = 3)+
  ggtitle("Height and diameter relationship")+
  xlab("Diameter at breath Height (DBH)")+
  ylab("Height in meter")+
  theme_bw()+ stat_smooth()

# species composition in each area
species_composition <- The_PSP_area %>% select(Binomial,area) %>% group_by(Binomial, area) 

# Estimate Biomass
The_PSP$WD=dataWD$meanWD

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

mean(Carbon_ha$Carbon)

write.table(Carbon_ha,"Data/PSP_carbon.csv", quote = FALSE,sep = ",", row.names = FALSE)

Average_carbon_ha <- data.frame(Area=Carbon_ha$area, Carbon=Carbon_ha$Carbon) %>% group_by(Area) %>% summarise(Avg_Carbon_ha=mean(Carbon))
mean(Average_carbon_ha$Avg_Carbon_ha)

ggplot(Average_carbon_ha, aes(x= Area, y = Avg_Carbon_ha)) + 
  geom_point() + ggtitle("Average Carbon(t/ha) Distribution")+
  xlab("Locations") +
  ylab("Carbon (t/ha)")

Carbon_ha %>% ggplot(aes(x= area, y = log(Carbon), fill = area)) + geom_boxplot()


# Land Tenure Analysis-----------------------

Tenure <- unique(The_PSP[,c("Plot", "land_tenure")]) %>%
  arrange(Plot) %>%
  mutate(land_tenure = as.factor(land_tenure))

Tenure$colour <- mapvalues(Tenure$land_tenure, 
                                levels(Tenure$land_tenure),
                                c("red", "green"))


Data <- The_PSP %>%
  filter(!is.na(Binomial)) %>%
  group_by(Plot, Binomial) %>%
  summarise(Tree = n()) %>%
  spread(key=Binomial, value = Tree, fill=0)

pcoa.object <-  vegdist(Data[,-1]) %>% pcoa()

# biplot(pcoa.object$vectors$Axis.1, col=forest.type$group)

biplot.dat <- data.frame(comp1 = pcoa.object$vectors[,1],
                         comp2 = pcoa.object$vectors[,2],
                         Tenure = Tenure$land_tenure)

ggplot(biplot.dat, aes(comp1, comp2, color = Tenure)) +
  geom_point(alpha = 0.5)+ ggtitle ("Principal Component Ordination Analysis (PCoA-PC1 Vs PC2)")+
  xlab("PC1 - Percent Variation Explained")+ 
  ylab("PC2 - Percent Variation Explained")


# MDS--------


# Now draw an MDS plot using the same data and the Euclidean distance. This graph should look the same as the PCA plot


## first, calculate the distance matrix using the Euclidian distance.
## NOTE: We are transposing, scaling and centering the data just like PCA.
distance.matrix <- dist(scale(Data, center=TRUE, scale=TRUE),
                        method="euclidean")

## do the MDS math (this is basically eigen value decomposition)
mds.stuff <- cmdscale(distance.matrix, eig=TRUE, x.ret=TRUE)

## calculate the percentage of variation that each MDS axis accounts for...
mds.var.per <- round(mds.stuff$eig/sum(mds.stuff$eig)*100, 1)
mds.var.per

## now make a fancy looking plot that shows the MDS axes and the variation:
mds.values <- mds.stuff$points
mds.data <- data.frame(X=mds.values[,1],Y=mds.values[,2])
mds.data

ggplot(data=mds.data, aes(x=X, y=Y)) +
  geom_point() +
  theme_bw() +
  xlab(paste("MDS1 - ", mds.var.per[1], "%", sep="")) +
  ylab(paste("MDS2 - ", mds.var.per[2], "%", sep="")) +
  ggtitle("MDS plot using Euclidean distance")

## 3) Now draw an MDS plot using the same data and the average log(fold change)
##    This graph should look different than the first two


## first, take the log2 of all the values in the data.matrix.
## This makes it easy to compute log2(Fold Change) between a gene in two
## samples since...
##
## log2(Fold Change) = log2(value for sample 1) - log2(value for sample 2)
##
log2.data.matrix <- log2(Data)

## now create an empty distance matrix
log2.distance.matrix <- matrix(0,
                               nrow=ncol(log2.data.matrix),
                               ncol=ncol(log2.data.matrix),
                               dimnames=list(colnames(log2.data.matrix),
                                             colnames(log2.data.matrix)))

log2.distance.matrix

## now compute the distance matrix using avg(absolute value(log2(FC)))
for(i in 1:ncol(log2.distance.matrix)) {
  for(j in 1:i) {
    log2.distance.matrix[i, j] <-
      mean(abs(log2.data.matrix[,i] - log2.data.matrix[,j]))
  }
}
log2.distance.matrix

## do the MDS math (this is basically eigen value decomposition)
## cmdscale() is the function for "Classical Multi-Dimensional Scalign"
mds.stuff <- cmdscale(as.dist(log2.distance.matrix),
                      eig=TRUE,
                      x.ret=TRUE)

## calculate the percentage of variation that each MDS axis accounts for...
mds.var.per <- round(mds.stuff$eig/sum(mds.stuff$eig)*100, 1)
mds.var.per

## now make a fancy looking plot that shows the MDS axes and the variation:
mds.values <- mds.stuff$points
mds.data <- data.frame(Sample=rownames(mds.values),
                       X=mds.values[,1],
                       Y=mds.values[,2])
mds.data

ggplot(data=mds.data, aes(x=X, y=Y)) +
  geom_point() +
  theme_bw() +
  xlab(paste("MDS1 - ", mds.var.per[1], "%", sep="")) +
  ylab(paste("MDS2 - ", mds.var.per[2], "%", sep="")) +
  ggtitle("MDS plot using avg(logFC) as the distance")
