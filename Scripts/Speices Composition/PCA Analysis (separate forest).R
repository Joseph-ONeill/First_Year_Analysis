#PCA Analysis based on basal area of each tree in each plot for each forest type.

rm(list = ls())
library(tidyverse)

Raw_data <- read.csv("data/Forests_WCdata.csv")
str(Raw_data)
View(Raw_data)   
names(Raw_data)
levels(Raw_data$Forest_Type)
plot(y=Raw_data$H_m, x=Raw_data$Dbh_cm)


# Calculating Basal Area. Function to calculate tree basal area (in m^2). Input data: tree diameter at breast height in centimeters
basal.area.fn <- function(x){ (pi*(x)^2)/40000 } # calculate basal area in m^2
Raw_data <- mutate(Raw_data, BA=basal.area.fn(Raw_data$Dbh_cm))


# Principal Component anaysis for each forest

# Dipterocarpus forests-----

PCA_main <- Raw_data %>% select(Forest_Type, Binomial, Plot_id, BA)
Dipterocarpus_Forest <- PCA_main %>% filter(Forest_Type=="Dipterocarpus_Forest") %>% 
  select(Binomial, Plot_id, BA)

BA_Test <- Raw_data %>% filter(Forest_Type=="Dipterocarpus_Forest") %>% 
  filter(Plot_id=="1") %>% 
  group_by(Binomial) %>% summarise(BA_sum=sum(BA))

DI_PCA <- Dipterocarpus_Forest %>%
  filter(!is.na(Binomial)) %>%
  group_by(Plot_id, Binomial) %>%
  summarise(Sum_BA = sum(BA)) %>%
  spread(key=Binomial, value = Sum_BA, fill=0)

# tDI = t(DI_PCA)
# 
# colnames(tDI) = paste('plot',tDI[1,],sep = '_')
# mPCA = tDI[-1,]

pca <- prcomp(t(mPCA),scal=TRUE)

plot(pca$x[,1],pca$x[,2])

pca.var <- pca$sdev^2
pca.var.per <- round(pca.var/sum(pca.var)*100, 1)
barplot(pca.var.per, main = "Scree Plot", xlab= "Principal Component", ylab = "Percent Variation")
library(ggplot2)

pca.data <- data.frame(Sample=rownames(pca$x),X=pca$x[,1], Y=pca$x[,2])

pca.data

ggplot(data = pca.data, aes(x=X, y=Y, label=Sample)) +
  geom_text()+
  xlab(paste("PC1 -", pca.var.per[1], "%", sep = " "))+
  ylab(paste("PC2 - ", pca.var.per[2], "%", sep=" ")) +
  theme_bw()+
  ggtitle("PCA Graph for Dipterocarpus Forest")

loading_scores <- pca$rotation[,1]
species_scores <- abs(loading_scores)
species_scores_ranked <- sort(species_scores, decreasing = TRUE)
top_10_species <- names(species_scores_ranked[1:10])
pca$rotation[top_10_species,1]


# Dry_Forest ---------------------------------------------------

PCA_main <- Raw_data %>% select(Forest_Type, Binomial, Plot_id)
Dry_Forest <- PCA_main %>% filter(Forest_Type=="Dry_Forest") %>% 
  select(Binomial, Plot_id)


DI_PCA <- Dry_Forest %>%
  filter(!is.na(Binomial)) %>% 
  group_by(Plot_id, Binomial) %>%
  summarise(Trees = n()) %>%
  spread(key=Binomial, value = Trees, fill=0)

tDI = t(DI_PCA)

colnames(tDI) = paste('plot',tDI[1,],sep = '_')
mPCA = tDI[-1,]

pca <- prcomp(t(mPCA),scal=TRUE)

plot(pca$x[,1],pca$x[,2])

pca.var <- pca$sdev^2
pca.var.per <- round(pca.var/sum(pca.var)*100, 1)
barplot(pca.var.per, main = "Scree Plot", xlab= "Principal Component", ylab = "Percent Variation")
library(ggplot2)

pca.data <- data.frame(Sample=rownames(pca$x),X=pca$x[,1], Y=pca$x[,2])

pca.data

ggplot(data = pca.data, aes(x=X, y=Y, label=Sample)) +
  geom_text()+
  xlab(paste("PC1 -", pca.var.per[1], "%", sep = " "))+
  ylab(paste("PC2 - ", pca.var.per[2], "%", sep=" ")) +
  theme_bw()+
  ggtitle("PCA Graph for Dry Forest")

loading_scores <- pca$rotation[,1]
species_scores <- abs(loading_scores)
species_scores_ranked <- sort(species_scores, decreasing = TRUE)
top_10_species <- names(species_scores_ranked[1:10])
pca$rotation[top_10_species,1]

# Dry_Hill_Forest ----------------------------------------------

PCA_main <- Raw_data %>% select(Forest_Type, Binomial, Plot_id)
Dry_Hill_Forest <- PCA_main %>% filter(Forest_Type=="Dry_Hill_Forest") %>% 
  select(Binomial, Plot_id)


DH_PCA <- Dry_Hill_Forest %>%
  filter(!is.na(Binomial)) %>% 
  group_by(Plot_id, Binomial) %>%
  summarise(Trees = n()) %>%
  spread(key=Binomial, value = Trees, fill=0)

tDH = t(DH_PCA)

colnames(tDH) = paste('plot',tDI[1,],sep = '_')
mPCA = tDH[-1,]

pca <- prcomp(t(mPCA),scal=TRUE)
summary(pca)

plot(pca$x[,1],pca$x[,2])

pca.var <- pca$sdev^2
pca.var.per <- round(pca.var/sum(pca.var)*100, 1)
barplot(pca.var.per, main = "Scree Plot", xlab= "Principal Component", ylab = "Percent Variation")
library(ggplot2)

pca.data <- data.frame(Sample=rownames(pca$x),X=pca$x[,1], Y=pca$x[,2])

pca.data

ggplot(data = pca.data, aes(x=X, y=Y, label=Sample)) +
  geom_text()+
  xlab(paste("PC1 -", pca.var.per[1], "%", sep = " "))+
  ylab(paste("PC2 - ", pca.var.per[2], "%", sep=" ")) +
  theme_bw()+
  ggtitle("PCA Graph for Dry Hill Forest")

loading_scores <- pca$rotation[,1]
species_scores <- abs(loading_scores)
species_scores_ranked <- sort(species_scores, decreasing = TRUE)
top_10_species <- names(species_scores_ranked[1:10])
pca$rotation[top_10_species,1]


# Dry_Mixed_Deciduous_Forest ----------------------------------------------

PCA_main <- Raw_data %>% select(Forest_Type, Binomial, Plot_id)
Dry_Mixed_Deciduous_Forest <- PCA_main %>% filter(Forest_Type=="Dry_Mixed_Deciduous_Forest") %>% 
  select(Binomial, Plot_id)


DMDF_PCA <- Dry_Mixed_Deciduous_Forest %>%
  filter(!is.na(Binomial)) %>% 
  group_by(Plot_id, Binomial) %>%
  summarise(Trees = n()) %>%
  spread(key=Binomial, value = Trees, fill=0)

tDMDF = t(DMDF_PCA)

colnames(tDMDF) = paste('plot',tDMDF[1,],sep = '_')
mPCA = tDMDF[-1,]

pca <- prcomp(t(mPCA),scal=TRUE)

plot(pca$x[,1],pca$x[,2])

pca.var <- pca$sdev^2
pca.var.per <- round(pca.var/sum(pca.var)*100, 1)
barplot(pca.var.per, main = "Scree Plot", xlab= "Principal Component", ylab = "Percent Variation")
library(ggplot2)

pca.data <- data.frame(Sample=rownames(pca$x),X=pca$x[,1], Y=pca$x[,2])

pca.data

ggplot(data = pca.data, aes(x=X, y=Y, label=Sample)) +
  geom_text()+
  xlab(paste("PC1 -", pca.var.per[1], "%", sep = " "))+
  ylab(paste("PC2 - ", pca.var.per[2], "%", sep=" ")) +
  theme_bw()+
  ggtitle("PCA Graph for Dry Mixed Deciduous Forest")

loading_scores <- pca$rotation[,1]
species_scores <- abs(loading_scores)
species_scores_ranked <- sort(species_scores, decreasing = TRUE)
top_10_species <- names(species_scores_ranked[1:10])
pca$rotation[top_10_species,1]


# Moist_Mixed_Deciduous_Forest ----------------------------------------------

PCA_main <- Raw_data %>% select(Forest_Type, Binomial, Plot_id)
Moist_Mixed_Deciduous_Forest <- PCA_main %>% filter(Forest_Type=="Moist_Mixed_Deciduous_Forest") %>% 
  select(Binomial, Plot_id)


MMDF_PCA <- Moist_Mixed_Deciduous_Forest %>%
  filter(!is.na(Binomial)) %>% 
  group_by(Plot_id, Binomial) %>%
  summarise(Trees = n()) %>%
  spread(key=Binomial, value = Trees, fill=0)

# tMMDF = t(MMDF_PCA)
# 
# colnames(tMMDF) = paste('plot',tMMDF[1,],sep = '_')
# mPCA = tMMDF[-1,]

pca <- prcomp(t(mPCA),scal=TRUE)

plot(pca$x[,1],pca$x[,2])

pca.var <- pca$sdev^2
pca.var.per <- round(pca.var/sum(pca.var)*100, 1)
barplot(pca.var.per, main = "Scree Plot", xlab= "Principal Component", ylab = "Percent Variation")
library(ggplot2)

pca.data <- data.frame(Sample=rownames(pca$x),X=pca$x[,1], Y=pca$x[,2])

pca.data

ggplot(data = pca.data, aes(x=X, y=Y, label=Sample)) +
  geom_text()+
  xlab(paste("PC1 -", pca.var.per[1], "%", sep = " "))+
  ylab(paste("PC2 - ", pca.var.per[2], "%", sep=" ")) +
  theme_bw()+
  ggtitle("PCA Graph for Moist Mixed Deciduous Forest")

loading_scores <- pca$rotation[,1]
species_scores <- abs(loading_scores)
species_scores_ranked <- sort(species_scores, decreasing = TRUE)
top_10_species <- names(species_scores_ranked[1:10])
pca$rotation[top_10_species,1]

















