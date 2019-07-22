# Load the Raw data file
rm(list = ls())
library(plyr)
library(tidyverse)
library(vegan)
library(ape)

# PSPAnalysis-----------------------

The_PSP <- read_csv("Data/PSP_data.csv")

The_PSP <- The_PSP %>% mutate(Species_names = str_replace(Binomial, " ", "_"))

Tenure <- unique(The_PSP[,c("Plot", "land_tenure")]) %>%
  arrange(Plot) %>%
  mutate(land_tenure = as.factor(land_tenure))

Tenure$colour <- mapvalues(Tenure$land_tenure, 
                           levels(Tenure$land_tenure),
                           c("red", "green"))


Data <- The_PSP %>%
  filter(!is.na(Species_names)) %>%
  group_by(Plot, Species_names)%>%
  dplyr::summarise(Tree = n()) %>%
  spread(key=Species_names, value = Tree, fill=0)

pcoa.object <-  vegdist(Data[,-1]) %>% pcoa()

# biplot(pcoa.object$vectors$Axis.1, col=forest.type$group)

biplot.dat <- data.frame(comp1 = pcoa.object$vectors[,1],
                         comp2 = pcoa.object$vectors[,2],
                         Tenure = Tenure$land_tenure)

ggplot(biplot.dat, aes(comp1, comp2, color = Tenure)) +
  geom_point(alpha = 1, size = 5)+
  xlab("PC1 - Percent Variation Explained")+ 
  ylab("PC2 - Percent Variation Explained")+
  theme_bw()+
  theme(text = element_text(size=15),axis.text.x  = element_text(angle=0, hjust=2),
        axis.title.x = element_text(margin = margin(t = 18, r = 0, b = 0, l = 0)),
        axis.title.y = element_text(margin = margin(t = 0, r = 18, b = 0, l = 0)))




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
  ggtitle("MDS plot using Euclidean distance")+
  

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

# Combined forests PCOA ---------------------------------------------------------------------------

Raw_data <- read.csv("Data/All_data.csv")
str(Raw_data)
summary(Raw_data)
nrow(Raw_data)

forest.type <- unique(Raw_data[,c(2,1)])

forest.type$Plot_id <- 1:nrow(forest.type)

unique(forest.type$Forest_Type)

forest.type$colour <- plyr::mapvalues(forest.type$Forest_type, 
                                levels(as.factor(forest.type$Forest_type)),
                                c("red", "blue", "#000000", "green", "brown"))



# basal.area.fn <- function(x){ (pi*(x)^2)/40000 } # calculate basal area in m^2
# Raw_data <- mutate(Raw_data, BA=basal.area.fn(Raw_data$Dbh_cm))
# Transforming the data.frame into matrix form

Data <- Raw_data %>%
  filter(!is.na(Species_names)) %>%
  filter(!duplicated(Plot_id)) %>% 
  group_by(Plot_id, Mean_precipation, Mean_temperature,Species_names) %>%
  dplyr::summarise(Tree = n()) %>%
  spread(key=Species_names, value = Tree, fill=0)

# Data$test1 <- Raw_data$Mean_precipation
# mutaye(Data, Test = Raw_data$Mean_precipation )

pcoa.object <-  vegdist(Data[,-1]) %>% pcoa()

# biplot(pcoa.object$vectors$Axis.1, col=forest.type$group)

biplot.dat <- data.frame(comp1 = pcoa.object$vectors[,1],
                         comp2 = pcoa.object$vectors[,2],
                         Forest = forest.type$Forest_type)



ggplot(biplot.dat, aes(comp1, comp2, color =Forest)) +
  geom_point(alpha = 0.5)+ ggtitle ("Principal Component Analysis (PCoA-PC1 Vs PC2)")+
  xlab("PC1 - Percent Variation Explained")+ 
  ylab("PC2 - Percent Variation Explained")+
  theme_bw()

