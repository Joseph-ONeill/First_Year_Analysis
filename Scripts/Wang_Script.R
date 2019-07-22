
file = "D:/CambridgeWork/17_Prediction/xxxx.csv"
mydata_beta <- read.csv(file, header=TRUE, sep=",")

names(mydata_beta)

# get columns with xy, site ID, and species data
sppData_beta <- mydata_beta[, c("Species", "Site", "Northing", "Easting")]

envTab_beta <- mydata_beta[, c(2,6:ncol(mydata_beta))]


# Beta------------------------------------------------------------------------------

library(betapart)
## distance matrix
mad.spp.dist <-beta.pair(myabundance.matrix, index.family="sorensen")

SIM <- mad.spp.dist$beta.sim
SNE <- mad.spp.dist$beta.sne
SOR <- mad.spp.dist$beta.sor


# store computed indices in a new data frame called 'indices'
indices <- mydata[,1:13]
indices$Richness <- specnumber(myabundance.matrix) ## Species richness (S), rowSums(BCI > 0) does the same...
indices$Shannon <- diversity(myabundance.matrix) # shannon is default
indices$Evenness <- indices$Shannon/log(indices$Richness) #Pielou's evenness J
indices$Rarefied <- c(rarefy(myabundance.matrix[1:179,], sample=15))