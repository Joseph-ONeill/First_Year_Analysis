rm(list = ls())
# installation of the package from CRAN
install.packages("gdm")
# installation from GitHub
install.packages("devtools")
#library(devtools)
install_github("fitzLab-AL/GDM")
library(gdm)
# reads in example input data
load(system.file("./data/gdm.RData", package="gdm"))
# columns 3-7 are soils variables, remainder are climate 
gdmExpData[1:3,]

# get columns with xy, site ID, and species data
sppTab <- gdmExpData[, c("species", "site", "Lat", "Long")]

# get columns with env. data and xy-coordinates
envTab <- gdmExpData[, c(2:ncol(gdmExpData))]

# x-y species list example
gdmTab <- formatsitepair(sppTab, bioFormat=2, XColumn="Long", YColumn="Lat", sppColumn="species", siteColumn="site", predData=envTab)
gdmTab[1:3,]

# Biological distance matrix example
dim(gdmDissim)

gdmDissim[1:5, 1:5]

site <- unique(sppTab$site) 
gdmDissim <- cbind(site, gdmDissim)

gdmTab.dis <- formatsitepair(gdmDissim, bioFormat=3, XColumn="Long", YColumn="Lat", predData=envTab, siteColumn="site")

gdm.1 <- gdm(gdmTab, geo=T)

# get idea of number of panels
length(gdm.1$predictors)

plot(gdm.1, plot.layout=c(4,3))

# To allow easy customization of I-spline plots, the isplineExtract function will extract the plotted values for
#each I-spline.

gdm.1.splineDat <- isplineExtract(gdm.1) 
str(gdm.1.splineDat)

plot(gdm.1.splineDat$x[,"Geographic"], gdm.1.splineDat$y[,"Geographic"], lwd=3, type="l", xlab="Geographic distance", ylab="Partial ecological distance")

gdm.1.pred <- predict(gdm.1, gdmTab) 
head(gdm.1.pred)

plot(gdmTab$distance, gdm.1.pred, xlab="Observed dissimilarity",
     ylab="Predicted dissimilarity", xlim=c(0,1), ylim=c(0,1), pch=20, col=rgb(0,0,1,0.5))+ lines(c(-1,2), c(-1,2))

# reordering environmental data to create table for transformation 
envTrans <- envTab[, c(13,12,2:11)] # same order as gdmTab
envTrans[1:3,]

tabTrans <- gdm.transform(gdm.1, envTrans) # now scaled to biological importance 
tabTrans[1:3,]

