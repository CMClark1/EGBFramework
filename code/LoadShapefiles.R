#############################
# Maritimes Region
# Import shapefiles for map boundaries
# September 2023
# Author: Caira Clark
#############################


library(rgdal)
library(rgeos)
library('maptools')
library(ggpubr)

#Import Strata and NAFO Boundaries

#Importing shape files
setwd("S:/Science/Population Ecology/Georges Bank/Useful R-scripts/Mapping Data")

#File import
# Strata boundaries for Figure 2 (strata outlines)
input <- "MaritimesRegionEcosystemAssessmentStrata(2014-)NAD83" #No extension
SSstrat14.shp <- readOGR (".", input)
names (SSstrat14.shp)
SSstrat14.df <- fortify (SSstrat14.shp, region = "StrataID")
SSstrat14.df2 <-SSstrat14.df
SSstrat14.df2$lat2 <- ifelse (SSstrat14.df2$lat > 48, 48, ifelse (SSstrat14.df2$lat < 40, 40, SSstrat14.df2$lat))
SSstrat14.df2$long2 <- ifelse (SSstrat14.df2$long > -56, -56, ifelse (SSstrat14.df2$long < -69, -69, SSstrat14.df2$long))

input <- "NAFO_SubUnits_CanAtlantic" #No extension
NAFO.shp <- readOGR (".", input)
names (NAFO.shp)
NAFO.df2 <- fortify (NAFO.shp, region = "UnitArea")
NAFO.df2$lat2 <- ifelse (NAFO.df2$lat > 48, 48, ifelse (NAFO.df2$lat < 40, 40, NAFO.df2$lat))
NAFO.df2$long2 <- ifelse (NAFO.df2$long > -56.0, -56.0, ifelse (NAFO.df2$long < -69.0, -69.0, NAFO.df2$long))

input <- "Can-USBorder" #No extension
Border.shp <- readOGR (".", input)
names (Border.shp)
Border.df <- fortify (Border.shp, region = "BoundID")
Border.df2 <- Border.df [order (Border.df$lat),] 
Border.df2$lat2 <- ifelse (Border.df2$lat > 48, 48, ifelse (Border.df2$lat < 40, 40, Border.df2$lat))
Border.df2$long2 <- ifelse (Border.df2$long > -56, -56, ifelse (Border.df2$long < -69, -69, Border.df2$long))
Border.df3 <- subset(Border.df2, Border.df2[ , 2] > 40.5) 
Border.df3$long2 <- ifelse (Border.df3$long2 > -66.0, -66.146, Border.df3$long2)