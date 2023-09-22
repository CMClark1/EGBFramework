#############################
# Maritimes Region
# Import shapefiles for map boundaries
# September 2023
# Author: Caira Clark
#############################

#Use this as part of the script you're working on to run this file 
#source("S:/Science/Population Ecology/Georges Bank/Useful R-scripts/LoadShapefiles.R")

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
SSstrat14.df2$long2 <- ifelse (SSstrat14.df2$long > -56, -56, ifelse (SSstrat14.df2$long < -71, -71, SSstrat14.df2$long))

input <- "NAFO_SubUnits_CanAtlantic" #No extension
NAFO.shp <- readOGR (".", input)
names (NAFO.shp)
NAFO.df2 <- fortify (NAFO.shp, region = "UnitArea")
NAFO.df2$lat2 <- ifelse (NAFO.df2$lat > 48, 48, ifelse (NAFO.df2$lat < 40, 40, NAFO.df2$lat))
NAFO.df2$long2 <- ifelse (NAFO.df2$long > -56.0, -56.0, ifelse (NAFO.df2$long < -71.0, -71.0, NAFO.df2$long))

input <- "Can-USBorder" #No extension
Border.shp <- readOGR (".", input)
names (Border.shp)
Border.df <- fortify (Border.shp, region = "BoundID")
Border.df2 <- Border.df [order (Border.df$lat),] 
Border.df2$lat2 <- ifelse (Border.df2$lat > 48, 48, ifelse (Border.df2$lat < 40, 40, Border.df2$lat))
Border.df2$long2 <- ifelse (Border.df2$long > -56, -56, ifelse (Border.df2$long < -71, -71, Border.df2$long))
Border.df3 <- subset(Border.df2, Border.df2[ , 2] > 40.5) 
Border.df3$long2 <- ifelse (Border.df3$long2 > -66.0, -66.146, Border.df3$long2)

#Example of how to use these for a map
#map1<-
  #ggplot(data, aes(x=LON, y=LAT)) +
  #geom_polygon (data = SSstrat14.df2, aes (x = long2, y = lat2, group = group), colour = "black", fill=NA, linewidth = 0.4) +
  #geom_polygon (data = NAFO.df2, aes (x = long2, y = lat2, group = group), colour = "black", fill = NA, linewidth = 0.4) +
  #geom_path (data = Border.df2, aes (x = long2, y = lat2), colour = "black", linetype = "dashed", linewidth = 0.8) +
  #coord_map () + 
  #theme_bw () + 
  #theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank()) + 
  #scale_y_continuous ("Latitude", expand = c (0,0), limits = c (40, 48)) + 
  #scale_x_continuous ( "Longitude", expand = c (0,0), limits = c (-71, -56)) +
  #theme (axis.title = element_text (size = 9), 
         #axis.text = element_text (size = 8), 
         #legend.text = element_text (size = 8), 
         #legend.title = element_text (size = 9))