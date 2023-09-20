####################
# 5Z Cod Survey Timing
# Caira Clark, Jessie McIntyre, based on script from Dan Ricard
# 16 August 2023
####################

#Load packages
require(ggplot2)
require(sqldf)
library(rgdal)
library(rgeos)
library(tidyverse)
library(tidyr)
library(plyr)
library(dplyr)
library(shapefiles)
require(lubridate)
require(maptools)
require(mapplots)
require(raster)
require(reshape2)
require(mapproj)
library(readxl)
library(ggpubr)
library(stringr)
library(ROracle)
library(here)


channel <- ROracle::dbConnect(DBI::dbDriver("Oracle"), username=oracle.username, password=oracle.password, oracle.dsn) 

# Load survey data - cod only

gsweight <- ROracle::dbGetQuery(channel, "
                             select a.MISSION, a.SETNO, a.SDATE, a.STRAT, a.SLAT, a.SLONG, a.DIST, a.DUR,
                             b.totwgt, b.spec, b.sampwgt, b.size_class
                             from groundfish.gsinf a, groundfish.gscat b, groundfish.gsmissions c
                             where a.type=1
                             and b.spec=10
                             and c.season in ('SPRING')
                             and c.year between ('1986') and ('2023')
                             and a.strat in ('5Z1', '5Z2', '5Z3', '5Z4', '5Z5', '5Z6', '5Z7', '5Z8', '5Z9')
                             and a.mission=b.mission
                             and b.mission=c.mission
                             and a.setno=b.setno")

##PART 1. SETS BY YEAR GROUP (10 YEARS) WITH POINTS---------------------------


## Format dataframe for catch info
gsweight$year <- year(gsweight$SDATE)
gsweight$month <- month (gsweight$SDATE)
gsweight$MISSIONSET <- paste(gsweight$MISSION, gsweight$SETNO, sep="_") #Create a "missionset" column to have a unique identifier for each set
gsweight$std.WGT <- gsweight$TOTWGT * (1.75/SSmap$DIST) # Standardize weight for distance of tow (1.75nm)
gsweight$SLAT <- (as.numeric(substr(gsweight$SLAT,1,2))+(gsweight$SLAT - as.numeric(substr(gsweight$SLAT,1,2))*100)/60)
gsweight$SLONG <- (as.numeric(substr(gsweight$SLONG,1,2))+(gsweight$SLONG - as.numeric(substr(gsweight$SLONG,1,2))*100)/60)*-1
SSmap <- gsweight
t<-SSmap

mapWGT <- subset(gsweight, select = c ("year", "SPEC", "STRAT", "MISSIONSET","std.WGT", "SLAT", "SLONG"))

# Weight Mean (mean weight per set) column like in STRANAL
mapWGT1 <- plyr::ddply (mapWGT,. (year, SPEC, STRAT, MISSIONSET), summarize, LAT = mean (SLAT),
                        LON = mean (SLONG), WGT = sum (std.WGT))

## Make df with only desired year(s)
mapWGT.yr<-mapWGT1 %>% filter(year>1986 & year<2024)

# Add 0 data for any missing years/stratum 
# list of all years (regardless of species) with all species (regardless of years observed)
setnos <- expand.grid(SPEC = unique (mapWGT.yr$SPEC),
                      MISSIONSET = unique (mapWGT.yr$MISSIONSET),
                      year = unique (mapWGT.yr$year))
# Add to annual WGT1 with NA's
mapWGT2 <- merge (setnos, mapWGT.yr, all = TRUE)
# replace NA's with 0
mapWGT2$WGT [is.na (mapWGT2$WGT)] <- 0

# Determine lat/lon of tows
towloc <- subset (mapWGT2, select = c ("MISSIONSET", "LAT", "LON"))
towloc <- na.omit (towloc)
towloc<-unique(towloc)
towloc2 <- expand.grid (SPEC = unique (mapWGT.yr$SPEC),
                        MISSIONSET = unique (mapWGT.yr$MISSIONSET)
)
towloc3<-merge(towloc2,towloc,all.x=T)

# Add tow locations to weight file
mapWGT3 <- merge ( towloc3, mapWGT2, all.x = T)
mapWGT3 <- subset (mapWGT3, select = c ( "SPEC", "MISSIONSET", "WGT", "LAT", "LON"))
mapWGT3$WGT [is.na (mapWGT3$WGT)] <- 0

# Weights by group (for plotting)
mapWGT3$CAT <- ifelse (mapWGT3$WGT == 0, "0", NA)
mapWGT3$CAT <- ifelse (mapWGT3$WGT > 0, ifelse (mapWGT3$WGT < 10, "1-10", mapWGT3$CAT), mapWGT3$CAT)
mapWGT3$CAT <- ifelse (mapWGT3$WGT > 9.999999, ifelse (mapWGT3$WGT < 50, "10-50", mapWGT3$CAT), mapWGT3$CAT)
mapWGT3$CAT <- ifelse (mapWGT3$WGT > 49.999999, ifelse (mapWGT3$WGT < 100, "50-100", mapWGT3$CAT), mapWGT3$CAT)
mapWGT3$CAT <- ifelse (mapWGT3$WGT > 99.999999, "100+", mapWGT3$CAT)
# Order factor levels
mapWGT3$CAT <- factor (mapWGT3$CAT, levels = c ("0", "1-10", "10-50", "50-100", "100+"))

# Add rows with NA's so all species have all categories of size class (so legends remain consistent between plots)
cats <- expand.grid (SPEC = unique (mapWGT3$SPEC),
                     CAT = unique (mapWGT3$CAT))
# Add to mapWGT3 with NA's
mapWGT4 <- merge (cats, mapWGT3, all = TRUE)

#Importing shape files
setwd("S:/Science/Population Ecology/Georges Bank/Useful R-scripts/Mapping Data")
# Strata boundaries
library(rgdal)
library(rgeos)
library('maptools')


#File import
# Strata boundaries for Figure 2 (strata outlines)
input <- "MaritimesRegionEcosystemAssessmentStrata(2014-)NAD83" #No extension
SSstrat14.shp <- readOGR (".", input)
names (SSstrat14.shp)
SSstrat14.df <- fortify (SSstrat14.shp, region = "StrataID")
SSstrat14.df2 <-SSstrat14.df
SSstrat14.df2$lat2 <- ifelse (SSstrat14.df2$lat > 44, 44, ifelse (SSstrat14.df2$lat < 40, 40, SSstrat14.df2$lat))
SSstrat14.df2$long2 <- ifelse (SSstrat14.df2$long > -65, -65, ifelse (SSstrat14.df2$long < -71, -71, SSstrat14.df2$long))

input <- "NAFO_SubUnits_CanAtlantic" #No extension
NAFO.shp <- readOGR (".", input)
names (NAFO.shp)
NAFO.df2 <- fortify (NAFO.shp, region = "UnitArea")
NAFO.df2$lat2 <- ifelse (NAFO.df2$lat > 44, 44, ifelse (NAFO.df2$lat < 40, 40, NAFO.df2$lat))
NAFO.df2$long2 <- ifelse (NAFO.df2$long > -65.0, -65.0, ifelse (NAFO.df2$long < -71.0, -71.0, NAFO.df2$long))

input <- "Can-USBorder" #No extension
Border.shp <- readOGR (".", input)
names (Border.shp)
Border.df <- fortify (Border.shp, region = "BoundID")
Border.df2 <- Border.df [order (Border.df$lat),] 
Border.df2$lat2 <- ifelse (Border.df2$lat > 44, 44, ifelse (Border.df2$lat < 40, 40, Border.df2$lat))
Border.df2$long2 <- ifelse (Border.df2$long > -65, -65, ifelse (Border.df2$long < -71, -71, Border.df2$long))
Border.df3 <- subset(Border.df2, Border.df2[ , 2] > 40.5) 
# Had to calculate slope of line to get long at 41 lat
# m = -1.152244595; b = -35.21634559
Border.df3$long2 <- ifelse (Border.df3$long2 > -66.0, -66.146, Border.df3$long2)

# setting shape type and size for geom_point
shapes <- c( 3, 16, 16, 16, 16)
sizes <- c (1, 1, 2, 3, 4)

mapSPEC <- mapWGT4
mapSPEC$YEAR <- substr(mapSPEC$MISSIONSET, 4, 7)

mapSPEC <- mapSPEC %>% mutate(
  year_group = dplyr::case_when(
    YEAR <= 1995            ~ "1986-1995",
    YEAR > 1995 & YEAR <= 2005 ~ "1996-2005",
    YEAR > 2005 & YEAR <= 2015 ~ "2006-2015",
    YEAR > 2015  ~ "2016+"))

map<-
  ggplot () +
  geom_polygon (data = SSstrat14.df2, aes (x = long2, y = lat2, group = group), colour = "dodgerblue1", fill = NA, size = 0.4) +
  geom_polygon (data = NAFO.df2, aes (x = long2, y = lat2, group = group), colour = "black", fill = NA, size = 0.4) +
  geom_path (data = Border.df2, aes (x = long2, y = lat2), colour = "black", linetype = "dashed", linewidth = 0.8) +
  coord_map () + 
  theme_bw () + 
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank()) + 
  scale_y_continuous ("Latitude", expand = c (0,0), limits = c (40, 43)) + 
  scale_x_continuous ( "Longitude", expand = c (0,0), limits = c (-72, -65)) +
  theme (axis.title = element_text (size = 9), 
         axis.text = element_text (size = 8), 
         legend.text = element_text (size = 8), 
         legend.title = element_text (size = 9)) 

map +
  geom_point (data = mapSPEC, aes (x = LON, y = LAT, size = CAT, shape = CAT)) +
  scale_shape_manual (values = shapes, drop = FALSE) + scale_size_manual (values = sizes, drop = FALSE) +
  theme (legend.position = "right") +
  labs (shape="Catch (kg/tow)", size = "Catch (kg/tow)") +
  facet_wrap(year_group~.)

setwd("~/LocalRespository/EGBFramework")
ggsave(here("figures/Survey_DFO_SetsPoints.png"), width = 7, height = 6, units="in")

##PART 2. CATCH BY STRATUM ----------------------

###Rework Dan Ricard's code (https://github.com/dfo-gulf-science/Maritimes-SUMMER-Atlas) to apply to 5Z spatial units

##Setting up function
#################
qu <- paste("
SELECT
mission,
setno,
strat,
sdate,
TO_CHAR(sdate,'yyyy') YEAR,
TO_CHAR(sdate,'mm') MONTH,
TO_CHAR(sdate,'dd') DAY,
dmin,
dmax,
bottom_temperature,
bottom_salinity,
dist,
gear,
area as ManArea,
-1*ROUND(TRUNC(SLONG/100)+MOD(SLONG,100)/60,5) SLO,
ROUND(TRUNC(SLAT/100)+MOD(SLAT,100)/60,5) SLA
FROM groundfish.gsinf
where
type in (1)
AND extract(year from sdate) >= 1986
order by YEAR, mission, setno
", sep="")

tows.df<-ROracle::dbGetQuery(channel, qu)
tows.df$depth <- (tows.df$DMIN + tows.df$DMAX) /2
tows.df$decade <- floor(as.numeric(tows.df$YEAR)/10)
tows.df$MONTH<-as.numeric(tows.df$MONTH)
tows.df<-subset(tows.df, MONTH%in%c(6,7,8))
tows.df$MANAREA<-as.numeric(tows.df$MANAREA)

## strata statistics
qu <- paste("
select
*
from
groundfish.gsstratum
", sep="")

strata.stats.df<-ROracle::dbGetQuery(channel, qu)

all.df <- merge(tows.df, strata.stats.df, by="STRAT")
agg.all.df <- aggregate(SETNO~STRAT+AREA+YEAR, data=all.df, length)
t.df<-agg.all.df[agg.all.df$SETNO>=2,]
surveyed.df <- aggregate(AREA~YEAR, data=t.df, sum)
## area surveyed, each stratum is counted in if at least 2 successful tows were done in a year

##

#Writing General function for a given species:
#################
extract.catch.fct <- function(spec.num) {
  # survey data
  ## bring back a data frame with tows and their associated environmental covariates
  qu <- paste("
SELECT
mission,
setno,
strat,
sdate,
TO_CHAR(sdate,'yyyy') YEAR,
TO_CHAR(sdate,'mm') MONTH,
TO_CHAR(sdate,'dd') DAY,
dmin,
dmax,
bottom_temperature,
bottom_salinity,
dist,
gear,
area as MANAREA,
-1*ROUND(TRUNC(SLONG/100)+MOD(SLONG,100)/60,5) SLO,
ROUND(TRUNC(SLAT/100)+MOD(SLAT,100)/60,5) SLA
FROM groundfish.gsinf
where
type = 1
order by YEAR, mission, setno
", sep="")
  
  #type in (1,5)
  
  #tows.df <- sqlQuery(channel, qu)
  tows.df<-ROracle::dbGetQuery(channel, qu)
  tows.df$depth <- (tows.df$DMIN + tows.df$DMAX) /2
  tows.df$decade <- floor(as.numeric(tows.df$YEAR)/10)
  tows.df$MONTH<-as.numeric(tows.df$MONTH)
  tows.df<-subset(tows.df, MONTH%in%c(2,3,4))
  tows.df$MANAREA<-as.numeric(tows.df$MANAREA)
  
  
  qu <- paste("
SELECT
i.mission,
i.setno,
i.strat,
i.sdate,
TO_CHAR(i.sdate,'yyyy') YEAR,
TO_CHAR(i.sdate,'mm') MONTH,
TO_CHAR(i.sdate,'dd') DAY,
c.spec,
s.CODE SCIEN,
s.comm,
c.totno,
c.totwgt,
i.dmin,
i.dmax,
i.bottom_temperature,
i.bottom_salinity,
i.dist,
i.gear,
c.totno * (1.75/i.dist) as totnocorr,
c.totwgt * (1.75/i.dist) as totwgtcorr
FROM
groundfish.gsinf i,
groundfish.gscat c,
groundfish.GSSPECIES s
where
i.type = 1 AND
i.mission = c.mission AND
i.setno = c.setno AND
s.CODE=c.spec and
s.CODE='",spec.num,"'
order by YEAR, i.mission, i.setno
", sep="")
  
  #i.type in (1,5)
  
  
  #spec.df <- sqlQuery(channel, qu)
  spec.df<-ROracle::dbGetQuery(channel, qu)
  spec.df$MONTH<-as.numeric(spec.df$MONTH)
  merged.df <- merge(spec.df, tows.df, all.y=TRUE) #Removing the merge id column names precludes the need to then remove the duplicated variables.
  merged.df[is.na(merged.df$SPEC),]$SPEC <- spec.num
  
  merged.df[is.na(merged.df$COMM),]$COMM <- unique(merged.df[!is.na(merged.df$COMM),]$COMM)
  merged.df[is.na(merged.df$SCIEN),]$SCIEN <- unique(merged.df[!is.na(merged.df$SCIEN),]$SCIEN)
  
  merged.df[is.na(merged.df$TOTNO),]$TOTNO <- 0
  merged.df[is.na(merged.df$TOTWGT),]$TOTWGT <- 0
  merged.df[is.na(merged.df$TOTNOCORR),]$TOTNOCORR <- 0
  merged.df[is.na(merged.df$TOTWGTCORR),]$TOTWGTCORR <- 0
  
  #names(merged.df) <- c("mission","setno","spec","scien","comm","totno","totwgt","totno.corr","totwgt.corr","Strata","YEAR","month","day","dmin","dmax","temperature","salinity","lon","lat","DEPTH","decade","dist")
  names(merged.df)<-tolower(names(merged.df))
  
  tt <- droplevels(merged.df)
  merged.df <- tt
  merged.df$unique.id <- paste0(merged.df$mission, "-", merged.df$setno)
  
  #Sorting out sets with two size classes:
  tot_wgt_sets<-aggregate(totwgtcorr~unique.id, merged.df, FUN = "sum")
  tot_no_sets<-aggregate(totnocorr~unique.id, merged.df, FUN = "sum")
  merged.df<-unique(merged.df[,-((which(colnames(merged.df)=="totno")):(which(colnames(merged.df)=="totwgtcorr")))])
  merged.df<-merge(merged.df, tot_wgt_sets, all.x=TRUE)
  merged.df<-merge(merged.df, tot_no_sets, all.x=TRUE)
  
  return(merged.df)
  
} ## end function definition
############

#Application:
############
pollock.df<-extract.catch.fct(10) #this is the species code for cod
##############

Assessment_strata<-'5Z' #specify spatial area from list below (just have one for 5Z here)

if(Assessment_strata=='5Z'){pollock.df<-subset(pollock.df, strat %in% c('5Z1','5Z2', '5Z3', '5Z4', '5Z5', '5Z6', '5Z7', '5Z8', '5Z9'))}

#Painting Strata by catch density

pollock.df5<-aggregate(totwgtcorr~year+strat, pollock.df, FUN="mean") # year strat totwgtcorr 
pollock.df7<-aggregate(totwgtcorr~year, pollock.df5, FUN="sum") #year tot_wt_strat #This is wrong. Size of stratum is not accounted for in the sum.
names(pollock.df7)<-c('year','total_wt_strat')
pollock.df8<-merge(pollock.df5, pollock.df7)
pollock.df8$PropStrat<-with(pollock.df8, totwgtcorr/total_wt_strat) #PropStrata=totwgtcorr/total_wt_strat

ss_basins<-aggregate(PropStrat~year, subset(pollock.df8, strat%in%c('5Z1','5Z2', '5Z3', '5Z4', '5Z5', '5Z6', '5Z7', '5Z8', '5Z9')&!year%in%c(1986:1995)), FUN="sum")
ss_basins$spat<-"Strat5Z19"
gchannel<-aggregate(PropStrat~year, subset(pollock.df8, strat%in%c('5Z1','5Z2', '5Z3', '5Z4', '5Z5', '5Z6', '5Z7', '5Z8', '5Z9')), FUN="sum")
gchannel$spat<-"Strat5Z19"
combined<-rbind(ss_basins, gchannel)
combined$year<-as.numeric(combined$year)

##APPLY TO CREATE MAP

#Added figure with shaded strata boundaries:
mapWGT.yr<-mapWGT1 %>% 
  filter(year>1985 & year<1996)

# Add 0 data for any missing years/stratum 
# list of all years (regardless of species) with all species (regardless of years observed)
setnos <- expand.grid (SPEC = unique (mapWGT.yr$SPEC),
                       MISSIONSET = unique (mapWGT.yr$MISSIONSET),
                       year = unique (mapWGT.yr$year))
# Add to annualWGT1 with NA's
mapWGT2 <- merge (setnos, mapWGT.yr, all = TRUE)

# replace NA's with 0
mapWGT2$WGT [is.na (mapWGT2$WGT)] <- 0

# Determine lat/lon of tows
towloc <- subset (mapWGT2, select = c ("MISSIONSET", "LAT", "LON"))
towloc <- na.omit (towloc)
towloc<-unique(towloc)
towloc2 <- expand.grid (SPEC = unique (mapWGT.yr$SPEC),
                        MISSIONSET = unique (mapWGT.yr$MISSIONSET)
)
towloc3<-merge(towloc2,towloc,all.x=T)

# Add tow locations to weight file
mapWGT3 <- merge ( towloc3, mapWGT2, all.x = T)
mapWGT3 <- subset (mapWGT3, select = c ( "SPEC", "MISSIONSET", "WGT", "LAT", "LON"))
mapWGT3$WGT [is.na (mapWGT3$WGT)] <- 0

# Weights by group (for plotting)
mapWGT3$CAT <- ifelse (mapWGT3$WGT == 0, "0", NA)
mapWGT3$CAT <- ifelse (mapWGT3$WGT > 0, ifelse (mapWGT3$WGT < 10, "1-10", mapWGT3$CAT), mapWGT3$CAT)
mapWGT3$CAT <- ifelse (mapWGT3$WGT > 9.999999, ifelse (mapWGT3$WGT < 50, "10-50", mapWGT3$CAT), mapWGT3$CAT)
mapWGT3$CAT <- ifelse (mapWGT3$WGT > 49.999999, ifelse (mapWGT3$WGT < 100, "50-100", mapWGT3$CAT), mapWGT3$CAT)
mapWGT3$CAT <- ifelse (mapWGT3$WGT > 99.999999, "100+", mapWGT3$CAT)

# Order factor levels
mapWGT3$CAT <- factor (mapWGT3$CAT, levels = c ("0", "1-10", "10-50", "50-100", "100+"))

# GPS converter Deg min sec to degrees decimal
mapWGT3$Lat  = with (mapWGT3, trunc (LAT/100) + 
                       (LAT%%100)/60)
mapWGT3$Lon = (with (mapWGT3, trunc (LON/100) + 
                       (LON%%100)/60)*-1)

# Add rows with NA's so all species have all categories of size class (so legends remain consistent between plots)
cats <- expand.grid (SPEC = unique (mapWGT3$SPEC),
                     CAT = unique (mapWGT3$CAT))

# Add to mapWGT3 with NA's
mapSPEC <- merge (cats, mapWGT3, all = TRUE)

#Adding shading to strata. Uses object called combined, from DistributionSurveyDataPull.R. 4X5Y Cod proportion of 
mean2008_2018<-aggregate(PropStrat~strat, subset(pollock.df8, year%in%c(seq(1986, 1995,1))), FUN="mean") #generate mean fill for a range of years
names(mean2008_2018)[1]<-'id' #rename strat to match the shapefile

#In the shape file, strata 5Z3 and 5Z4 are parcelled out into smaller units. Doing this for the mean file as well to ensure a proper merge
missingvalues<-data.frame(id=c("5Z31","5Z32", "5Z33", "5Z34", "5Z41", "5Z42"), PropStrat=c(rep(mean2008_2018[mean2008_2018$id=='5Z3',2], 4), rep(mean2008_2018[mean2008_2018$id=='5Z4',2], 2)))
mean2008_2018<-rbind(mean2008_2018, missingvalues)
names(mean2008_2018)[1]<-'id' #rename strat to match the shapefile

#Start merging, but without messing up the order
temp<-subset(SSstrat14.df2, select=c('id'))
temp<-merge(temp, mean2008_2018, all.x=TRUE)
names(temp)[1]<-"id2"
SSstrat14.df2_fil<-cbind(SSstrat14.df2, temp)
#end merge

# setting shape type and size for geom_point
shapes <- c( 3, 16, 16, 16, 16)
sizes <- c (2, 2, 3, 4, 5)

catchdensity <- ggplot () +
  geom_polygon (data = SSstrat14.df2_fil, aes (x = long2, y = lat2, group = group, fill=PropStrat), colour = "dodgerblue1", size = 0.4) +
  scale_fill_gradient(low = "white", high = "dodgerblue1", na.value="white") +
  geom_polygon (data = NAFO.df2, aes (x = long2, y = lat2, group = group), colour = "black", fill = NA, size = 0.4) +
  geom_path (data = Border.df2, aes (x = long2, y = lat2), colour = "black", linetype = "dashed", size = 0.8) +
  #geom_point (data = mapSPEC, aes (x = Lon, y = Lat, size=CAT, shape = CAT)) +
  scale_shape_manual (values = shapes, drop = FALSE) + scale_size_manual (values = sizes, drop = FALSE)+
  coord_map () + 
  theme_bw () + 
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank()) + 
  scale_y_continuous ("Latitude", expand = c (0,0), limits = c (40, 43)) + 
  scale_x_continuous ( "Longitude", expand = c (0,0), limits = c (-71, -65)) +
  theme (axis.title = element_text (size = 9), 
         axis.text = element_text (size = 8), 
         legend.text = element_text (size = 8), 
         legend.title = element_text (size = 9)) + 
  theme (legend.position = "right") +
  #labs (shape="Catch (kg/tow)", size = "Catch (kg/tow)")+
  labs (shape="kg/trait", size = "kg/trait")
catchdensity

plot1 <- catchdensity+ggtitle("1986-1995")
plot2 <- catchdensity+ggtitle("1996-2005")
plot3 <- catchdensity+ggtitle("2006-2015")
plot4 <- catchdensity+ggtitle("2016+")

library(ggpubr)
ggarrange(plot1, plot2, plot3, plot4, ncol=1, nrow=4, common.legend = TRUE, legend="bottom")

ggsave(here("figures/Survey_DFO_CatchDensity.png"), width = 4, height = 12, units="in")

#PART 3. SET DENSITY---------------

#Application:

pollock.df<-extract.catch.fct(10) #this is the species code for cod

Assessment_strata<-'5Z' #specify spatial area from list below (just have one for 5Z here)

if(Assessment_strata=='5Z'){pollock.df<-subset(pollock.df, strat %in% c('5Z1','5Z2', '5Z3', '5Z4', '5Z5', '5Z6', '5Z7', '5Z8', '5Z9'))}

#Painting Strata by set density

pollock.df5 <- pollock.df %>% group_by(year, strat) %>% dplyr::summarise(sets=n()) #year, strat, sets per strata
pollock.df7 <- pollock.df %>% group_by(year) %>% dplyr::summarise(sets=n()) #year, sets per year
names(pollock.df7)<-c('year', 'total_sets')
pollock.df8 <- merge(pollock.df5, pollock.df7)
pollock.df8$PropStrat <- pollock.df8$sets/pollock.df8$total_sets

ss_basins<-aggregate(PropStrat~year, subset(test3, strat%in%c('5Z1','5Z2', '5Z3', '5Z4', '5Z5', '5Z6', '5Z7', '5Z8', '5Z9')&!year%in%c(2016:2023)), FUN="sum")
ss_basins$spat<-"Strat5Z19"
gchannel<-aggregate(PropStrat~year, subset(test3, strat%in%c('5Z1','5Z2', '5Z3', '5Z4', '5Z5', '5Z6', '5Z7', '5Z8', '5Z9')), FUN="sum")
gchannel$spat<-"Strat5Z19"
combined<-rbind(ss_basins, gchannel)
combined$year<-as.numeric(combined$year)

##APPLY TO CREATE MAP

#Added figure with shaded strata boundaries:

#Adding shading to strata. Uses object called combined, from DistributionSurveyDataPull.R. 4X5Y Cod proportion of 
mean2008_2018<-aggregate(PropStrat~strat, subset(pollock.df8, year%in%c(seq(2016, 2023,1))), FUN="mean") #generate mean fill for a range of years
names(mean2008_2018)[1]<-'id' #rename strat to match the shapefile

#In the shape file, strata 5Z3 and 5Z4 are parcelled out into smaller units. Doing this for the mean file as well to ensure a proper merge
missingvalues<-data.frame(id=c("5Z31","5Z32", "5Z33", "5Z34", "5Z41", "5Z42"), PropStrat=c(rep(mean2008_2018[mean2008_2018$id=='5Z3',2], 4), rep(mean2008_2018[mean2008_2018$id=='5Z4',2], 2)))
mean2008_2018<-rbind(mean2008_2018, missingvalues)
names(mean2008_2018)[1]<-'id' #rename strat to match the shapefile

#Start merging, but without messing up the order
temp<-subset(SSstrat14.df2, select=c('id'))
temp<-merge(temp, mean2008_2018, all.x=TRUE)
temp <- temp %>% group_by(id) %>% dplyr::summarise(PropStrat=mean(PropStrat)) %>% filter(!is.na(PropStrat))
SSstrat14.df2_fil<-left_join(SSstrat14.df2, temp)
#end merge

# setting shape type and size for geom_point
shapes <- c( 3, 16, 16, 16, 16)
sizes <- c (2, 2, 3, 4, 5)

setdensity <- ggplot () +
  geom_polygon (data = SSstrat14.df2_fil, aes (x = long2, y = lat2, group = group, fill=PropStrat), colour = "#00b159", size = 0.4) +
  scale_fill_gradient(low = "white", high = "#00b159", na.value="white") +
  geom_polygon (data = NAFO.df2, aes (x = long2, y = lat2, group = group), colour = "black", fill = NA, size = 0.4) +
  geom_path (data = Border.df2, aes (x = long2, y = lat2), colour = "black", linetype = "dashed", size = 0.8) +
  #geom_point (data = mapSPEC, aes (x = Lon, y = Lat, size=CAT, shape = CAT)) +
  scale_shape_manual (values = shapes, drop = FALSE) + scale_size_manual (values = sizes, drop = FALSE)+
  coord_map () + 
  theme_bw () + 
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank()) + 
  scale_y_continuous ("Latitude", expand = c (0,0), limits = c (40, 43)) + 
  scale_x_continuous ( "Longitude", expand = c (0,0), limits = c (-71, -65)) +
  theme (axis.title = element_text (size = 9), 
         axis.text = element_text (size = 8), 
         legend.text = element_text (size = 8), 
         legend.title = element_text (size = 9)) + 
  theme (legend.position = "right") +
  #labs (shape="Catch (kg/tow)", size = "Catch (kg/tow)")+
  labs (shape="kg/trait", size = "kg/trait")
setdensity


plot1 <- setdensity+ggtitle("1986-1995")
plot2 <- setdensity+ggtitle("1996-2005")
plot3 <- setdensity+ggtitle("2006-2015")
plot4 <- setdensity+ggtitle("2016+")

library(ggpubr)
ggarrange(plot1, plot2, plot3, plot4, ncol=1, nrow=4, common.legend = TRUE, legend="bottom")

ggsave(here("figures/Survey_DFO_SetDensity.png"), width = 4, height = 12, units="in")
