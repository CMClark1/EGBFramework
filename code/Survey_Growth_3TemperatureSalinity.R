#############################
# Maritimes Cod
# Growth Part 2 - Have there been changes to temperature or salinity?
# September 2023
# Author: Caira Clark
#############################

library(ggplot2)
library(ROracle)
library(tidyr)
library(plyr)
library(dplyr)
library(viridis)
library(lubridate)
library(ggpubr)
library(here)

#Run the script that loads the shapefiles for maps
source("S:/Science/Population Ecology/Georges Bank/Useful R-scripts/LoadShapefiles.R")

channel <- ROracle::dbConnect(DBI::dbDriver("Oracle"), username=oracle.username, password=oracle.password, oracle.dsn) 

#Load DFO survey data for temperature

dfo1<-ROracle::dbGetQuery(channel, "select c.year, c.season, b.SETNO, b.slat LAT, b.slong LON, b.strat, d.unit, b.bottom_temperature
                         from groundfish.gsinf b, groundfish.gsmissions c, groundfish.gsarea2 d
                         where d.unit like ('4%')
                         and c.season in ('SPRING', 'SUMMER')
                         and b.mission=c.mission
                         and b.area=d.area")

dfo2<-ROracle::dbGetQuery(channel, "select c.year, c.season, b.SETNO, b.slat LAT, b.slong LON, b.strat, d.unit, b.bottom_temperature
                         from groundfish.gsinf b, groundfish.gsmissions c, groundfish.gsarea2 d
                         where d.unit like ('5%')
                         and c.season in ('SPRING', 'SUMMER')
                         and b.mission=c.mission
                         and b.area=d.area")

dfo <- rbind(dfo1, dfo2)

dfo$LAT<-with(dfo, as.numeric(substr(LAT,1,2))+(LAT - as.numeric(substr(LAT,1,2))*100)/60) #Converts DDMM to decimal degrees
dfo$LON<-with(dfo, as.numeric(substr(LON,1,2))+((LON - as.numeric(substr(LON,1,2))*100)/60)*-1) #Converts DDMM to decimal degrees
dfo$LON<-dfo$LON*-1
dfo <- dfo %>% mutate(DESCRIPTION=case_when(SEASON=="SPRING" ~ "DFO SPRING", SEASON=="SUMMER" ~ "DFO SUMMER"), 
                      UNIT=substr(case_when(grepl("5Z", UNIT) ~ "5Z", grepl("4", UNIT) ~ UNIT), 1, 3))
dfo$YEAR <- as.character(dfo$YEAR)

andes <- read.csv(here("data/andes_temp_data.csv"))
colnames(andes)[1] <- "YEAR"

dfo <- rbind(dfo, andes)

dfo <- dfo %>% mutate(YEARS=case_when(YEAR<1970 ~ "1960s",
                       YEAR%in%1970:1979 ~ "1970s",
                       YEAR%in%1980:1989 ~ "1980s",
                       YEAR%in%1990:1999 ~ "1990s",
                       YEAR%in%2000:2009 ~ "2000s",
                       YEAR%in%2010:2019 ~ "2010s",
                       YEAR>2019 ~ "2020-2023")) %>%
      distinct()

dfo[is.na(dfo) | dfo =="Inf" | dfo =="-Inf"] = NA
dfo$BOTTOM_TEMPERATURE <- as.numeric(dfo$BOTTOM_TEMPERATURE)

stratsum <- dfo %>% filter(!is.na(BOTTOM_TEMPERATURE)) %>%
  group_by(YEARS, SEASON, STRAT) %>%
  summarise(meantemp=mean(BOTTOM_TEMPERATURE)) %>%
  mutate(group=paste(STRAT, ".1", sep = "")) %>%
  filter(!is.na(SEASON) & !is.na(YEARS))

stratsum2 <- dfo %>% filter(!is.na(BOTTOM_TEMPERATURE)) %>%
  group_by(YEAR, SEASON, STRAT) %>%
  summarise(meantemp=mean(BOTTOM_TEMPERATURE)) %>%
  mutate(group=paste(STRAT, ".1", sep = "")) %>%
  filter(!is.na(SEASON) & !is.na(YEAR))

#Combine shapefile for Scotian Shelf strata with data
SSstrat14.df2_fil <- stratsum %>% group_by(YEARS, SEASON) %>% full_join(SSstrat14.df2) %>% filter(!is.na(SEASON) & !is.na(YEARS))
SSstrat14.df2_fil2 <- stratsum2 %>% group_by(YEAR, SEASON) %>% full_join(SSstrat14.df2) %>% filter(!is.na(SEASON) & !is.na(YEAR))

map1<-
  ggplot() +
  scale_fill_viridis(option="A") +
  geom_polygon (data = SSstrat14.df2_fil, aes (x = long2, y = lat2, group = group, fill=meantemp), linewidth = 0.4) +
  geom_polygon (data = SSstrat14.df2, aes (x = long2, y = lat2, group = group), colour = "black", fill=NA, linewidth = 0.4) +
  geom_polygon (data = NAFO.df2, aes (x = long2, y = lat2, group = group), colour = "black", fill = NA, linewidth = 0.4) +
  geom_path (data = Border.df2, aes (x = long2, y = lat2), colour = "black", linetype = "dashed", linewidth = 0.8) +
  coord_map () + 
  theme_bw () + 
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank()) + 
  scale_y_continuous ("Latitude", expand = c (0,0), limits = c (40, 48)) + 
  scale_x_continuous ( "Longitude", expand = c (0,0), limits = c (-69, -56)) +
  theme (axis.title = element_text (size = 9), 
         axis.text = element_text (size = 8), 
         legend.text = element_text (size = 8), 
         legend.title = element_text (size = 9)) +
  facet_grid(YEARS~SEASON)
map1 

ggsave(here("figures/Survey_Env_TemperatureMap.png"), width=10, height=20, units="in")

map2<-
  ggplot() +
  scale_fill_viridis(option="A") +
  geom_polygon (data = SSstrat14.df2_fil2%>%filter(YEAR>2016), aes (x = long2, y = lat2, group = group, fill=meantemp), linewidth = 0.4) +
  geom_polygon (data = SSstrat14.df2, aes (x = long2, y = lat2, group = group), colour = "black", fill=NA, linewidth = 0.4) +
  geom_polygon (data = NAFO.df2, aes (x = long2, y = lat2, group = group), colour = "black", fill = NA, linewidth = 0.4) +
  geom_path (data = Border.df2, aes (x = long2, y = lat2), colour = "black", linetype = "dashed", linewidth = 0.8) +
  coord_map () + 
  theme_bw () + 
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank()) + 
  scale_y_continuous ("Latitude", expand = c (0,0), limits = c (40, 48)) + 
  scale_x_continuous ( "Longitude", expand = c (0,0), limits = c (-69, -56)) +
  theme (axis.title = element_text (size = 9), 
         axis.text = element_text (size = 8), 
         legend.text = element_text (size = 8), 
         legend.title = element_text (size = 9)) +
  facet_grid(YEAR~SEASON)
map2

ggsave(here("figures/Survey_Env_TemperatureMap_5years.png"), width=10, height=20, units="in")


