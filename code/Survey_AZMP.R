#############################
# 5Z Cod
# AZMP Environmental Data
# September 2023
# Author: Caira Clark & Irene Andrushchenko
#############################

library(ROracle);library(ggplot2);library(viridis);library(dplyr);library(ggpubr);library(here);library(psych);library(azmpdata); library(reshape2); library(tidyr)

#Run the script that loads the shapefiles for maps
#source("S:/Science/Population Ecology/Georges Bank/Useful R-scripts/LoadShapefiles.R")

channel <- ROracle::dbConnect(DBI::dbDriver("Oracle"), username=oracle.username, password=oracle.password, oracle.dsn) 

#help("azmpdata") 
#library(help = "azmpdata") #looks up list of available variables

#See what tables have specific keywords
variable_lookup(keywords = "temperature")
variable_lookup(keywords = "zooplankton")

#Load data of interest
data("Derived_Annual_Broadscale") #surface and bottom temperatures
data("Zooplankton_Annual_Sections") #zooplankton by section
data("Zooplankton_Occupations_Broadscale") #zooplankton by set location

#Annual Bottom Temperature
Derived_Annual_Broadscale %>%
  filter(!is.na(temperature_at_sea_floor)) %>%
  mutate(NAFO=case_when(area%in%c('4Vn','4Vs','Misaine Bank','Cabot Strait') ~ "4V",
                        area%in%c('4W','Emerald Basin') ~ '4W',
                        area%in%c('4X','4XeGoM+BoF','4XSS','Georges Basin','Lurcher Shoal') ~ "4X",
                        area%in%c('E Georges Bank') ~ "5Z",
                        area%in%c('4XeGoM+BoF','4XSS') ~ "4X5")) %>%
  filter(NAFO%in%c("4X", "5Z")) %>%
  group_by(year, NAFO, area) %>% 
  summarise(mean=mean(temperature_at_sea_floor)) %>%
  ggplot(aes(year, mean, group=area, colour=area)) +
  geom_path(linewidth=1) +
  facet_grid(NAFO~.) +
  scale_colour_viridis_d(option="turbo", end=0.8) +
  theme_bw()+
  ylab("Temperature (*C)") +
  ggtitle("Annual Sea Floor Temperature")

#Annual Sea Surface Temperature
Derived_Annual_Broadscale %>%
  filter(!is.na(sea_surface_temperature_from_satellite)) %>%
  mutate(NAFO=case_when(area%in%c('4Vn','4Vs','Misaine Bank','Cabot Strait') ~ "4V",
                        area%in%c('4W','Emerald Basin') ~ '4W',
                        area%in%c('4X','4XeGoM+BoF','4XSS','Georges Basin','Lurcher Shoal') ~ "4X",
                        area%in%c('E Georges Bank') ~ "5Z",
                        area%in%c('4XeGoM+BoF','4XSS') ~ "4X5")) %>%
  filter(NAFO%in%c("4X", "5Z") & !is.na(NAFO)) %>%
  group_by(year, NAFO, area) %>% 
  summarise(mean=mean(sea_surface_temperature_from_satellite)) %>%
  ggplot(aes(year, mean, group=area, colour=area)) +
  geom_path(linewidth=1) +
  facet_grid(NAFO~.) +
  scale_colour_viridis_d(option="turbo", end=0.8) +
  theme_bw()+
  ylab("Temperature (*C)") +
  ggtitle("Annual Sea Surface Temperature")

#Zooplankton by Section
Zooplankton_Annual_Sections %>%
  filter(!is.na(zooplankton_meso_dry_weight)) %>%
  mutate(NAFO=case_when(section%in%c('CSL','LL') ~ "4V",
                        section%in%c('HL') ~ '4W',
                        section%in%c('BBL') ~ "4X")) %>%
  filter(NAFO%in%c("4X", "5Z") & !is.na(NAFO)) %>%
  group_by(year, NAFO, section) %>% 
  summarise(mean=mean(zooplankton_meso_dry_weight)) %>%
  ggplot(aes(year, mean, group=section, colour=mean)) +
  geom_path(linewidth=2) +
  facet_grid(NAFO~.) +
  scale_colour_viridis_c(option="inferno", end=0.8) +
  theme_bw()+
  ylab("Weight (g)") +
  ggtitle("Annual Zooplankton")

#Zooplankton Map
head(Zooplankton_Occupations_Broadscale)

sf::sf_use_s2(FALSE)
samparea <- Mar.utils::identify_area(df=Zooplankton_Occupations_Broadscale, lat.field = "latitude", lon.field = "longitude",
                                     agg.poly.shp = "S:/Science/Population Ecology/Georges Bank/Useful R-scripts/Mapping Data/MaritimesRegionEcosystemAssessmentStrata(2014-)NAD83.shp",
                                     agg.poly.field = "id")

