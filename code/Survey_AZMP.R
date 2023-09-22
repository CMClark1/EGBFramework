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

ggsave(here("figures/Survey_Env_AnnualBottomTemp.png"), width=15, height=10, units="in")

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

ggsave(here("figures/Survey_Env_AnnualSurfaceTemp.png"), width=15, height=10, units="in")

rm(Derived_Annual_Broadscale)

#Zooplankton by Section

data("Zooplankton_Annual_Sections") #zooplankton by section

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

ggsave(here("figures/Survey_Env_AnnualZooDryWeight.png"), width=15, height=10, units="in")

rm(Zooplankton_Annual_Sections)

#Calanus Map
data("Zooplankton_Occupations_Broadscale") #zooplankton by set location

sf::sf_use_s2(FALSE)
samparea <- Mar.utils::identify_area(df=Zooplankton_Occupations_Broadscale, lat.field = "latitude", lon.field = "longitude",
                                     agg.poly.shp = "S:/Science/Population Ecology/Georges Bank/Useful R-scripts/Mapping Data/MaritimesRegionEcosystemAssessmentStrata(2014-)NAD83.shp",
                                     agg.poly.field = "StrataID")
rm(Zooplankton_Occupations_Broadscale)

calanus <- samparea %>% mutate(YEARS=case_when(year<1970 ~ "1960s",
                                    year%in%1970:1979 ~ "1970s",
                                    year%in%1980:1989 ~ "1980s",
                                    year%in%1990:1999 ~ "1990s",
                                    year%in%2000:2009 ~ "2000s",
                                    year%in%2010:2019 ~ "2010s",
                                    year>2019 ~ "2020+"),
                               SEASON=case_when(season=="Summer" ~ "SUMMER",
                                                season=="Winter" ~ "SPRING")) %>% 
  filter(!is.na(Calanus_finmarchicus_abundance)) %>%
  group_by(YEARS, SEASON, StrataID) %>%
  summarise(Calanus_mean=mean(Calanus_finmarchicus_abundance))
colnames(calanus)[3] <- "id"


calanus2 <- samparea %>% mutate(YEARS=case_when(year<1970 ~ "1960s",
                                               year%in%1970:1979 ~ "1970s",
                                               year%in%1980:1989 ~ "1980s",
                                               year%in%1990:1999 ~ "1990s",
                                               year%in%2000:2009 ~ "2000s",
                                               year%in%2010:2019 ~ "2010s",
                                               year>2019 ~ "2020+"),
                               SEASON=case_when(season=="Summer" ~ "SUMMER",
                                                season=="Winter" ~ "SPRING")) %>% 
  filter(!is.na(Calanus_finmarchicus_abundance)) %>%
  group_by(year, SEASON, StrataID) %>%
  summarise(Calanus_mean=mean(Calanus_finmarchicus_abundance))
colnames(calanus2)[3] <- "id"

#Run the script that loads the shapefiles for maps
source("S:/Science/Population Ecology/Georges Bank/Useful R-scripts/LoadShapefiles.R")

SSstrat14.df2_fil <- calanus %>% group_by(YEARS, SEASON) %>% full_join(SSstrat14.df2) %>% filter(!is.na(SEASON) & !is.na(YEARS))
SSstrat14.df2_fil2 <- calanus2 %>% group_by(year, SEASON) %>% full_join(SSstrat14.df2) %>% filter(!is.na(SEASON) & !is.na(year))

ggplot() +
  scale_fill_viridis(option="inferno") +
  geom_polygon (data = SSstrat14.df2_fil, aes (x = long2, y = lat2, group = group, fill=Calanus_mean), linewidth = 0.4) +
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

ggsave(here("figures/Survey_Env_CalanusMap.png"), width=10, height=20, units="in")

ggplot() +
  scale_fill_viridis(option="inferno") +
  geom_polygon (data = SSstrat14.df2_fil2%>%filter(year>2015), aes (x = long2, y = lat2, group = group, fill=Calanus_mean), linewidth = 0.4) +
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
  facet_grid(year~SEASON)

ggsave(here("figures/Survey_Env_CalanusMap_5year.png"), width=10, height=20, units="in")


#Zooplankton Dry Weight Map

dryweight <- samparea %>% mutate(YEARS=case_when(year<1970 ~ "1960s",
                                               year%in%1970:1979 ~ "1970s",
                                               year%in%1980:1989 ~ "1980s",
                                               year%in%1990:1999 ~ "1990s",
                                               year%in%2000:2009 ~ "2000s",
                                               year%in%2010:2019 ~ "2010s",
                                               year>2019 ~ "2020+"),
                               SEASON=case_when(season=="Summer" ~ "SUMMER",
                                                season=="Winter" ~ "SPRING")) %>% 
  filter(!is.na(zooplankton_meso_dry_weight)) %>%
  group_by(YEARS, SEASON, StrataID) %>%
  summarise(DryWeightmean=mean(zooplankton_meso_dry_weight))
colnames(dryweight)[3] <- "id"


dryweight2 <- samparea %>% mutate(YEARS=case_when(year<1970 ~ "1960s",
                                                 year%in%1970:1979 ~ "1970s",
                                                 year%in%1980:1989 ~ "1980s",
                                                 year%in%1990:1999 ~ "1990s",
                                                 year%in%2000:2009 ~ "2000s",
                                                 year%in%2010:2019 ~ "2010s",
                                                 year>2019 ~ "2020+"),
                                 SEASON=case_when(season=="Summer" ~ "SUMMER",
                                                  season=="Winter" ~ "SPRING")) %>% 
  filter(!is.na(zooplankton_meso_dry_weight)) %>%
  group_by(year, SEASON, StrataID) %>%
  summarise(DryWeightmean=mean(zooplankton_meso_dry_weight))
colnames(dryweight)[3] <- "id"

SSstrat14.df2_fil <- dryweight %>% group_by(YEARS, SEASON) %>% full_join(SSstrat14.df2) %>% filter(!is.na(SEASON) & !is.na(YEARS))
SSstrat14.df2_fil2 <- dryweight %>% group_by(year, SEASON) %>% full_join(SSstrat14.df2) %>% filter(!is.na(SEASON) & !is.na(year))

ggplot() +
  scale_fill_viridis(option="inferno") +
  geom_polygon (data = SSstrat14.df2_fil, aes (x = long2, y = lat2, group = group, fill=DryWeightmean), linewidth = 0.4) +
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

ggsave(here("figures/Survey_Env_ZooDryWeightMap.png"), width=10, height=20, units="in")

ggplot() +
  scale_fill_viridis(option="inferno") +
  geom_polygon (data = SSstrat14.df2_fil2%>%filter(year>2015), aes (x = long2, y = lat2, group = group, fill=DryWeightmean), linewidth = 0.4) +
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
  facet_grid(year~SEASON)

ggsave(here("figures/Survey_Env_ZooDryWeightMap_5year.png"), width=10, height=20, units="in")

