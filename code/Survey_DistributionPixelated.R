#############################
# 5Z Cod
# Survey Distribution Pixelated
# July 1, 2021
# Author: Caira Clark
#############################

library(ROracle);library(ggplot2);library(viridis);library(dplyr);library(ggpubr);library(here);library(psych)

#Run the script that loads the shapefiles for maps
source("S:/Science/Population Ecology/Georges Bank/Useful R-scripts/LoadShapefiles.R")

channel <- ROracle::dbConnect(DBI::dbDriver("Oracle"), username=oracle.username, password=oracle.password, oracle.dsn) 

#DFO DISTRIBUTION PIXELATED--------------------------


dfo <- ROracle::dbGetQuery(channel, "
                             select a.MISSION, a.SETNO, a.SDATE, a.STRAT, a.SLAT, a.SLONG, a.DIST, a.DUR,
                             b.totwgt, b.spec, b.sampwgt, b.size_class, c.year
                             from groundfish.gsinf a, groundfish.gscat b, groundfish.gsmissions c
                             where a.type=1
                             and b.spec=10
                             and c.season in ('SPRING')
                             and c.year between ('1986') and ('2023')
                             and a.strat in ('5Z1', '5Z2', '5Z3', '5Z4', '5Z5', '5Z6', '5Z7', '5Z8', '5Z9')
                             and a.mission=b.mission
                             and b.mission=c.mission
                             and a.setno=b.setno")

sets <- ROracle::dbGetQuery(channel, "
                             select a.MISSION, a.SETNO, b.YEAR, a.SLAT, a.SLONG
                             from groundfish.gsinf a, groundfish.gsmissions b
                             where a.type=1
                             and b.season in ('SPRING')
                             and b.year between ('1986') and ('2023')
                             and a.strat in ('5Z1', '5Z2', '5Z3', '5Z4', '5Z5', '5Z6', '5Z7', '5Z8', '5Z9')
                             and a.mission=b.mission")

dfo <- left_join(sets, dfo) %>% mutate(
  year_group = dplyr::case_when(
    YEAR <= 1979            ~ "1970-1979",
    YEAR > 1979 & YEAR <= 1989 ~ "1980-1989",
    YEAR > 1989 & YEAR <= 1999 ~ "1990-1999",
    YEAR > 1999 & YEAR <= 2009 ~ "2000-2009",
    YEAR > 2009 & YEAR <= 2019 ~ "2010-2019",
    YEAR > 2019  ~ "2020+")) %>%
  mutate(std.WGT = TOTWGT * (1.75/DIST)) %>% # Standardize weight for distance of tow (1.75nm)
  mutate(LATITUDE = (as.numeric(substr(SLAT,1,2))+(SLAT - as.numeric(substr(SLAT,1,2))*100)/60)) %>%
  mutate(LONGITUDE = (as.numeric(substr(SLONG,1,2))+(SLONG - as.numeric(substr(SLONG,1,2))*100)/60)*-1)

dfo$LATITUDE <- round(dfo$LATITUDE, digits=1)
dfo$LONGITUDE <- round(dfo$LONGITUDE, digits=1)

dfo <- dfo %>% dplyr::select(year_group, LATITUDE, LONGITUDE, std.WGT)
dfo$std.WGT[is.na(dfo$std.WGT)] <- 0

dfo1 <- dfo %>% group_by(year_group, LATITUDE, LONGITUDE) %>% dplyr::summarise(mean=geometric.mean(std.WGT))
dfo1$SURVEY <- "DFO SPRING"

map1<-
  ggplot () +
  #coord_cartesian() +
  geom_tile(data=dfo1,aes(x=LONGITUDE, y=LATITUDE, fill=mean)) +
  scale_fill_viridis(begin=0.5, option="turbo", name="Mean kg/tow") +
  geom_polygon (data = SSstrat14.df2, aes (x = long2, y = lat2, group = group), colour = "black", fill = NA, linewidth = 0.4) +
  geom_polygon (data = NAFO.df2, aes (x = long2, y = lat2, group = group), colour = "black", fill = NA, linewidth= 0.4) +
  geom_path (data = Border.df2, aes (x = long2, y = lat2), colour = "black", linetype = "dashed", linewidth = 0.8) +
  coord_map () + 
  theme_bw () + 
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank()) + 
  scale_y_continuous ("Latitude", expand = c (0,0), limits = c (40, 43)) + 
  scale_x_continuous ( "Longitude", expand = c (0,0), limits = c (-71, -65)) +
  theme (axis.title = element_text (size = 9), 
         axis.text = element_text (size = 8), 
         legend.text = element_text (size = 8), 
         legend.title = element_text (size = 9)) +
  facet_grid(year_group~SURVEY)
map1



#NMFS SPRING DISTRIBUTION PIXELATED---------------------------

nmfs <- dbGetQuery(channel, "select a.cruise6 mission, a.est_year year, a.station setno, a.BEGLAT slat, a.BEGLON slong, c.expcatchwt, d.area_swept_wings_mean_km2 asw from
                     usnefsc.uss_station a, usnefsc.uss_mstr_cruise b, usnefsc.uss_catch c, usnefsc.uss_tow_evaluation d
                     where c.svspp in ('073')
                     and b.season in ('SPRING')
                     and b.purpose_code in ('10')
                     and a.est_year between ('1970') and ('2023')
                     and a.shg < ('137')
                     and a.cruise6=b.cruise6
                     and a.cruise6=c.cruise6
                     and a.cruise6=d.cruise6
                     and a.tow=c.tow
                     and a.station=d.station
                     and a.station=c.station
                     and a.stratum in ('01130', '01140', '01150', '01160', '01170', '01180', '01190', '01200', '01210')")

nmfs2 <- dbGetQuery(channel, "select a.cruise6 mission, a.est_year year, a.station setno, a.BEGLAT slat, a.BEGLON slong, c.expcatchwt from
                     usnefsc.uss_station a, usnefsc.uss_mstr_cruise b, usnefsc.uss_catch c
                     where c.svspp in ('073')
                     and b.season in ('SPRING')
                     and b.purpose_code in ('10')
                     and a.est_year between ('1970') and ('2007')
                     and a.shg < ('137')
                     and a.cruise6=b.cruise6
                     and a.cruise6=c.cruise6
                     and a.tow=c.tow
                     and a.station=c.station
                     and a.stratum in ('01130', '01140', '01150', '01160', '01170', '01180', '01190', '01200', '01210')")

nmfs2$ASW<-NA
nmfs<-rbind(nmfs2, nmfs)

#Pre-2009 has no ASW value and we just use the raw data without standardization

nmfs$DIST<-nmfs$ASW*(0.539957^2) #Converting the DIST variable for the US data into nautical miles for application
nmfs$YEAR <- as.numeric(nmfs$YEAR)
nmfs$TOTWGT <- (nmfs$EXPCATCHWT*0.007)/nmfs$DIST

pre2009 <- nmfs %>% filter(YEAR<2009) %>% mutate(TOTWGT=EXPCATCHWT)
post2009 <- nmfs %>% filter(YEAR>=2009)

nmfs <- rbind(pre2009, post2009)

nmfsconv <- read.csv(here("data/nmfsconv.csv"))
colnames(nmfsconv)[1] <- "YEAR"
nmfsconv[is.na(nmfsconv)] <- 1

nmfs <- left_join(nmfs,nmfsconv%>%select(-nsprconv)) %>% mutate(TOTWGT=TOTWGT*nfallconv) %>% select(-nfallconv)

sets <- dbGetQuery(channel, "select a.cruise6 mission, a.est_year year, a.station setno, a.beglat slat, a.beglon slong from
                     usnefsc.uss_station a, usnefsc.uss_mstr_cruise b
                     where b.season in ('SPRING')
                     and b.purpose_code in ('10')
                     and a.est_year between ('1970') and ('2023')
                     and a.shg < ('137')
                     and a.cruise6=b.cruise6
                     and a.stratum in ('01130', '01140', '01150', '01160', '01170', '01180', '01190', '01200', '01210')")

sets$YEAR <- as.numeric(sets$YEAR)


nmfs <- left_join(sets, nmfs) %>% mutate(
  year_group = dplyr::case_when(
    YEAR <= 1979            ~ "1970-1979",
    YEAR > 1979 & YEAR <= 1989 ~ "1980-1989",
    YEAR > 1989 & YEAR <= 1999 ~ "1990-1999",
    YEAR > 1999 & YEAR <= 2009 ~ "2000-2009",
    YEAR > 2009 & YEAR <= 2019 ~ "2010-2019",
    YEAR > 2019  ~ "2020+")) %>%
  mutate(LATITUDE = (as.numeric(substr(SLAT,1,2))+(SLAT - as.numeric(substr(SLAT,1,2))*100)/60)) %>%
  mutate(LONGITUDE = (as.numeric(substr(SLONG,1,2))+(SLONG - as.numeric(substr(SLONG,1,2))*100)/60)*-1)

nmfs$LATITUDE <- round(nmfs$LATITUDE, digits=1)
nmfs$LONGITUDE <- round(nmfs$LONGITUDE, digits=1)

nmfs <- nmfs %>% dplyr::select(year_group, LATITUDE, LONGITUDE, TOTWGT)
nmfs$TOTWGT[is.na(nmfs$TOTWGT)] <- 0

nmfs2 <- nmfs %>% group_by(year_group, LATITUDE, LONGITUDE) %>% dplyr::summarise(mean=geometric.mean(TOTWGT))
nmfs2$SURVEY <- "NMFS SPRING"

map2<-
  ggplot () +
  #coord_cartesian() +
  geom_tile(data=nmfs2,aes(x=LONGITUDE, y=LATITUDE, fill=mean)) +
  scale_fill_viridis(begin=0.5, option="turbo", name="Mean kg/tow") +
  geom_polygon (data = SSstrat14.df2, aes (x = long2, y = lat2, group = group), colour = "black", fill = NA, size = 0.4) +
  geom_polygon (data = NAFO.df2, aes (x = long2, y = lat2, group = group), colour = "black", fill = NA, size = 0.4) +
  geom_path (data = Border.df2, aes (x = long2, y = lat2), colour = "black", linetype = "dashed", linewidth = 0.8) +
  coord_map () + 
  theme_bw () + 
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank()) + 
  scale_y_continuous ("Latitude", expand = c (0,0), limits = c (40, 43)) + 
  scale_x_continuous ( "Longitude", expand = c (0,0), limits = c (-71, -65)) +
  theme (axis.title = element_text (size = 9), 
         axis.text = element_text (size = 8), 
         legend.text = element_text (size = 8), 
         legend.title = element_text (size = 9)) +
  facet_grid(year_group~SURVEY)
map2

#NMFS FALL DISTRIBUTION PIXELATED-----------------------

nmfs <- dbGetQuery(channel, "select a.cruise6 mission, a.est_year year, a.station setno, a.BEGLAT slat, a.BEGLON slong, c.expcatchwt, d.area_swept_wings_mean_km2 asw from
                     usnefsc.uss_station a, usnefsc.uss_mstr_cruise b, usnefsc.uss_catch c, usnefsc.uss_tow_evaluation d
                     where c.svspp in ('073')
                     and b.season in ('FALL')
                     and b.purpose_code in ('10')
                     and a.est_year between ('1970') and ('2023')
                     and a.shg < ('137')
                     and a.cruise6=b.cruise6
                     and a.cruise6=c.cruise6
                     and a.cruise6=d.cruise6
                     and a.tow=c.tow
                     and a.station=d.station
                     and a.station=c.station
                     and a.stratum in ('01130', '01140', '01150', '01160', '01170', '01180', '01190', '01200', '01210')")

nmfs2 <- dbGetQuery(channel, "select a.cruise6 mission, a.est_year year, a.station setno, a.BEGLAT slat, a.BEGLON slong, c.expcatchwt from
                     usnefsc.uss_station a, usnefsc.uss_mstr_cruise b, usnefsc.uss_catch c
                     where c.svspp in ('073')
                     and b.season in ('FALL')
                     and b.purpose_code in ('10')
                     and a.est_year between ('1970') and ('2007')
                     and a.shg < ('137')
                     and a.cruise6=b.cruise6
                     and a.cruise6=c.cruise6
                     and a.tow=c.tow
                     and a.station=c.station
                     and a.stratum in ('01130', '01140', '01150', '01160', '01170', '01180', '01190', '01200', '01210')")

nmfs2$ASW<-NA
nmfs<-rbind(nmfs2, nmfs)


#Pre-2009 has no ASW value and we just use the raw data without standardization

nmfs$DIST<-nmfs$ASW*(0.539957^2) #Converting the DIST variable for the US data into nautical miles for application
nmfs$YEAR <- as.numeric(nmfs$YEAR)
nmfs$TOTWGT <- (nmfs$EXPCATCHWT*0.007)/nmfs$DIST

pre2009 <- nmfs %>% filter(YEAR<2009) %>% mutate(TOTWGT=EXPCATCHWT)
post2009 <- nmfs %>% filter(YEAR>=2009)

nmfs <- rbind(pre2009, post2009)

nmfs <- left_join(nmfs,nmfsconv%>%select(-nfallconv)) %>% mutate(TOTWGT=TOTWGT*nsprconv) %>% select(-nsprconv)

sets <- dbGetQuery(channel, "select a.cruise6 mission, a.est_year year, a.station setno, a.beglat slat, a.beglon slong from
                     usnefsc.uss_station a, usnefsc.uss_mstr_cruise b
                     where b.season in ('FALL')
                     and b.purpose_code in ('10')
                     and a.est_year between ('1970') and ('2023')
                     and a.shg < ('137')
                     and a.cruise6=b.cruise6
                     and a.stratum in ('01130', '01140', '01150', '01160', '01170', '01180', '01190', '01200', '01210')")

sets$YEAR <- as.numeric(sets$YEAR)


nmfsf <- left_join(sets, nmfs) %>% mutate(
  year_group = dplyr::case_when(
    YEAR <= 1979            ~ "1970-1979",
    YEAR > 1979 & YEAR <= 1989 ~ "1980-1989",
    YEAR > 1989 & YEAR <= 1999 ~ "1990-1999",
    YEAR > 1999 & YEAR <= 2009 ~ "2000-2009",
    YEAR > 2009 & YEAR <= 2019 ~ "2010-2019",
    YEAR > 2019  ~ "2020+")) %>%
  mutate(LATITUDE = (as.numeric(substr(SLAT,1,2))+(SLAT - as.numeric(substr(SLAT,1,2))*100)/60)) %>%
  mutate(LONGITUDE = (as.numeric(substr(SLONG,1,2))+(SLONG - as.numeric(substr(SLONG,1,2))*100)/60)*-1)

nmfsf$LATITUDE <- round(nmfsf$LATITUDE, digits=1)
nmfsf$LONGITUDE <- round(nmfsf$LONGITUDE, digits=1)

nmfsf <- nmfsf %>% dplyr::select(year_group, LATITUDE, LONGITUDE, TOTWGT)
nmfsf$TOTWGT[is.na(nmfsf$TOTWGT)] <- 0

nmfsf3 <- nmfsf %>% group_by(year_group, LATITUDE, LONGITUDE) %>% dplyr::summarise(mean=geometric.mean(TOTWGT))
nmfsf3$SURVEY <- "NMFS FALL"

map3<-
  ggplot () +
  #coord_cartesian() +
  geom_tile(data=nmfsf3,aes(x=LONGITUDE, y=LATITUDE, fill=mean)) +
  scale_fill_viridis(begin=0.5, option="turbo", name="Mean kg/tow") +
  geom_polygon (data = SSstrat14.df2, aes (x = long2, y = lat2, group = group), colour = "black", fill = NA, size = 0.4) +
  geom_polygon (data = NAFO.df2, aes (x = long2, y = lat2, group = group), colour = "black", fill = NA, size = 0.4) +
  geom_path (data = Border.df2, aes (x = long2, y = lat2), colour = "black", linetype = "dashed", linewidth = 0.8) +
  coord_map () + 
  theme_bw () + 
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank()) + 
  scale_y_continuous ("Latitude", expand = c (0,0), limits = c (40, 43)) + 
  scale_x_continuous ( "Longitude", expand = c (0,0), limits = c (-71, -65)) +
  theme (axis.title = element_text (size = 9), 
         axis.text = element_text (size = 8), 
         legend.text = element_text (size = 8), 
         legend.title = element_text (size = 9)) +
  facet_grid(year_group~SURVEY)
map3

#ALL SURVEYS TOGETHER-----------------------------

setwd("~/LocalRespository/EGBFramework")
dfo <- ggarrange(map1, ncol=1, nrow=1, common.legend=TRUE, legend="bottom")
nmfsspring <- ggarrange(map2, ncol=1, nrow=1, common.legend=TRUE, legend="bottom")
nmfsfall <- ggarrange(map3, ncol=1, nrow=1, common.legend=TRUE, legend="bottom")
ggarrange(dfo, nmfsspring, nmfsfall, ncol=3, nrow=1)

ggsave(here("figures/Survey_Distribution.png"), width=8.5, height=11, units="in")
