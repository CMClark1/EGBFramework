#############################
# Missing Slow Growing Cod in 5Z
# Data Exploration
# September 2023
# Author: Caira Clark / Irene Andrushchenko
#############################

library(ggplot2)
library(ROracle)
library(tidyr)
library(plyr)
library(dplyr)
library(viridis)
library(lubridate)
library(tidyverse)
library(ggpubr)
library(here)

channel <- ROracle::dbConnect(DBI::dbDriver("Oracle"), username=oracle.username, password=oracle.password, oracle.dsn) 

###Part 1. Data Pull --------------------------------------------
#Pull all cod, haddock, pollock survey, port sample, and observer age/lengths for 4VWX5Z

##Surveys

#NMFS

nmfs<-ROracle::dbGetQuery(channel, "SELECT B.Year, B.SEASON, a.SVSPP SPEC, A.LENGTH FLEN, A.AGE, C.EST_MONTH MONTH, C.EST_DAY DAY, C.DECDEG_BEGLAT LAT, C.DECDEG_BEGLON LON, C.TOW SETNO
                                   FROM USNEFSC.USS_MSTR_CRUISE B INNER JOIN (USNEFSC.USS_STATION C INNER JOIN USNEFSC.USS_DETAIL A ON (C.CRUISE6 = A.CRUISE6) AND (C.STRATUM = A.STRATUM) AND (C.TOW = A.TOW) AND (C.STATION = A.STATION)) ON B.CRUISE6 = A.CRUISE6
                                   WHERE (((A.LENGTH) Is Not Null) AND ((B.SEASON)in('FALL','SPRING')) AND ((A.SVSPP) in ('073','074','075')) AND ((B.PURPOSE_CODE)='10') AND ((C.AREA) In ('551','552','561','562', '522', '525')) AND ((C.STRATUM) In ('01160','01170','01180','01190','01200','01210','01220')))
                                   ORDER BY B.Year, C.CRUISE6
                                   ")
nmfs$UNIT <- "5Z"
nmfs <- nmfs %>% mutate(DESCRIPTION=case_when(SEASON=="FALL" ~ "NMFS FALL", SEASON=="SPRING" ~ "NMFS SPRING"),
                        SPEC=case_when(SPEC=="073" ~ 10,
                                       SPEC=="074" ~ 11,
                                       SPEC=="075" ~ 16))

#DFO

dfo1<-ROracle::dbGetQuery(channel, "select c.year, c.season, a.spec, a.flen, a.age, extract(MONTH from b.SDATE) MONTH, extract(DAY from b.SDATE) DAY, b.slat LAT, b.slong LON, b.SETNO, d.unit
                         from groundfish.gsdet a, groundfish.gsinf b, groundfish.gsmissions c, groundfish.gsarea2 d
                         where a.SPEC in ('10', '11', '16')
                         and d.unit like ('4%')
                         and c.season in ('SPRING', 'SUMMER')
                         and a.mission=b.mission 
                         and a.setno=b.setno
                         and b.mission=c.mission
                         and b.area=d.area")

dfo2<-ROracle::dbGetQuery(channel, "select c.year, c.season, a.spec, a.flen, a.age, extract(MONTH from b.SDATE) MONTH, extract(DAY from b.SDATE) DAY, b.slat LAT, b.slong LON, b.SETNO, d.unit
                         from groundfish.gsdet a, groundfish.gsinf b, groundfish.gsmissions c, groundfish.gsarea2 d
                         where a.SPEC in ('10', '11', '16')
                         and d.unit like ('5%')
                         and c.season in ('SPRING', 'SUMMER')
                         and a.mission=b.mission 
                         and a.setno=b.setno
                         and b.mission=c.mission
                         and b.area=d.area")

dfo <- rbind(dfo1, dfo2)
rm(dfo1, dfo2)

dfo$LAT<-with(dfo, as.numeric(substr(LAT,1,2))+(LAT - as.numeric(substr(LAT,1,2))*100)/60) #Converts DDMM to decimal degrees
dfo$LON<-with(dfo, as.numeric(substr(LON,1,2))+((LON - as.numeric(substr(LON,1,2))*100)/60)*-1) #Converts DDMM to decimal degrees
dfo$LON<-dfo$LON*-1
dfo <- dfo %>% mutate(DESCRIPTION=case_when(SEASON=="SPRING" ~ "DFO SPRING", SEASON=="SUMMER" ~ "DFO SUMMER"), UNIT=substr(case_when(grepl("5Z", UNIT) ~ "5Z",
                                                                                                                              grepl("4", UNIT) ~ UNIT), 1, 3)) %>%
               filter(!grepl("4T", UNIT))

#Binding all together
surveys <- rbind(dfo, nmfs)
rm(dfo, nmfs)

#Load port samples

port<-ROracle::dbGetQuery(channel, "select a.area, a.species SPEC, a.datelanded, b.fishlen FLEN, b.age, d.description GEAR from MFD_PORT_SAMPLES.gpsamples a, MFD_PORT_SAMPLES.gpages b,MFD_PORT_SAMPLES.GPUNIQ_AREA c, MFD_PORT_SAMPLES.GPUNIQ_GEAR d where species in (10, 11, 16) and a.sample like '20%' and begotol is not null and datelanded >= '01-JAN-00' and datelanded <= '31-DEC-23' and a.area between ('450') and ('539') and a.sample=b.sample and a.area=c.areacode and a.fishing=d.gearcode") 

port$YEAR<-with(port, as.numeric(substr(DATELANDED,1,4)))
port$MONTH<-with(port, as.numeric(substr(DATELANDED,6,7)))
port$DAY<-with(port, as.numeric(substr(DATELANDED,9,10)))

portareas <- ROracle::dbGetQuery(channel, "select a.areacode AREA, a.description UNIT from mfd_port_samples.gparea a 
                                 where a.areacode between ('450') and ('539')") %>% distinct() 

port <- left_join(port, portareas) %>% mutate(UNIT=substr(str_replace(UNIT, " ", ""), 1, 3)) %>%
  mutate(UNIT=case_when(UNIT%in%c("5YB","5YF") ~ "5Y",
                        UNIT=="5ZE" ~ "5Z",
                        UNIT!=c("5YB","5YF","5ZE") ~ UNIT)) %>%
                 mutate(SPEC=substr(SPEC, 2,3),
                        DESCRIPTION="PORT SAMPLE",
                        SETNO=NA,
                        LAT=NA,
                        LON=NA,
                        SEASON=case_when(MONTH%in%c(12,1,2) ~ "WINTER",
                                         MONTH%in%3:5 ~ "SPRING",
                                         MONTH%in%6:8 ~ "SUMMER",
                                         MONTH%in%9:11 ~ "WINTER")) %>%
  dplyr::select(YEAR, SEASON, SPEC, FLEN, AGE, MONTH, DAY, LAT, LON, SETNO, UNIT, DESCRIPTION)

rm(portareas)

#Load observer data

obs1<-ROracle::dbGetQuery(channel, "select a.landing_date, b.fishset_id SETNO, b.nafarea_id UNIT, c.gearcd_id GEAR, d.speccd_id SPEC, e.FISH_LENGTH FLEN, e.FISH_AGE AGE from observer.istrips a, observer.isfishsets b, observer.isgears c, observer.iscatches d, observer.isfish e where 
                                 a.trip_id=b.trip_id AND 
                                 b.gear_id=c.gear_id AND
                                 b.fishset_id=d.fishset_id AND
                                 d.catch_id=e.catch_id AND
                                 d.speccd_id in ('10', '11', '16') AND
                                 b.nafarea_id like ('5Z%')")

obs2<-ROracle::dbGetQuery(channel, "select a.landing_date, b.fishset_id SETNO, b.nafarea_id UNIT, c.gearcd_id GEAR, d.speccd_id SPEC, e.FISH_LENGTH FLEN, e.FISH_AGE AGE from observer.istrips a, observer.isfishsets b, observer.isgears c, observer.iscatches d, observer.isfish e where 
                                 a.trip_id=b.trip_id AND 
                                 b.gear_id=c.gear_id AND
                                 b.fishset_id=d.fishset_id AND
                                 d.catch_id=e.catch_id AND
                                 d.speccd_id in ('10', '11', '16') AND
                                 b.nafarea_id like ('4X%')")

obs <- rbind(obs1, obs2)
rm(obs1, obs2)

obs <- obs %>% mutate(GEAR=case_when(GEAR==12 ~ "STERN OTT TRWL",
                              GEAR==41 ~ "GILLNET",
                              GEAR%in%c(50,51) ~ "LONGLINE",
                              GEAR!=c(12,41,50,51) ~ "DFO_OTHER"),
               YEAR=substr(LANDING_DATE,1,4),
               MONTH=substr(LANDING_DATE,6,7),
               DAY=substr(LANDING_DATE,9,10),
               LAT=NA,
               LON=NA,
               SEASON=case_when(MONTH%in%c(12,1,2) ~ "WINTER",
                                MONTH%in%3:5 ~ "SPRING",
                                MONTH%in%6:8 ~ "SUMMER",
                                MONTH%in%9:11 ~ "WINTER"),
               DESCRIPTION="OBSERVER SAMPLE") %>%
  dplyr::select(YEAR, SEASON, SPEC, FLEN, AGE, MONTH, DAY, LAT, LON, SETNO, UNIT, DESCRIPTION) %>%
  filter(!is.na(YEAR), !is.na(AGE))

samples <- rbind(surveys, port, obs)
rm(surveys, port, obs)



###Part 2. Calculate Partial Age and Format Data  --------------------------------------------


samples <- samples %>% mutate(SPEC=case_when(SPEC=="10" ~ "COD",
                                             SPEC=="11" ~ "HADDOCK",
                                             SPEC=="16" ~ "POLLOCK",
                                             SPEC=="073" ~ "COD",
                                             SPEC=="074" ~ "HADDOCK",
                                             SPEC=="075" ~ "POLLOCK"))

#Creating Partial Age
samples$date<-with(samples, paste(YEAR, MONTH, sep="-"))
samples$date<-with(samples, paste(date, DAY, sep="-"))                   
samples$date<-with(samples, as.Date(date), format=c("%Y-%m-%d"))
samples$DATELANDED<-NULL
samples$yday<-yday(samples$date)

#Adjusted for a Feb 1 or a Jan 1 birthdate:
FebBD<-subset(samples, !DESCRIPTION%in%c('NMFS_FALL','NMFS_SPRING'))
JanBD<-subset(samples, DESCRIPTION%in%c('NMFS_FALL','NMFS_SPRING'))
FebBD$yday2<-with(FebBD, ifelse(yday>31, yday-31, yday-31+365))
JanBD$yday2<-JanBD$yday

#Bind back in
samples<-rbind(FebBD, JanBD)
samples$PartAge<-with(samples, AGE+yday2/365)

samples <- samples %>% mutate(YEARS=case_when(YEAR<1970 ~ "1960s",
                           YEAR%in%1970:1979 ~ "1970s",
                           YEAR%in%1980:1989 ~ "1980s",
                           YEAR%in%1990:1999 ~ "1990s",
                           YEAR%in%2000:2009 ~ "2000s",
                           YEAR%in%2010:2019 ~ "2010s",
                           YEAR>2019 ~ YEAR))
samples$YEARS <- as.factor(samples$YEARS)
#samples$UNIT <- as.factor(samples$UNIT)
#samples$UNIT <- factor(samples$UNIT, levels=rev(levels(samples$UNIT)))


###Part 3. Data Visualization --------------------------------------------

#5Z Cod >2009 All Data
ggplot(subset(samples, YEAR>2009 & SPEC=="COD" & grepl("5Z", UNIT)), aes(PartAge, FLEN))+
  geom_point(aes(col=YEARS),alpha=0.5)+
  theme_bw()+
  xlim(0,10)+
  ylim(0,150)+
  scale_x_continuous(breaks = c(0,1,2,3,4,5,6,7,8,9,10),limits = c(0,10))+
  geom_vline(xintercept=c(0,1,2,3,4,5,6,7,8,9,10), linetype=2)+
  scale_color_manual(values=c("lightgrey", "#440154FF", "#22A884FF", "#7AD151FF", "#FDE725FF" ))+
  ylab("Fish length (cm)")+
  xlab("Partial age")+
  theme(legend.title = element_blank())+
  ggtitle("5Z Cod >2009 All Data")

ggsave(here("figures/Survey_Growth_PartialAge_5ZCod_alldata.png"), width=10, height=8, units="in")

#5Z Cod >2009 Survey Data
ggplot(subset(samples, YEAR>2009 & SPEC=="COD" & DESCRIPTION%in%c("PORT SAMPLE", "OBSERVER SAMPLE") & grepl("5Z", UNIT)), aes(PartAge, FLEN))+
  geom_point(aes(col=YEARS),alpha=0.5)+
  theme_bw()+
  xlim(0,10)+
  ylim(0,150)+
  scale_x_continuous(breaks = c(0,1,2,3,4,5,6,7,8,9,10),limits = c(0,10))+
  geom_vline(xintercept=c(0,1,2,3,4,5,6,7,8,9,10), linetype=2)+
  scale_color_manual(values=c("lightgrey", "#440154FF", "#22A884FF", "#7AD151FF", "#FDE725FF" ))+
  ylab("Fish length (cm)")+
  xlab("Partial age")+
  theme(legend.title = element_blank())+
  ggtitle("5Z Cod >2009 Survey Data")

ggsave(here("figures/Survey_Growth_PartialAge_5ZCod_surveyonly.png"), width=10, height=8, units="in")

#5Z Faceted by Data Source
ggplot(samples%>%filter(YEAR>2009 & grepl("5Z", UNIT) & SPEC=="COD"), aes(PartAge, FLEN))+
  geom_point(aes(col=YEARS),alpha=0.5)+
  theme_bw()+
  xlim(0,10)+
  ylim(0,150)+
  scale_x_continuous(breaks = c(0,1,2,3,4,5,6,7,8,9,10),limits = c(0,10))+
  geom_vline(xintercept=c(0,1,2,3,4,5,6,7,8,9,10), linetype=2)+
  scale_color_manual(values=c("lightgrey", "#440154FF", "#22A884FF", "#7AD151FF", "#FDE725FF" ))+
  ylab("Fish length (cm)")+
  xlab("Partial age")+
  facet_grid(DESCRIPTION~.) +
  theme(legend.title = element_blank())+
  ggtitle("5Z Cod >2009 All Data")

ggsave(here("figures/Survey_Growth_PartialAge_5ZCod_facetdatasource.png"), width=10, height=8, units="in")

#5Z Cod, Haddock, Pollock >2009
ggplot(subset(samples, YEAR>2009 & grepl("5Z", UNIT)), aes(PartAge, FLEN))+
  geom_point(aes(col=YEARS),alpha=0.5)+
  theme_bw()+
  xlim(0,10)+
  ylim(0,150)+
  scale_x_continuous(breaks = c(0,1,2,3,4,5,6,7,8,9,10),limits = c(0,10))+
  geom_vline(xintercept=c(0,1,2,3,4,5,6,7,8,9,10), linetype=2)+
  scale_color_manual(values=c("lightgrey", "#440154FF", "#22A884FF", "#7AD151FF", "#FDE725FF" ))+
  ylab("Fish length (cm)")+
  xlab("Partial age")+
  facet_grid(~SPEC) +
  theme(legend.title = element_blank())+
  ggtitle("5Z Cod, Haddock, Pollock >2009 All Data")

ggsave(here("figures/Survey_Growth_PartialAge_5ZCod_groundfishcomp.png"), width=10, height=8, units="in")

#4X Cod >2009
ggplot(samples%>%filter(YEAR>2009 & grepl("4X", UNIT) & SPEC=="COD"), aes(PartAge, FLEN))+
  geom_point(aes(col=YEARS),alpha=0.5)+
  theme_bw()+
  xlim(0,10)+
  ylim(0,150)+
  scale_x_continuous(breaks = c(0,1,2,3,4,5,6,7,8,9,10),limits = c(0,10))+
  geom_vline(xintercept=c(0,1,2,3,4,5,6,7,8,9,10), linetype=2)+
  scale_color_manual(values=c("lightgrey", "#440154FF", "#22A884FF", "#7AD151FF", "#FDE725FF" ))+
  ylab("Fish length (cm)")+
  xlab("Partial age")+
  facet_wrap(~UNIT) +
  theme(legend.title = element_blank())+
  ggtitle("4X Cod >2009 All Data")

ggsave(here("figures/Survey_Growth_PartialAge_4Xcod_alldata.png"), width=10, height=8, units="in")

#4Xpqrs (BOF) faceted by data source
ggplot(samples%>%filter(YEAR>2009 & UNIT%in%c("4XP","4XQ","4XR","4XS") & SPEC=="COD"), aes(PartAge, FLEN))+
  geom_point(aes(col=YEARS),alpha=0.5)+
  theme_bw()+
  xlim(0,10)+
  ylim(0,150)+
  scale_x_continuous(breaks = c(0,1,2,3,4,5,6,7,8,9,10),limits = c(0,10))+
  geom_vline(xintercept=c(0,1,2,3,4,5,6,7,8,9,10), linetype=2)+
  scale_color_manual(values=c("lightgrey", "#440154FF", "#22A884FF", "#7AD151FF", "#FDE725FF" ))+
  ylab("Fish length (cm)")+
  xlab("Partial age")+
  facet_grid(DESCRIPTION~UNIT) +
  theme(legend.title = element_blank())+
  ggtitle("4X Cod >2009 All Data")

ggsave(here("figures/Survey_Growth_PartialAge_4Xpqrs_facetdatasource.png"), width=10, height=8, units="in")

#4W Cod >2009
ggplot(samples%>%filter(YEAR>2009 & grepl("4W", UNIT) & SPEC=="COD"), aes(PartAge, FLEN))+
  geom_point(aes(col=YEARS),alpha=0.5)+
  theme_bw()+
  xlim(0,10)+
  ylim(0,150)+
  scale_x_continuous(breaks = c(0,1,2,3,4,5,6,7,8,9,10),limits = c(0,10))+
  geom_vline(xintercept=c(0,1,2,3,4,5,6,7,8,9,10), linetype=2)+
  scale_color_manual(values=c("lightgrey", "#440154FF", "#22A884FF", "#7AD151FF", "#FDE725FF" ))+
  ylab("Fish length (cm)")+
  xlab("Partial age")+
  facet_wrap(~UNIT) +
  theme(legend.title = element_blank())+
  ggtitle("4W Cod >2009 All Data")

ggsave(here("figures/Survey_Growth_PartialAge_4Wcod_alldata.png"), width=10, height=8, units="in")


#Overlays

#2010-2019 faceted by area
samples2 <- samples%>%filter(YEAR%in%2010:2019 & (grepl("5Z", UNIT) | UNIT%in%c("4XP","4XQ","4XR","4XS")) & SPEC=="COD") %>% 
  mutate(UNIT=case_when(UNIT%in%c("5Z","5ZE","5ZJ","5ZM") ~ "5Z",
                         !UNIT%in%c("5Z","5ZE","5ZJ","5ZM") ~ UNIT))
samples2$UNIT <- as.factor(samples2$UNIT)
samples2$UNIT <- factor(samples2$UNIT, levels=rev(levels(samples2$UNIT)))

plot1 <- ggplot(data=samples2, aes(PartAge, FLEN))+
  geom_point(aes(col=UNIT),alpha=0.5)+
  theme_bw()+
  xlim(0,10)+
  ylim(0,150)+
  scale_x_continuous(breaks = c(0,1,2,3,4,5,6,7,8,9,10),limits = c(0,10))+
  geom_vline(xintercept=c(0,1,2,3,4,5,6,7,8,9,10), linetype=2)+
  scale_color_manual(values=c("lightgrey", "#440154FF", "#22A884FF", "#7AD151FF", "#FDE725FF" ))+
  ylab("Fish length (cm)")+
  xlab("Partial age")+
  ggtitle("2010-2020")

#2020-2023 faceted by area
samples3 <- samples%>%filter(YEAR%in%2020:2023 & (grepl("5Z", UNIT) | UNIT%in%c("4XP","4XQ","4XR","4XS")) & SPEC=="COD") %>% 
  mutate(UNIT=case_when(UNIT%in%c("5Z","5ZE","5ZJ","5ZM") ~ "5Z",
                        !UNIT%in%c("5Z","5ZE","5ZJ","5ZM") ~ UNIT))
samples3$UNIT <- as.factor(samples3$UNIT)
samples3$UNIT <- factor(samples3$UNIT, levels=rev(levels(samples3$UNIT)))

plot2 <- ggplot(data=samples3, aes(PartAge, FLEN))+
  geom_point(aes(col=UNIT),alpha=0.5)+
  theme_bw()+
  xlim(0,10)+
  ylim(0,150)+
  scale_x_continuous(breaks = c(0,1,2,3,4,5,6,7,8,9,10),limits = c(0,10))+
  geom_vline(xintercept=c(0,1,2,3,4,5,6,7,8,9,10), linetype=2)+
  scale_color_manual(values=c("lightgrey", "#440154FF", "#22A884FF", "#7AD151FF", "#FDE725FF" ))+
  ylab("Fish length (cm)")+
  xlab("Partial age")+
  ggtitle("2020-2023")

plot3 <- ggarrange(plot1, plot2, ncol=2, nrow=1, common.legend = TRUE, legend = "right")

annotate_figure(plot3, top = text_grob("Partial Ages for 5Z and 4Xpqrs Cod", 
                                      color = "black", face = "bold", size = 14))

ggsave(here("figures/Survey_Growth_PartialAge5Z4Xpqrs.png"), width=15, height=10, units="in")

#Trendline plots

samples4 <- samples %>% filter((grepl("4X", UNIT)) | (grepl("5Z", UNIT))) %>% 
  filter(DESCRIPTION%in%c("DFO SUMMER", "DFO SPRING")) %>%
  mutate(AREA=case_when(UNIT%in%c("4XM","4XN","4XO")~"SS",
                        UNIT%in%c("4XP","4XQ","4XR","4XS","5Y")~"BOF",
                        UNIT%in%c("5Z","5ZE","5ZJ","5ZM")~"5Z")) %>%
  mutate(YEARS=case_when(YEAR%in%2020:2023 ~ "2020-2023", !YEAR%in%2020:2023 ~ as.character(YEARS))) %>%
  group_by(YEARS, PartAge, AREA) %>%
  summarize(FLEN.m=mean(FLEN), FLEN.s=sd(FLEN), min=min(FLEN.m-1.96*FLEN.s), max=max(FLEN.m+1.96*FLEN.s))

plot4 <- ggplot(data=subset(samples4, PartAge<13))+
  geom_smooth(aes(PartAge, FLEN.m, col=factor(AREA), group=AREA), size=1)+
  #geom_line(aes(PartAge, max, col=factor(AREA)), linetype=2)+
  #geom_line(aes(PartAge, min, col=factor(AREA)), linetype=2)+
  ylim(0,150)+
  scale_x_discrete(name ="Age", limits=c('1','2','3','4','5','6','7','8','9','10','11','12')) + 
  ylab("Length (cm)") + 
  facet_wrap(~YEARS) +
  guides(col = guide_legend(title = "AREA")) +
  theme_bw() +
  scale_colour_viridis_d()
plot4

ggsave(here("figures/Survey_Growth_Smooth_BOFvSSv5Z.png"), width=10, height=8, units="in")

samples5 <- samples %>% filter((grepl("4X", UNIT)) | (grepl("5Z", UNIT))) %>% 
  filter(!UNIT%in%c("4XM", "4XN","4XO") & DESCRIPTION%in%c("DFO SUMMER", "DFO SPRING")) %>%
  mutate(AREA=case_when(UNIT%in%c("4XM","4XN","4XO")~"SS",
                        UNIT%in%c("4XP","4XQ","4XR","4XS","5Y")~"BOF",
                        UNIT%in%c("5Z","5ZE","5ZJ","5ZM")~"5Z")) %>%
  mutate(YEARS=case_when(YEAR%in%2020:2023 ~ "2020-2023", !YEAR%in%2020:2023 ~ as.character(YEARS))) %>%
  mutate(UNIT=case_when(UNIT%in%c("5Z","5ZE","5ZJ","5ZM") ~ "5Z",
                        !UNIT%in%c("5Z","5ZE","5ZJ","5ZM") ~ UNIT)) %>%
  group_by(YEARS, PartAge, UNIT) %>%
  summarize(FLEN.m=mean(FLEN), FLEN.s=sd(FLEN), min=min(FLEN.m-1.96*FLEN.s), max=max(FLEN.m+1.96*FLEN.s))

plot5 <- ggplot(data=subset(samples5, PartAge<13))+
    geom_smooth(aes(PartAge, FLEN.m, col=factor(UNIT)), size=1)+
    #geom_line(aes(AGE, max, col=factor(UNIT)), linetype=2)+
    #geom_line(aes(AGE, min, col=factor(UNIT)), linetype=2)+
    ylim(0,150)+
    scale_x_discrete(name ="Age", limits=c('1','2','3','4','5','6','7','8','9','10','11','12')) + 
    ylab("Length (cm)") + 
    facet_wrap(~YEARS) +
    guides(col = guide_legend(title = "UNIT")) +
    theme_bw() +
    scale_colour_viridis_d()
plot5

ggsave(here("figures/Survey_Growth_Smooth_4Xpqrs5Z.png"), width=10, height=8, units="in")


#Look at 5Z at a finer scale - j vs m vs n etc.

sf::sf_use_s2(FALSE)

samparea <- Mar.utils::identify_area(df=samples, lat.field = "LAT", lon.field = "LON",
                                   agg.poly.shp = "S:/Science/Population Ecology/Georges Bank/Useful R-scripts/Mapping Data/MaritimesRegionEcosystemAssessmentStrata(2014-)NAD83.shp",
                                   agg.poly.field = "id")


#Use Survey data only for now

test <- samples %>% filter(SPEC=="COD" & !is.na(LAT) & UNIT=="5Z")
test$YEAR <- as.integer(test$YEAR)
test$SETNO <- as.integer(test$SETNO)

dfostrata<-ROracle::dbGetQuery(channel, "select c.year, c.season, b.setno, b.strat
                         from groundfish.gsinf b, groundfish.gsmissions c, groundfish.gsarea2 d
                         where c.season in ('SPRING', 'SUMMER')
                         and b.mission=c.mission
                         and b.area=d.area")

samples6 <- left_join(test, dfostrata) %>% filter(STRAT%in%c("5Z1","5Z2","5Z3","5Z4","5Z5","5Z6","5Z7","5Z8","5Z9")) %>%
  mutate(YEARS=case_when(YEAR%in%2020:2023 ~ "2020-2023", !YEAR%in%2020:2023 ~ as.character(YEARS))) %>%
  group_by(YEARS, PartAge, STRAT) %>%
  summarize(FLEN.m=mean(FLEN), FLEN.s=sd(FLEN), min=min(FLEN.m-1.96*FLEN.s), max=max(FLEN.m+1.96*FLEN.s))

plot6 <- ggplot(data=subset(samples6, PartAge<13))+
  geom_smooth(aes(PartAge, FLEN.m, col=factor(STRAT)), size=1, se=FALSE)+
  #geom_line(aes(AGE, max, col=factor(UNIT)), linetype=2)+
  #geom_line(aes(AGE, min, col=factor(UNIT)), linetype=2)+
  ylim(0,150)+
  scale_x_discrete(name ="Age", limits=c('1','2','3','4','5','6','7','8','9','10','11','12')) + 
  ylab("Length (cm)") + 
  facet_wrap(~YEARS) +
  guides(col = guide_legend(title = "STRATA")) +
  theme_bw() +
  scale_colour_viridis_d(option="turbo")
plot6

ggsave(here("figures/Survey_Growth_Smooth_5Z19.png"), width=10, height=8, units="in")
