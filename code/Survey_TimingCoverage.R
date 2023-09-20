####################
# 5Z Cod Survey Timing
# Caira Clark
# 16 August 2023
####################

library(ROracle)
library(ggplot2)
library(tidyr)
library(dplyr)
library(lubridate)
library(here)

channel <- ROracle::dbConnect(DBI::dbDriver("Oracle"), username=oracle.username, password=oracle.password, oracle.dsn) 

#DFO SPRING SURVEY-----------------------------------

##Set Types (Standard, Comparative, etc.)------------------------------

settype <- dbGetQuery(channel, "select a.mission, a.season, a.year, b.type from 
                    groundfish.gsmissions a, groundfish.gsinf b
                    where a.season in ('SPRING')
                    and a.year between ('1986') and ('2023')
                    and a.mission=b.mission")

settype %>% group_by(MISSION, YEAR, TYPE) %>% dplyr::summarise(n=n()) %>% arrange(YEAR) %>% pivot_wider(names_from = TYPE, values_from = n)

rm(settype)

#Survey Dates over time


dates1 <- dbGetQuery(channel, "select a.mission, a.year, c.unit,
                    to_char(b.sdate, 'yyyy-mm-dd') from 
                    groundfish.gsmissions a, groundfish.gsinf b, groundfish.gsarea2 c
                    where a.season in ('SPRING')
                    and a.year between ('1986') and ('2023')
                    and b.type in ('1')
                    and b.area=c.area
                    and a.mission=b.mission")

dates2 <- dbGetQuery(channel, "select a.mission, a.year, c.unit,
                    to_char(b.sdate, 'yyyy-mm-dd') from 
                    groundfish.gsmissions a, groundfish.gsinf b, groundfish.gsarea2 c
                    where a.season in ('SPRING')
                    and a.year in ('2022')
                    and b.type in ('5')
                    and b.area=c.area
                    and a.mission=b.mission")

dates <- rbind(dates1, dates2)

rm(dates1, dates2)

colnames(dates)[4] ="SDATE"
dates$DATE <- yday(dates$SDATE)
dates$MONTH <- as.factor(month(ymd(dates$SDATE)))

test <- dates %>% group_by(YEAR, MONTH, DATE, UNIT) %>% dplyr::summarize(n=n()) 
test$YEAR <- as.factor(test$YEAR)
test$UNIT <- substr(test$UNIT, start = 1, stop = 2)

minmax <- dates %>% group_by(YEAR) %>% 
  dplyr::summarize(start = min(DATE), end = max(DATE)) %>%
  gather(key=date_type, value=date, -YEAR)

minmax$YEAR <- as.factor(minmax$YEAR)

plot1 <- ggplot() +
  geom_hline(yintercept = 31, linetype="dotted", size=1) +
  geom_hline(yintercept = 58, linetype="dotted", size=1) +
  geom_hline(yintercept = 89, linetype="dotted", size=1) +
  geom_line(data=minmax, mapping=aes(x=YEAR, y=date), size=5, colour="grey") +
  geom_point(data=test, mapping=aes(x=YEAR, y=DATE, colour=UNIT)) +
  scale_colour_viridis_d(begin=0.4) +
  coord_flip() +
  theme_bw() +
  xlab("Survey Year")+
  ylab("Day of the Year") +
  annotate("text", x=39, y=43, label = "February") +
  annotate("text", x=39, y=72, label = "March") +
  annotate("text", x=39, y=95, label = "April") +
  annotate("text", x=40, y=95, label = " ") +
  ggtitle("DFO Spring Survey")
  #xlim(1970, 2023)
plot1

#ggsave(here("figures/Survey_DFO_Timing.png"),width=6, height=8, units="in")
  
##Survey Coverage--------------------------

cov1 <- dbGetQuery(channel, "select a.year, b.strat, b.area, b.slat, b.slong, c.totwgt from 
                    groundfish.gsmissions a, groundfish.gsinf b, groundfish.gscat c
                    where a.season in ('SPRING')
                    and a.year between ('1986') and ('2023')
                    and b.area in ('523', '524', '522', '525')
                    and c.spec in ('10')
                    and b.type in ('1')
                    and a.mission=b.mission
                    and b.mission=c.mission")

cov2 <- dbGetQuery(channel, "select a.year, b.strat, b.area, b.slat, b.slong, c.totwgt from 
                    groundfish.gsmissions a, groundfish.gsinf b, groundfish.gscat c
                    where a.season in ('SPRING')
                    and a.year in ('2022')
                    and b.area in ('523', '524', '522', '525')
                    and c.spec in ('10')
                    and b.type in ('5')
                    and a.mission=b.mission
                    and b.mission=c.mission")

cov <- rbind(cov1, cov2)

rm(cov1, cov2)

cov$LATITUDE = (as.numeric(substr(cov$SLAT,1,2))+(cov$SLAT - as.numeric(substr(cov$SLAT,1,2))*100)/60)
cov$LONGITUDE = (as.numeric(substr(cov$SLONG,1,2))+(cov$SLONG - as.numeric(substr(cov$SLONG,1,2))*100)/60)*-1

cov <- cov %>%   mutate(
  year_group = dplyr::case_when(
    YEAR <= 1995            ~ "1986-1995",
    YEAR > 1995 & YEAR <= 2005 ~ "1996-2005",
    YEAR > 2005 & YEAR <= 2015 ~ "2006-2015",
    YEAR > 2015             ~ "2016+")) %>%
  mutate(
    AREA = dplyr::case_when(
      AREA %in% c(523, 524)           ~ "EGB",
      AREA %in% c(522, 525)           ~ "WGB"))


# Mapping Portion

#Importing shape files
setwd("S:/Science/Population Ecology/Georges Bank/Useful R-scripts/Mapping Data")
# Strata boundaries
library(rgdal)
library(rgeos)
library(gpclib)
library('maptools')#problems encountered here. Persmissions issue with next line isn't run -> solution found on https://groups.google.com/g/ggplot2/c/pFhd_7EtTQY

#File import
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
Border.df3$long2 <- ifelse (Border.df3$long2 > -66.0, -66.146, Border.df3$long2)

# setting shape type and size for geom_point
shapes <- c( 3, 16, 16, 16, 16)
sizes <- c (1, 1, 2, 3, 4)

map<-
  ggplot () +
  geom_polygon (data = SSstrat14.df2, aes (x = long2, y = lat2, group = group), colour = "dodgerblue1", fill = NA, size = 0.4) +
  geom_polygon (data = NAFO.df2, aes (x = long2, y = lat2, group = group), colour = "black", fill = NA, size = 0.4) +
  geom_path (data = Border.df2, aes (x = long2, y = lat2), colour = "black", linetype = "dashed", linewidth = 0.8) +
  geom_point (data = cov, aes (x = LONGITUDE, y = LATITUDE)) +
  scale_shape_manual (values = shapes, drop = FALSE) + scale_size_manual (values = sizes, drop = FALSE) +
  coord_map () + 
  theme_bw () + 
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank()) + 
  scale_y_continuous ("Latitude", expand = c (0,0), limits = c (40, 43)) + 
  scale_x_continuous ( "Longitude", expand = c (0,0), limits = c (-70, -65)) +
  theme (axis.title = element_text (size = 9), 
         axis.text = element_text (size = 8), 
         legend.text = element_text (size = 8), 
         legend.title = element_text (size = 9)) + 
  theme (legend.position = "right") +
  facet_wrap(year_group~.)

map

#ggsave(here("figures/Survey_DFO_SetLocations.png"),width=7, height=6, units="in")

##Sets per Strata Table----------------------------

data <- dbGetQuery(channel, "select a.year, b.strat, b.setno from 
                    groundfish.gsmissions a, groundfish.gsinf b
                    where a.season in ('SPRING')
                    and a.year between ('1986') and ('2023')
                    and b.strat in ('5Z1', '5Z2', '5Z3', '5Z4', '5Z5', '5Z6', '5Z7', '5Z8', '5Z9')
                    and b.type in ('1')
                    and a.mission=b.mission")

data2 <- dbGetQuery(channel, "select a.year, b.strat, b.setno from 
                    groundfish.gsmissions a, groundfish.gsinf b
                    where a.season in ('SPRING')
                    and a.year in ('2022')
                    and b.strat in ('5Z1', '5Z2', '5Z3', '5Z4', '5Z5', '5Z6', '5Z7', '5Z8', '5Z9')
                    and b.type in ('5')
                    and a.mission=b.mission")

data <- rbind(data, data2)

summary <- data %>% group_by(YEAR, STRAT) %>% tally()
names(summary)[names(summary) == 'n'] <- 'SETS'

summary <- summary %>%
  mutate(
  planned = dplyr::case_when(
    STRAT=='5Z1' ~ 10,
    STRAT=='5Z2' ~ 35,
    STRAT=='5Z3' ~ 15,
    STRAT=='5Z4' ~ 15,
    STRAT=='5Z5' ~ 5,
    STRAT=='5Z6' ~ 12,
    STRAT=='5Z7' ~ 5,
    STRAT=='5Z8' ~ 3,
    STRAT=='5Z9' ~ 4)) %>%
  mutate(status = dplyr::case_when(
    SETS>=(planned*0.8) ~ "Complete",
    SETS<(planned*0.8) ~ "Incomplete"
  ))


ggplot(data=summary) + 
  geom_point(aes(x=STRAT,y=YEAR, size=SETS, colour=status)) +
  coord_flip() +
  theme_bw() +
  xlab("STRATA") +
  scale_colour_viridis_d(name="STATUS", end=0.8, option="turbo")

setwd("~/LocalRespository/EGBFramework")
ggsave(here("figures/Survey_DFO_SetsperStrata.png"), height=5, width=10, units="in")

summary %>% select(YEAR, STRAT, SETS) %>% pivot_wider(names_from = STRAT, values_from = SETS)
summary %>% select(YEAR, STRAT, planned) %>% pivot_wider(names_from = STRAT, values_from = planned)
print(summary %>% select(YEAR, STRAT, status) %>% filter(status=="Complete") %>% pivot_wider(names_from = STRAT, values_from = status), n=100)

#Number of days at sea per year

days <- dbGetQuery(channel, "select a.year, b.strat, b.setno, to_char(b.sdate, 'yyyy-mm-dd') from 
                    groundfish.gsmissions a, groundfish.gsinf b
                    where a.season in ('SPRING')
                    and a.year between ('1986') and ('2023')
                    and b.strat in ('5Z1', '5Z2', '5Z3', '5Z4', '5Z5', '5Z6', '5Z7', '5Z8', '5Z9')
                    and b.type in ('1')
                    and a.mission=b.mission")

colnames(days)[4] <- "DATE"

days$DATE <- as.integer(yday(days$DATE))

days <- days %>% 
  group_by(YEAR, DATE) %>% tally() %>% #number of sets on each day in each year
  group_by(YEAR) %>% tally() #number of days fished in each year

print(days$n)


#NMFS SPRING SURVEY-------------------------

#Survey Dates over time

dates <- dbGetQuery(channel, "select a.cruise6, a.est_year, a.est_month, a.begin_est_towdate, a.area from
                     usnefsc.uss_station a, usnefsc.uss_mstr_cruise b
                     where b.season in ('SPRING')
                     and b.purpose_code in ('10')
                     and a.est_year between ('1970') and ('2023')
                     and a.statype in ('1')
                     and a.shg < ('137')
                     and a.cruise6=b.cruise6
                     and a.area in ('522', '525', '561', '562', '551', '552')")

colnames(dates)[4] ="SDATE"
colnames(dates)[2] ="YEAR"
colnames(dates)[3] ="MONTH"
dates$DATE <- yday(dates$SDATE)
dates$MONTH <- as.numeric(dates$MONTH)
dates$YEAR <- as.numeric(dates$YEAR)

test <- dates %>% group_by(YEAR, MONTH, DATE, AREA) %>% dplyr::summarize(n=n()) 
test$YEAR <- as.factor(test$YEAR)
test$AREA <- as.factor(test$AREA)

#WHOLE SURVEY MINMAX
minmax <- dates1 %>% group_by(YEAR) %>% 
  dplyr::summarize(start = STARTDATE, end = ENDDATE) %>%
  gather(key=date_type, value=date, -YEAR) %>%
  distinct()
minmax$date <- yday(minmax$date)
minmax$YEAR <- as.factor(minmax$YEAR)

#ONLY GB MINMAX
minmax <- dates %>% group_by(YEAR) %>% 
  dplyr::summarize(start = yday(min(SDATE)), end = yday(max(SDATE))) %>%
  gather(key=date_type, value=date, -YEAR) %>%
  distinct()
minmax$YEAR <- as.factor(minmax$YEAR)

plot2 <- ggplot() +
  geom_hline(yintercept = 89, linetype="dotted", size=1) +
  geom_hline(yintercept = 129, linetype="dotted", size=1) +
  geom_line(data=minmax, mapping=aes(x=YEAR, y=date), size=4, colour="grey") +
  geom_point(data=test, mapping=aes(x=YEAR, y=DATE, colour=AREA)) +
  scale_colour_viridis_d(begin=0.4) +
  coord_flip() +
  theme_bw() +
  xlab("Survey Year")+
  ylab("Day of the Year") +
  annotate("text", x=45, y=78, label = "March") +
  annotate("text", x=45, y=110, label = "April") +
  annotate("text", x=45, y=140, label = "May") +
  annotate("text", x=46, y=110, label = " ") +
  ggtitle("NMFS Spring Survey")
plot2

#ggsave(here("figures/Survey_NMFSSpring_Timing.png"),width=6, height=8, units="in")

#Sets per Strata---------------

data <- dbGetQuery(channel, "select a.cruise6, a.est_year, a.area, a.tow from
                     usnefsc.uss_station a, usnefsc.uss_mstr_cruise b
                     where b.season in ('SPRING')
                     and b.purpose_code in ('10')
                     and a.est_year between ('1970') and ('2023')
                     and a.statype in ('1')
                     and a.shg < ('137')
                     and a.cruise6=b.cruise6
                     and a.area in ('522', '525', '561', '562', '551', '552')")

colnames(data)[2] <- "YEAR"
data$AREA <- as.factor(data$AREA)

summary <- data %>% group_by(YEAR, AREA) %>% tally()
names(summary)[names(summary) == 'n'] <- 'SETS'
summary$YEAR <- as.numeric(summary$YEAR)

ggplot(data=summary) + 
  geom_point(aes(x=AREA,y=YEAR, size=SETS)) +
  coord_flip() +
  theme_bw() +
  xlab("STRATA") +
  scale_colour_viridis_d(name="STATUS", end=0.8, option="turbo")

ggsave(here("figures/Survey_NMFSSpring_SetsperStrata.png"), height=5, width=10, units="in")

#NMFS FALL SURVEY-------------------------

#Survey Dates over time

dates1 <- dbGetQuery(channel, "select a.cruise6, a.year, a.startdate, a.enddate from
                     usnefsc.uss_mstr_cruise a
                     where a.season in ('FALL')
                     and a.purpose_code in ('10')
                     and a.year between ('1970') and ('2023')")

dates <- dbGetQuery(channel, "select a.cruise6, a.est_year, a.est_month, a.begin_est_towdate, a.area from
                     usnefsc.uss_station a, usnefsc.uss_mstr_cruise b
                     where b.season in ('FALL')
                     and b.purpose_code in ('10')
                     and a.est_year between ('1970') and ('2023')
                     and a.statype in ('1')
                     and a.shg < ('137')
                     and a.cruise6=b.cruise6
                     and a.area in ('522', '525', '561', '562', '551', '552')")

colnames(dates)[4] ="SDATE"
colnames(dates)[2] ="YEAR"
colnames(dates)[3] ="MONTH"
dates$DATE <- yday(dates$SDATE)
dates$MONTH <- as.numeric(dates$MONTH)
dates$YEAR <- as.numeric(dates$YEAR)

test <- dates %>% group_by(YEAR, MONTH, DATE, AREA) %>% dplyr::summarize(n=n()) 
test$YEAR <- as.factor(test$YEAR)
test$AREA <- as.factor(test$AREA)

#WHOLE SURVEY MINMAX
minmax <- dates1 %>% group_by(YEAR) %>% 
  dplyr::summarize(start = STARTDATE, end = ENDDATE) %>%
  gather(key=date_type, value=date, -YEAR) %>%
  distinct()
minmax$date <- yday(minmax$date)
minmax$YEAR <- as.factor(minmax$YEAR)

#ONLY GB MINMAX
minmax <- dates %>% group_by(YEAR) %>% 
  dplyr::summarize(start = yday(min(SDATE)), end = yday(max(SDATE))) %>%
  gather(key=date_type, value=date, -YEAR) %>%
  distinct()
minmax$YEAR <- as.factor(minmax$YEAR)

plot3 <- ggplot() +
  geom_hline(yintercept = 273, linetype="dotted", size=1) +
  geom_hline(yintercept = 304, linetype="dotted", size=1) +
  geom_line(data=minmax, mapping=aes(x=YEAR, y=date), size=4, colour="grey") +
  geom_point(data=test, mapping=aes(x=YEAR, y=DATE, colour=AREA)) +
  scale_colour_viridis_d(begin=0.4) +
  coord_flip() +
  theme_bw() +
  xlab("Survey Year")+
  ylab("Day of the Year") +
  annotate("text", x=45, y=267, label = "Sept.") +
  annotate("text", x=45, y=290, label = "Oct.") +
  annotate("text", x=45, y=315, label = "Nov.") +
  annotate("text", x=46, y=310, label = " ") +
  ggtitle("NMFS Fall Survey")
plot3

#ggsave(here("figures/Survey_NMFSSpring_Timing.png"),width=6, height=8, units="in")

library(ggpubr)
ggarrange(plot1, plot2, plot3, ncol=3, nrow=1, legend="bottom")

ggsave(here("figures/Survey_Timing.png"), width=11, height=8.5, units="in")

#Sets per Strata---------------

data <- dbGetQuery(channel, "select a.cruise6, a.est_year, a.area, a.tow from
                     usnefsc.uss_station a, usnefsc.uss_mstr_cruise b
                     where b.season in ('SPRING')
                     and b.purpose_code in ('10')
                     and a.est_year between ('1970') and ('2023')
                     and a.statype in ('1')
                     and a.shg < ('137')
                     and a.cruise6=b.cruise6
                     and a.area in ('522', '525', '561', '562', '551', '552')")

colnames(data)[2] <- "YEAR"
data$AREA <- as.factor(data$AREA)

summary <- data %>% group_by(YEAR, AREA) %>% tally()
names(summary)[names(summary) == 'n'] <- 'SETS'
summary$YEAR <- as.numeric(summary$YEAR)

ggplot(data=summary) + 
  geom_point(aes(x=AREA,y=YEAR, size=SETS)) +
  coord_flip() +
  theme_bw() +
  xlab("STRATA") +
  scale_colour_viridis_d(name="STATUS", end=0.8, option="turbo")

ggsave(here("figures/Survey_NMFSSpring_SetsperStrata.png"), height=5, width=10, units="in")
