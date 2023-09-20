#############################
# 5Z Cod
# Survey - Slow Growing Fish
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

channel <- ROracle::dbConnect(DBI::dbDriver("Oracle"), username=oracle.username, password=oracle.password, oracle.dsn) 

#Survey

#NMFS Data Pulls

nmfs_fall<-ROracle::dbGetQuery(channel, "SELECT B.Year, a.SVSPP SPEC, A.LENGTH FLEN, A.AGE, C.EST_MONTH MONTH, C.EST_DAY DAY, C.DECDEG_BEGLAT LAT, C.DECDEG_BEGLON LON, C.TOW SETNO
                                   FROM USNEFSC.USS_MSTR_CRUISE B INNER JOIN (USNEFSC.USS_STATION C INNER JOIN USNEFSC.USS_DETAIL A ON (C.CRUISE6 = A.CRUISE6) AND (C.STRATUM = A.STRATUM) AND (C.TOW = A.TOW) AND (C.STATION = A.STATION)) ON B.CRUISE6 = A.CRUISE6
                                   WHERE (((A.LENGTH) Is Not Null) AND ((B.SEASON)='FALL') AND ((A.SVSPP) in ('073','074','075')) AND ((B.PURPOSE_CODE)='10') AND ((C.AREA) In ('551','552','561','562', '522', '525')) AND ((C.STRATUM) In ('01160','01170','01180','01190','01200','01210','01220')))
                                   ORDER BY B.Year, C.CRUISE6
                                   ")
nmfs_fall$DESCRIPTION <-'NMFS FALL'


nmfs_spring<-ROracle::dbGetQuery(channel, "SELECT B.Year, a.SVSPP SPEC, A.LENGTH FLEN, A.AGE, C.EST_MONTH MONTH, C.EST_DAY DAY, C.DECDEG_BEGLAT LAT, C.DECDEG_BEGLON LON, C.TOW SETNO
                                   FROM USNEFSC.USS_MSTR_CRUISE B INNER JOIN (USNEFSC.USS_STATION C INNER JOIN USNEFSC.USS_DETAIL A ON (C.CRUISE6 = A.CRUISE6) AND (C.STRATUM = A.STRATUM) AND (C.TOW = A.TOW) AND (C.STATION = A.STATION)) ON B.CRUISE6 = A.CRUISE6
                                   WHERE (((A.LENGTH) Is Not Null) AND ((B.SEASON)='SPRING') AND ((A.SVSPP) in ('073','074','075')) AND ((B.PURPOSE_CODE)='10') AND ((C.AREA) In ('551','552','561','562', '522', '525')) AND ((C.STRATUM) In ('01160','01170','01180','01190','01200','01210','01220')))
                                   ORDER BY B.Year, C.CRUISE6
                                   ")
nmfs_spring$DESCRIPTION<-'NMFS SPRING'

#DFO Data Pulls
dfo<-ROracle::dbGetQuery(channel, "select c.year, a.spec, a.flen, a.age, extract(MONTH from b.SDATE) MONTH, extract(DAY from b.SDATE) DAY, b.slat LAT, b.slong LON, b.SETNO
                         from groundfish.gsdet a, groundfish.gsinf b, groundfish.gsmissions c, groundfish.gsarea2 d
                         where a.SPEC in ('10', '11', '16')
                         and d.unit in ('5ZEJ', '5ZEM', '5ZEH', '5ZEN')
                         and c.season in ('SPRING')
                         and a.mission=b.mission 
                         and a.setno=b.setno
                         and b.mission=c.mission
                         and b.area=d.area")

dfo$LAT<-with(dfo, as.numeric(substr(LAT,1,2))+(LAT - as.numeric(substr(LAT,1,2))*100)/60) #Converts DDMM to decimal degrees
dfo$LON<-with(dfo, as.numeric(substr(LON,1,2))+((LON - as.numeric(substr(LON,1,2))*100)/60)*-1) #Converts DDMM to decimal degrees
dfo$LON<-dfo$LON*-1
dfo$DESCRIPTION<-'DFO SPRING'

dfo1<-ROracle::dbGetQuery(channel, "select c.year, a.spec, a.flen, a.age, extract(MONTH from b.SDATE) MONTH, extract(DAY from b.SDATE) DAY, b.slat LAT, b.slong LON, b.SETNO
                         from groundfish.gsdet a, groundfish.gsinf b, groundfish.gsmissions c, groundfish.gsarea2 d
                         where a.SPEC in ('10', '11', '16')
                         and d.unit in ('5ZEJ', '5ZEM', '5ZEH', '5ZEN')
                         and c.season in ('SUMMER')
                         and a.mission=b.mission 
                         and a.setno=b.setno
                         and b.mission=c.mission
                         and b.area=d.area")

dfo1$LAT<-with(dfo1, as.numeric(substr(LAT,1,2))+(LAT - as.numeric(substr(LAT,1,2))*100)/60) #Converts DDMM to decimal degrees
dfo1$LON<-with(dfo1, as.numeric(substr(LON,1,2))+((LON - as.numeric(substr(LON,1,2))*100)/60)*-1) #Converts DDMM to decimal degrees
dfo1$LON<-dfo1$LON*-1
dfo1$DESCRIPTION<-'DFO SUMMER'

#Binding all together
samples <- rbind(dfo, dfo1, nmfs_fall, nmfs_spring)

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

#Calculate partial age
samples$PartAge<-with(samples, AGE+yday2/365)

#Since 2010
samples$YEARS<-with(samples, ifelse(YEAR<2021&YEAR>2009, '2010-2020',NA))
samples$YEARS<-with(samples, ifelse(YEAR>2020, '2021',YEARS))
samples$YEARS<-with(samples, ifelse(YEAR>2021, '2022',YEARS))
samples$YEARS<-with(samples, ifelse(YEAR>2022, '2023',YEARS))

samples1<-samples %>% 
  arrange(YEARS) %>% 
  filter(YEAR>2009)

#5Z cod
ggplot(subset(samples1, YEAR>2009 & SPEC=="COD"), aes(PartAge, FLEN))+
  geom_point(aes(col=YEARS),alpha=0.5)+
  theme_bw()+
  xlim(0,10)+
  ylim(0,150)+
  scale_x_continuous(breaks = c(0,1,2,3,4,5,6,7,8,9,10),limits = c(0,10))+
  geom_vline(xintercept=c(0,1,2,3,4,5,6,7,8,9,10), linetype=2)+
  scale_color_manual(values=c("lightgrey", "#00BA38", "#619CFF", "black" ))+
  ylab("Fish length (cm)")+
  xlab("Partial age")+
  facet_grid(~SPEC) +
  theme(legend.title = element_blank())

#5Z cod haddock pollock
ggplot(subset(samples1, YEAR>2009), aes(PartAge, FLEN))+
  geom_point(aes(col=YEARS),alpha=0.5)+
  theme_bw()+
  xlim(0,10)+
  ylim(0,150)+
  scale_x_continuous(breaks = c(0,1,2,3,4,5,6,7,8,9,10),limits = c(0,10))+
  geom_vline(xintercept=c(0,1,2,3,4,5,6,7,8,9,10), linetype=2)+
  scale_color_manual(values=c("lightgrey", "#00BA38", "#619CFF", "black" ))+
  ylab("Fish length (cm)")+
  xlab("Partial age")+
  facet_grid(~SPEC) +
  theme(legend.title = element_blank())

#Add additional 4VWX data

dfo<-ROracle::dbGetQuery(channel, "select c.year, a.spec, a.flen, a.age, extract(MONTH from b.SDATE) MONTH, extract(DAY from b.SDATE) DAY, b.slat LAT, b.slong LON, b.SETNO, d.unit
                         from groundfish.gsdet a, groundfish.gsinf b, groundfish.gsmissions c, groundfish.gsarea2 d
                         where a.SPEC in ('10', '11', '16')
                         and d.unit like ('4%')
                         and c.season in ('SPRING')
                         and a.mission=b.mission 
                         and a.setno=b.setno
                         and b.mission=c.mission
                         and b.area=d.area")

dfo$DESCRIPTION<-'DFO SPRING'

dfo1<-ROracle::dbGetQuery(channel, "select c.year, a.spec, a.flen, a.age, extract(MONTH from b.SDATE) MONTH, extract(DAY from b.SDATE) DAY, b.slat LAT, b.slong LON, b.SETNO, d.unit
                         from groundfish.gsdet a, groundfish.gsinf b, groundfish.gsmissions c, groundfish.gsarea2 d
                         where a.SPEC in ('10', '11', '16')
                         and d.unit like ('4%')
                         and c.season in ('SUMMER')
                         and a.mission=b.mission 
                         and a.setno=b.setno
                         and b.mission=c.mission
                         and b.area=d.area")

dfo1$DESCRIPTION<-'DFO SUMMER'

dfo4X <- rbind(dfo, dfo1)

dfo4X$LAT<-with(dfo4X, as.numeric(substr(LAT,1,2))+(LAT - as.numeric(substr(LAT,1,2))*100)/60) #Converts DDMM to decimal degrees
dfo4X$LON<-with(dfo4X, as.numeric(substr(LON,1,2))+((LON - as.numeric(substr(LON,1,2))*100)/60)*-1) #Converts DDMM to decimal degrees
dfo4X$LON<-dfo4X$LON*-1

dfo4X$date<-with(dfo4X, paste(YEAR, MONTH, sep="-"))
dfo4X$date<-with(dfo4X, paste(date, DAY, sep="-"))                   
dfo4X$date<-with(dfo4X, as.Date(date), format=c("%Y-%m-%d"))
dfo4X$DATELANDED<-NULL
dfo4X$yday<-yday(dfo4X$date)

#Adjusted for a Feb 1 or a Jan 1 birthdate:
FebBD<-subset(dfo4X, !DESCRIPTION%in%c('NMFS_FALL','NMFS_SPRING'))
JanBD<-subset(dfo4X, DESCRIPTION%in%c('NMFS_FALL','NMFS_SPRING'))
FebBD$yday2<-with(FebBD, ifelse(yday>31, yday-31, yday-31+365))
JanBD$yday2<-JanBD$yday

#Bind back in
dfo4X_backup<-dfo4X #Samples backup is with all data adjusted to Feb 1
dfo4X_backup$yday2<-with(dfo4X_backup, ifelse(yday>31, yday-31, yday-31+365))
dfo4X<-rbind(FebBD, JanBD)

dfo4X$PartAge<-with(dfo4X, AGE+yday2/365)
dfo4X_backup$PartAge<-with(dfo4X_backup, AGE+yday2/365)

dfo4X <- dfo4X %>% mutate(SPEC=case_when(SPEC=="10" ~ "COD",
                                             SPEC=="11" ~ "HADDOCK",
                                             SPEC=="16" ~ "POLLOCK")) %>%
  filter(YEAR>2009) %>%
  mutate(YEARS=case_when(YEAR%in%2010:2020 ~ "2010-2020",
                         YEAR==2021 ~ "2021",
                         YEAR==2022 ~ "2022",
                         YEAR==2023 ~ "2023"),
         NAFO=substr(UNIT, 1, 3)) %>%
  select(-UNIT)

samples1$NAFO <- "5Z"

allareas <- rbind(dfo4X, samples1)

#Faceted by species and NAFO area
ggplot(subset(allareas, YEAR>2009 & !NAFO%in%c("4VN","4VS", "4WD", "4WE", "4WF", "4WG", "4WH", "4WJ", "4WK", "4WL", "5Z")), aes(PartAge, FLEN))+
  geom_point(aes(col=YEARS),alpha=0.5)+
  theme_bw()+
  xlim(0,10)+
  ylim(0,150)+
  scale_x_continuous(breaks = c(0,1,2,3,4,5,6,7,8,9,10),limits = c(0,10))+
  geom_vline(xintercept=c(0,1,2,3,4,5,6,7,8,9,10), linetype=2)+
  scale_color_manual(values=c("lightgrey", "#00BA38", "#619CFF", "black" ))+
  ylab("Fish length (cm)")+
  xlab("Partial age")+
  facet_grid(NAFO~SPEC) +
  theme(legend.title = element_blank())

#Check if the pattern in 4Xpqrs (BOF) is there in port sample and observer data too

#Port Samples
samples<-ROracle::dbGetQuery(channel, "select a.sample, a.port, a.area,a.TRIP_NUMBER, datelanded, b.fishlen, b.age, d.description, a.remark from MFD_PORT_SAMPLES.gpsamples a, MFD_PORT_SAMPLES.gpages b,MFD_PORT_SAMPLES.GPUNIQ_AREA c, MFD_PORT_SAMPLES.GPUNIQ_GEAR d where species=10 and a.sample like '20%' and begotol is not null and datelanded >= '01-JAN-00' and datelanded <= '31-DEC-23' and a.area in (464,465,466,467) and a.sample=b.sample and a.area=c.areacode and a.fishing=d.gearcode group by a.sample, b.fishlen, b.age, a.port, a.area, a.TRIP_NUMBER, datelanded, c.description, d.description, a.remark order by a.sample") 

samples<-subset(samples, select=c('FISHLEN', 'AGE', 'DESCRIPTION', 'DATELANDED', 'AREA'))
samples$YEAR<-with(samples, as.numeric(substr(DATELANDED,1,4)))
samples$MONTH<-with(samples, as.numeric(substr(DATELANDED,6,7)))
samples$DAY<-with(samples, as.numeric(substr(DATELANDED,9,10)))

samples <- samples %>% filter(YEAR>2009) %>% mutate(NAFO=case_when(AREA==464 ~ "4XP",
                                  AREA==465 ~ "4XQ",
                                  AREA==466 ~ "4XR",
                                  AREA==467 ~ "4XS")) %>% 
                              mutate(YEARS=case_when(YEAR%in%2010:2020 ~ "2010-2020",
                                                     YEAR==2021 ~ "2021",
                                                     YEAR==2022 ~ "2022",
                                                     YEAR==2023 ~ "2023"))

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
samples_backup<-samples #Samples backup is with all data adjusted to Feb 1
samples_backup$yday2<-with(samples_backup, ifelse(yday>31, yday-31, yday-31+365))
samples<-rbind(FebBD, JanBD)

samples$PartAge<-with(samples, AGE+yday2/365)
samples_backup$PartAge<-with(samples_backup, AGE+yday2/365)


#Add 4X survey data back in

head(dfo4X)
head(samples1)

dfo4X1 <- dfo4X %>% filter(SPEC=="COD") %>% select(FLEN, AGE, DESCRIPTION, NAFO, YEAR, MONTH, DAY, YEARS, date, yday, yday2, PartAge) %>% filter(!is.na(AGE))
colnames(dfo4X1)[1] <- "FISHLEN"

samples1$NAFO <- "5Z"

samples2 <- rbind(samples1%>%filter(SPEC=="COD")%>%select(FLEN,AGE,DESCRIPTION,NAFO,YEAR,MONTH,DAY,YEARS,date,yday,yday2,PartAge)%>%rename(FISHLEN=FLEN), dfo4X1)

samples2 <- samples2 %>% mutate(YEARS=case_when(YEAR%in%2010:2014~ "2010-2014",
                                    YEAR%in%2015:2019~ "2015-2019",
                                    YEAR%in%2020:2023~ "2020-2023"))
#create convex hulls
hull_data <- 
  samples2 %>%
  filter(YEAR>2010 & NAFO%in%c("4XP", "4XQ", "4XR", "4XS", "5Z")) %>%
  drop_na() %>%
  group_by(YEARS, NAFO) %>% 
  slice(chull(PartAge, FISHLEN))

#no hull
p <- ggplot(subset(samples2, YEAR>2010 & NAFO%in%c("4XP", "4XQ", "4XR", "4XS", "5Z")), aes(PartAge, FISHLEN))+
  geom_point(aes(col=NAFO),alpha=0.5)+
  theme_bw()+
  xlim(0,10)+
  ylim(0,150)+
  scale_x_continuous(breaks = c(0,1,2,3,4,5,6,7,8,9,10),limits = c(0,10))+
  geom_vline(xintercept=c(0,1,2,3,4,5,6,7,8,9,10), linetype=2)+
  scale_color_manual(values=c("lightgrey", "#00BA38", "#619CFF", "yellow","black" ))+
  ylab("Fish length (cm)")+
  xlab("Partial age")+
  theme(legend.title = element_blank())
p

#hull
p2 <- ggplot(subset(samples2, YEAR>2010 & NAFO%in%c("4XP", "4XQ", "4XR", "4XS", "5Z")), aes(PartAge, FISHLEN))+
  geom_point(aes(col=NAFO),alpha=0.5)+
  theme_bw()+
  xlim(0,10)+
  ylim(0,150)+
  scale_x_continuous(breaks = c(0,1,2,3,4,5,6,7,8,9,10),limits = c(0,10))+
  geom_vline(xintercept=c(0,1,2,3,4,5,6,7,8,9,10), linetype=2)+
  scale_color_manual(values=c("lightgrey", "#00BA38", "#619CFF", "yellow","black" ))+
  ylab("Fish length (cm)")+
  xlab("Partial age")+
  facet_grid(~YEARS) +
  geom_polygon(data = hull_data,
               aes(fill = NAFO,
                   colour = NAFO),
               alpha = 0.3)
p2


#Overlay 5Z and 4X

a5Z4X <- rbind(samples, samples2)

ggplot(subset(a5Z4X, YEAR>2009 & NAFO%in%c("4XP", "4XQ", "4XR", "4XS", "5Z")), aes(PartAge, FISHLEN))+
  geom_point(aes(col=NAFO),alpha=0.5)+
  theme_bw()+
  xlim(0,10)+
  ylim(0,150)+
  scale_x_continuous(breaks = c(0,1,2,3,4,5,6,7,8,9,10),limits = c(0,10))+
  geom_vline(xintercept=c(0,1,2,3,4,5,6,7,8,9,10), linetype=2)+
  #scale_color_manual(values=c("lightgrey", "#00BA38", "#619CFF", "black" ))+
  ylab("Fish length (cm)")+
  xlab("Partial age")+
  #facet_grid(NAFO~.) +
  theme(legend.title = element_blank())


#Determine boundary between fast/slow growing fish

samples2 <- samples1 %>% select(YEAR, YEARS, FLEN, AGE, PartAge)
samples2$YEARS<-with(samples2, ifelse(YEARS=="2010-2020", YEARS, "2021-2023"))

samples2$log.age <- log(samples2$PartAge)
samples2$log.length <- log(samples2$FLEN)
colnames(samples2)[3] <- "FLEN"
samples2$ID <- factor("SURVEY")
samples2[is.na(samples2) | samples2=="Inf" | samples2=="-Inf"] = NA

samples3 <- samples2 %>% mutate(FWT=1, FSEX=1, STRAT=1) %>% select(ID, YEARS, FLEN, FWT, FSEX, YEAR, STRAT,log.length, log.age)

seplines2 <- ddply(samples3, "YEARS",
                   function(mydf){
                     nsamp <- nrow(mydf)
                     fit <- lm( log.length ~ log.age, data=mydf)
                     summary<-summary(fit)
                     mycoef1 <- summary$coefficients[1,1]
                     intercept<-exp(mycoef1)
                     se1 <- summary$coefficients[1,2]
                     if (length(summary$coefficients)<5){
                       mycoef2=NA
                       se2=NA
                     } else {
                       mycoef2 <- summary$coefficients[2,1]
                       se2<-summary$coefficients[2,2]
                     }
                     rsq <-summary$r.squared
                     rse<-summary$sigma
                     
                     myres <- c(nsamp, mycoef1,intercept,mycoef2, se1,se2, rsq,rse)
                     names(myres) <- c("Nsamp","ln.intercept","intercept","slope", "se.intercept", "se.slope", "r_square", "resid_error")
                     return(myres)
                   })
output.all<-seplines2


PRED<-data.frame(1:10)
colnames(PRED)[1] <- "AGE"
PRED$smooth1020 <- 21.42993*(PRED$AGE^0.7132990) #2010-2020
PRED$smooth2023 <- 24.86945*(PRED$AGE^0.6691998) #2020-2023
PRED <- PRED%>%pivot_longer(!AGE, names_to="ID", values_to ="values")

ggplot(samples2, aes(PartAge, FLEN))+
  geom_point(aes(col=YEARS),alpha=0.5)+
  geom_path(data=PRED, aes(x=AGE, y=values, group=ID, colour=ID), size=1) +
  theme_bw()+
  xlim(0,10)+
  ylim(0,150)+
  geom_vline(xintercept=c(0,1,2,3,4,5,6,7,8,9,10), linetype=2)+
  ylab("Fish length (cm)")+
  xlab("Partial age")+
  theme(legend.title = element_blank())


#Use minimum 2021-2023 value with smoother

sampcopy <- samples %>% filter(YEAR>2009)
sampcopy$YEARS<-with(sampcopy, ifelse(YEARS=="2010-2020", YEARS, "2021-2023"))

minlen <- sampcopy %>% filter(YEARS!="<NA>") %>% group_by(YEARS, round(PartAge)) %>% dplyr::summarise(min=mean(FLEN)-sd(FLEN))
colnames(minlen)[2] <- "roundPartAge"

ggplot(sampcopy, aes(PartAge, FLEN))+
  geom_point(aes(col=YEARS),alpha=0.5)+
  geom_path(data=minlen, aes(x=roundPartAge, y=min, group=YEARS, colour=YEARS), size=1) +
  theme_bw()+
  xlim(0,10)+
  ylim(0,150)+
  geom_vline(xintercept=c(0,1,2,3,4,5,6,7,8,9,10), linetype=2)+
  ylab("Fish length (cm)")+
  xlab("Partial age")+
  theme(legend.title = element_blank())

slowfish <- minlen %>% filter(YEARS=="2010-2020") #slow growing fish have a length at age less than the mean length at partial age minus one standard deviation 2010-2020
slowfish <- as.data.frame(slowfish)
slowfish <- slowfish %>% select(roundPartAge, min)
colnames(slowfish) <- c("Age", "Length")

#So, where are these small fish located?

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
SSstrat14.df2$lat2 <- ifelse (SSstrat14.df2$lat > 46, 46, ifelse (SSstrat14.df2$lat < 40, 40, SSstrat14.df2$lat))
SSstrat14.df2$long2 <- ifelse (SSstrat14.df2$long > -63, -63, ifelse (SSstrat14.df2$long < -69, -69, SSstrat14.df2$long))

input <- "NAFO_SubUnits_CanAtlantic" #No extension
NAFO.shp <- readOGR (".", input)
names (NAFO.shp)
NAFO.df2 <- fortify (NAFO.shp, region = "UnitArea")
NAFO.df2$lat2 <- ifelse (NAFO.df2$lat > 46, 46, ifelse (NAFO.df2$lat < 40, 40, NAFO.df2$lat))
NAFO.df2$long2 <- ifelse (NAFO.df2$long > -63.0, -63.0, ifelse (NAFO.df2$long < -69.0, -69.0, NAFO.df2$long))

input <- "Can-USBorder" #No extension
Border.shp <- readOGR (".", input)
names (Border.shp)
Border.df <- fortify (Border.shp, region = "BoundID")
Border.df2 <- Border.df [order (Border.df$lat),] 
Border.df2$lat2 <- ifelse (Border.df2$lat > 46, 46, ifelse (Border.df2$lat < 40, 40, Border.df2$lat))
Border.df2$long2 <- ifelse (Border.df2$long > -63, -63, ifelse (Border.df2$long < -69, -69, Border.df2$long))
Border.df3 <- subset(Border.df2, Border.df2[ , 2] > 40.5) 
Border.df3$long2 <- ifelse (Border.df3$long2 > -66.0, -66.146, Border.df3$long2)

head(samples)
head(smallfish)

test <- samples %>% filter(YEAR>2009 & !is.na(AGE)) %>%
  mutate(YEARS=case_when(YEAR<2020 ~ "2010-2020",
                          YEAR>2019 ~ "2020-2023"))
colnames(smallfish)[2] <- "AGE"

test2 <- left_join(test, smallfish)

test3 <- test2 %>% mutate(GROWTH=case_when(FLEN>min ~ "fast",
                               FLEN<min ~ "slow"))


#Location of aged fish assigned fast/slow growing on all surveys
map1<-
  ggplot(test3%>%filter(!is.na(GROWTH)), aes(x=LON, y=LAT, colour=GROWTH)) +
  geom_point() +
  scale_fill_viridis_d(option="inferno") +
  geom_polygon (data = SSstrat14.df2, aes (x = long2, y = lat2, group = group), colour = "black", fill = NA, size = 0.4) +
  geom_polygon (data = NAFO.df2, aes (x = long2, y = lat2, group = group), colour = "black", fill = NA, size = 0.4) +
  geom_path (data = Border.df2, aes (x = long2, y = lat2), colour = "black", linetype = "dashed", linewidth = 0.8) +
  coord_map () + 
  theme_bw () + 
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank()) + 
  scale_y_continuous ("Latitude", expand = c (0,0), limits = c (40, 43)) + 
  scale_x_continuous ( "Longitude", expand = c (0,0), limits = c (-69, -63)) +
  theme (axis.title = element_text (size = 9), 
         axis.text = element_text (size = 8), 
         legend.text = element_text (size = 8), 
         legend.title = element_text (size = 9)) +
  facet_grid(YGROUP~DESCRIPTION)
map1 

##PROPORTION IN THE SET OF LARGE/sMALL

smallfish2 <- as.data.frame(smallfish)
smallfish2 <- smallfish2 %>% select(AGE, min)

#Additional data from 4X

head(dfo4X)
head(samples)

allsamples <- rbind(dfo4X, samples)

prop1 <- allsamples %>% filter(!is.na(AGE)) %>%
  mutate(YEARS=case_when(YEAR%in%1970:1979 ~ "1970s",
                         YEAR%in%1980:1989 ~ "1980s",
                         YEAR%in%1990:1999 ~ "1990s",
                         YEAR%in%2000:2009 ~ "2000s",
                         YEAR%in%2010:2019 ~ "2010s",
                         YEAR>2019 ~ "2020-2023")) %>%
  left_join(smallfish2) %>%
  mutate(GROWTH=case_when(FLEN>min ~ "fast",
                         FLEN<min ~ "slow")) %>% 
  group_by(YEARS, YEAR, DESCRIPTION, SETNO, LAT, LON, GROWTH) %>% 
  tally() %>% 
  mutate(total=sum(n)) %>%
  filter(GROWTH=="slow") %>% 
  mutate(prop=n/total) %>%
  arrange(DESCRIPTION, YEARS, YEAR, SETNO) %>%
  group_by(DESCRIPTION, YEARS, round(LAT, 1), round(LON, 1)) %>%
  dplyr::summarise(prop=mean(prop)) %>%
  dplyr::rename(c("LAT" = `round(LAT, 1)`, "LON" = `round(LON, 1)`,))

map2<-
  ggplot(prop1%>%filter(DESCRIPTION%in%c("DFO SPRING", "DFO SUMMER")), aes(x=LON, y=LAT)) +
  geom_tile(aes(fill=prop)) +
  #geom_density_2d_filled(aes(colour=prop)) +
  #geom_point(aes(colour=prop, size=prop)) +
  scale_fill_viridis(option="rocket", direction = -1) +
  geom_polygon (data = SSstrat14.df2, aes (x = long2, y = lat2, group = group), colour = "black", fill = NA, size = 0.4) +
  geom_polygon (data = NAFO.df2, aes (x = long2, y = lat2, group = group), colour = "black", fill = NA, size = 0.4) +
  geom_path (data = Border.df2, aes (x = long2, y = lat2), colour = "black", linetype = "dashed", linewidth = 0.8) +
  coord_map () + 
  theme_bw () + 
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank()) + 
  scale_y_continuous ("Latitude", expand = c (0,0), limits = c (40, 46)) + 
  scale_x_continuous ( "Longitude", expand = c (0,0), limits = c (-69, -63)) +
  theme (axis.title = element_text (size = 9), 
         axis.text = element_text (size = 8), 
         legend.text = element_text (size = 8), 
         legend.title = element_text (size = 9)) +
  facet_grid(DESCRIPTION~YEARS)
map2
