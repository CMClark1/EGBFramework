
#############################
# 5Z Cod
# Prey and Predation
# September 2023
# Author: Caira Clark
#############################

library(ROracle);library(ggplot2);library(viridis);library(dplyr);library(ggpubr);library(here)

#Run the script that loads the shapefiles for maps
source("S:/Science/Population Ecology/Georges Bank/Useful R-scripts/LoadShapefiles.R")

channel <- ROracle::dbConnect(DBI::dbDriver("Oracle"), username=oracle.username, password=oracle.password, oracle.dsn) 

#SANDLANCE--------------------------------------------

sandlance<-ROracle::dbGetQuery(channel, "select c.year, c.season, b.setno, b.strat, b.slat, b.slong, a.totwgt
                               from groundfish.gscat a, groundfish.gsinf b, groundfish.gsmissions c
                               where a.SPEC=610 
                               and c.season in ('SPRING', 'SUMMER')
                               and a.mission=b.mission 
                               and a.setno=b.setno
                               and a.mission=c.mission
                               and a.setno=b.setno 
                               and b.type=1")

sandlance$LAT<-with(sandlance, as.numeric(substr(SLAT, 1,2))+as.numeric(substr(SLAT,3,4))/60)
sandlance$LON<-with(sandlance, as.numeric(substr(SLONG, 1,2))+as.numeric(substr(SLONG,3,4))/60)
sandlance$LON<-sandlance$LON*-1

sandlance <- sandlance %>% mutate(YEARS=case_when(YEAR<1970 ~ "1960s",
                                     YEAR%in%1970:1979 ~ "1970s",
                                     YEAR%in%1980:1989 ~ "1980s",
                                     YEAR%in%1990:1999 ~ "1990s",
                                     YEAR%in%2000:2009 ~ "2000s",
                                     YEAR%in%2010:2019 ~ "2010s",
                                     YEAR>2019 ~ "2020+"))


ggplot(data=sandlance, aes(x=LON, y=LAT)) +
  scale_fill_viridis(option="A") +
  geom_point(aes(colour=TOTWGT)) +
  #geom_polygon (data = SSstrat14.df2_fil, aes (x = long2, y = lat2, group = group, fill=meantemp), linewidth = 0.4) +
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

ggsave(here("figures/Survey_Env_SandlanceLocation.png"), width=10, height=20, units="in")


#DIET DATA--------------------

predator <- read.csv(here("data/Diet/FH.Cod.Predator.Data.4VWX5Z.Sept.2023.csv"))
PreySpeciesCodes <- read.csv(here("data/Diet/Survey_diet_PreySpeciesCodes.csv"))

predator2 <- left_join(predator, PreySpeciesCodes%>%select(PREYSPECCD, COMM))

predator2 <- predator %>% mutate(YEAR=as.numeric(substr(predator$MISSION,4,7)),
                    LIFESTAGE=case_when(FLEN<41 ~ 'JUVENILE',
                                    FLEN>40 ~ 'ADULT')) %>% #This is based on L50 for 5Z
  filter(!NAFO_ZONE%in%c('4VN','4VS')) %>%
  filter(NAFO_SUBUNIT%in%c('4XP','4XQ','4XR','4XS')) %>%
  left_join(PreySpeciesCodes)




PreySpeciesCodes2<-subset(PreySpeciesCodes, PREYSPECCD%in%c(predator$PREYSPECCD))
PreySpeciesCodes2<-subset(PreySpeciesCodes2, select=c('PREYSPECCD','fish'))
predator2<-merge(predator2, PreySpeciesCodes2, all.x=TRUE)
round(xtabs(PWT~fish, predator2)/sum(predator2$PWT, na.rm=TRUE),2)

diet4X$fish<-with(diet4X, ifelse(fish%in%c('Amphipod','Fish','Haddock','Hake','Herring','Krill','Sandlance','Shrimp','Squid'), fish, 'DigestedRemains'))
temp<-aggregate(PWT~fish+PolStage+YearGroup, diet4X, FUN="sum")
library(ggplot2)
ggplot(subset(temp), aes(x="", y=PWT, fill=factor(fish)))+geom_bar(width=1, stat="identity", position="fill")+facet_grid(PolStage~YearGroup)+ coord_polar("y", start=0)+theme_bw()+guides(fill=guide_legend(title="Prey Item"))+theme(axis.text.x = element_blank())

#Don't ave an L50 for 4VW; just used the 4X value intead
diet4VW$PolStage<-with(diet4VW, ifelse(FLEN<40.8, 'JUV','ADULT'))
diet4VW$PolStage<-factor(diet4VW$PolStage, level=c('JUV','ADULT'))
diet4VW$YearGroup<-with(diet4VW, ifelse(YEAR<2010, '1999-2009','2010-2019'))

PreySpeciesCodes2<-subset(PreySpeciesCodes, PREYSPECCD%in%c(diet4VW$PREYSPECCD))
PreySpeciesCodes2<-subset(PreySpeciesCodes2, select=c('PREYSPECCD','fish'))
diet4VW<-merge(diet4VW, PreySpeciesCodes2, all.x=TRUE)
round(xtabs(PWT~fish, diet4VW)/sum(diet4VW$PWT, na.rm=TRUE),2)

diet4VW$fish<-with(diet4VW, ifelse(fish%in%c('Amphipod','Crustacean','Fish','Haddock','Hake','Herring','Krill','Myctophid','Redfish','Sandlance','Shrimp','Squid'), fish, 'DigestedRemains'))
temp<-aggregate(PWT~fish+PolStage+YearGroup, diet4VW, FUN="sum")

ggplot(subset(temp), aes(x="", y=PWT, fill=factor(fish)))+geom_bar(width=1, stat="identity", position="fill")+facet_grid(PolStage~YearGroup)+ coord_polar("y", start=0)+theme_bw()+guides(fill=guide_legend(title="Prey Item"))+theme(axis.text.x = element_blank())


#Using 4X L50 as well
diet$PolStage<-with(diet, ifelse(FLEN<40.8, 'JUV','ADULT'))
diet$PolStage<-factor(diet$PolStage, level=c('JUV','ADULT'))
diet$YearGroup<-with(diet, ifelse(YEAR<2010, '1999-2009','2010-2019'))

PreySpeciesCodes2<-subset(PreySpeciesCodes, PREYSPECCD%in%c(diet$PREYSPECCD))
PreySpeciesCodes2<-subset(PreySpeciesCodes2, select=c('PREYSPECCD','fish'))
diet<-merge(diet, PreySpeciesCodes2, all.x=TRUE)
round(xtabs(PWT~fish, diet)/sum(diet$PWT, na.rm=TRUE),2)

diet$fish<-with(diet, ifelse(fish%in%c('Amphipod','Crustacean','Fish','Haddock','Hake','Herring','Krill','Redfish','Sandlance','Shrimp','Squid'), fish, 'DigestedRemains'))
temp<-aggregate(PWT~fish+PolStage+YearGroup, diet, FUN="sum")
library(ggplot2)
ggplot(subset(temp), aes(x="", y=PWT, fill=factor(fish)))+geom_bar(width=1, stat="identity", position="fill")+facet_grid(PolStage~YearGroup)+ coord_polar("y", start=0)+theme_bw()+guides(fill=guide_legend(title="Prey Item"))+theme(axis.text.x = element_blank())


