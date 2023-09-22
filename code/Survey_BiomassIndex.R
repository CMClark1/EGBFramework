#############################
# 5Z Cod
# Survey Biomass Index
# July 1, 2021
# Author: Caira Clark and Irene Andrushchenko
#############################

require(dplyr)
require(ggplot2)
require(here)
require(tidyr)

#PART 1. REGULAR SCALED BIOMASS INDEX

#Load data. There are two groups depending on how the conversion factor is treated.
biomass <- read.csv(here("data/surveybiomass.csv"))
biomass1 <- biomass %>% filter(year<2009)
biomass2 <- biomass %>% filter(year>2008)

#Convert NMFS Fall and NMFS Spring
biomass1$cnmfsfall <- biomass1$nmfsfall * biomass1$nfallconv
biomass1$cnmfsspr <- biomass1$nmfsspr * biomass1$nsprconv
biomass2$cnmfsfall <- biomass2$nmfsfall / biomass2$nfallconv
biomass2$cnmfsspr <- biomass2$nmfsspr / biomass2$nsprconv

#Bind them together
biomass <- rbind(biomass1, biomass2)

#Calculate standard converted biomass for each survey
biomass1986 <-biomass %>% 
  filter(year > 1985) %>%
  dplyr::select(year, dfospr.egb, dfospr.gb, cnmfsfall, cnmfsspr)
biomass1986[is.na(biomass1986)] <- 0

biomass[is.na(biomass)] <- 0
biomass$cnmfsfall_mean <- mean(subset(biomass1986, year>1985&cnmfsfall>0)$cnmfsfall)
biomass$cnmfsspr_mean <- mean(subset(biomass1986, year>1985&cnmfsspr>0)$cnmfsspr)
biomass$dfospregb_mean <- mean(subset(biomass1986, year>1985&dfospr.egb>0)$dfospr.egb)
biomass$dfosprgb_mean <- mean(subset(biomass1986, year>1985&dfospr.gb>0)$dfospr.gb)

#These are the standard converted biomass calculations
biomass$NMFSFall.EGB <- biomass$cnmfsfall/biomass$cnmfsfall_mean
biomass$NMFSSpring.EGB <- biomass$cnmfsspr/biomass$cnmfsspr_mean
biomass$DFO.EGB <- biomass$dfospr.egb/biomass$dfospregb_mean
biomass$DFO.GB <- biomass$dfospr.gb/biomass$dfosprgb_mean

#Pivot into long format
biomass_long <- biomass %>%
  dplyr::select(year, NMFSFall.EGB, NMFSSpring.EGB, DFO.EGB, DFO.GB) %>%
  pivot_longer(!year, names_to = "Survey", values_to = "biomass")
biomass_long[biomass_long == 0] <- NA
biomass_long$FACET <- "Total Biomass (std)"

#PART 2. MEAN SCALED BIOMASS INDEX - WHOLE GB NMFS ONLY

biomass2 <- read.csv(here("data/nmfs_biomass.csv"))
colnames(biomass2)[1] <- "INDEX"
#biomass2 <- biomass2 %>% filter(YEAR!=2020)

means <- subset(biomass2, YEAR>1986&!is.na(INDEX_KG)) %>% group_by(INDEX) %>% summarise(mean(INDEX_KG))
biomass2 <- left_join(biomass2, means)

#These are the standard converted biomass calculations
biomass2$STANDARDIZED <- biomass2$INDEX_KG/biomass2$`mean(INDEX_KG)`

biomass2 <- biomass2 %>% select(YEAR, INDEX, STANDARDIZED)
colnames(biomass2)[1] <- "year"
colnames(biomass2)[2] <- "Survey"
colnames(biomass2)[3] <- "biomass"
biomass2$FACET <- "Mean Kg/Tow (std)"

biomass_long <- rbind(biomass_long, biomass2)


#Plot
ggplot(biomass_long) +
  geom_line(aes(x=year, y=biomass, group=Survey, color=Survey)) +
  geom_point(aes(x=year, y=biomass, group=Survey, colour = Survey),size=1) + 
  theme_bw() +
  xlab("Year") +
  ylab("Scaled Biomass Index") +
  scale_colour_viridis_d(option="turbo", end=0.8)+
  facet_wrap(~FACET, scale="free_y")

ggsave(here("figures/Survey_ScaledBiomassIndex.png"), width=10, height=5, units="in")

#Adding a variable for spatial coverage
biomass_long$Area<-with(biomass_long, ifelse(grepl("EGB",Survey),'EGB','GB'))
biomass_long$Entity<-with(biomass_long, ifelse(grepl("DFO",Survey),'DFO','NMFS'))
biomass_long$Season<-with(biomass_long, ifelse(grepl("Spring",Survey),'Spring','Fall'))
biomass_long$Season<-with(biomass_long, ifelse(grepl("DFO",Survey), 'Spring', Season))
biomass_long$EntSea<-with(biomass_long, paste(Entity, Season, sep=""))

#Plot
ggplot(biomass_long) +
  geom_line(aes(x=year, y=biomass, group=EntSea, color=EntSea)) +
  geom_point(aes(x=year, y=biomass, group=EntSea, colour = EntSea),size=1) + 
  theme_bw() +
  xlab("Year") +
  ylab("Scaled Biomass Index") +
  scale_colour_viridis_d(option="turbo", end=0.8)+
  facet_wrap(~Area, scale="free_y")+ylim(0,7)+
  geom_smooth(aes(x=year, y=biomass, group=EntSea, colour = EntSea), se=FALSE, size=1.2)

ggsave(here("figures/Survey_ScaledBiomassIndex_EGBvGB.png"), width=10, height=5, units="in")

#NMFS only

nmfsonly <- biomass_long%>%
  filter(grepl("NMFS", Survey)) %>% 
  mutate(SEASON=case_when(grepl("Fall", Survey) ~ "FALL", grepl("Spring", Survey) ~ "SPRING"),
         Survey=case_when(grepl("EGB", Survey) ~ paste(Survey, "Total Biomass (std)"), !grepl("EGB", Survey) ~ paste(Survey,"Mean kg/tow (std)")))



ggplot(nmfsonly) +
  geom_line(aes(x=year, y=biomass, group=Survey, color=Survey)) +
  geom_point(aes(x=year, y=biomass, group=Survey, colour = Survey),size=1) + 
  theme_bw() +
  xlab("Year") +
  ylab("Scaled Biomass Index") +
  scale_colour_viridis_d(option="turbo", end=0.8)+
  facet_wrap(~SEASON, scale="free_y")

ggsave(here("figures/Survey_ScaledBiomassIndex_NMFSonly.png"), width=10, height=5, units="in")

#Alternative calculation for NMFS EGB

CAA_nmfsspr <- read.csv(here("data/Survey CAA/nmfsspr_survey_caa.csv"))
CAA_nmfsspr$SURVEY <- "NMFS SPRING"
CAA_nmfsfall <- read.csv(here("data/Survey CAA/nmfsfall_survey_caa.csv")) #this stranal file should have the conversion factor selected.
CAA_nmfsfall$SURVEY <- "NMFS FALL"
CAA <- rbind(CAA_nmfsspr, CAA_nmfsfall)
CAA <- CAA %>% pivot_longer(!c(Year, SURVEY), names_to="AGE", values_to="CAA") %>% mutate(AGE=substr(AGE,2,3))

WAA_nmfsspr <- read.csv(here("data/Survey CAA/nmfsspr_survey_waa.csv"))
WAA_nmfsspr$SURVEY <- "NMFS SPRING"
WAA_nmfsfall <- read.csv(here("data/Survey CAA/nmfsfall_survey_waa.csv"))
WAA_nmfsfall$SURVEY <- "NMFS FALL"
WAA <- rbind(WAA_nmfsspr, WAA_nmfsfall)
WAA <- WAA %>% pivot_longer(!c(Year, SURVEY), names_to="AGE", values_to="WAA") %>% mutate(AGE=substr(AGE,2,3))

combined <- full_join(CAA, WAA)
combined$biomass <- combined$CAA*combined$WAA

ready <- combined %>% distinct() %>% select(Year, SURVEY, AGE, biomass) %>% pivot_wider(c(Year,AGE), names_from=SURVEY, values_from=biomass)
colnames(ready)[3] <- "NMFSSPRING"
colnames(ready)[4] <- "NMFSFALL"

write.csv(ready, here("data/NMFSEGB_Biomass_calculated.csv"))
