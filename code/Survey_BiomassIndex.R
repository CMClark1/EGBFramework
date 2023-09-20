#############################
# 5Z Cod
# Survey Biomass Index
# July 1, 2021
# Author: Caira Clark
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
biomass$cnmfsfall_mean <- mean(biomass1986$cnmfsfall)
biomass$cnmfsspr_mean <- mean(biomass1986$cnmfsspr)
biomass$dfospregb_mean <- mean(biomass1986$dfospr.egb)
biomass$dfosprgb_mean <- mean(biomass1986$dfospr.gb)

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
biomass2 <- biomass2 %>% filter(YEAR!=2020)

means <- biomass2 %>% group_by(INDEX) %>% summarise(mean(INDEX_KG))
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
