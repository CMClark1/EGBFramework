#############################
# 5Z Cod
# Survey Weight at Age
# July 1, 2021
# Author: Caira Clark
#############################

require(ggplot2)
require(dplyr)
require(tidyr)
require(here)
require(ggpubr)

##DFO Spring Survey---------------

WAA_dfo <- read.csv(here("data/Survey CAA/dfo_survey_waa.csv"))

WAA_dfo$'a10+' <- WAA_dfo$a10 + WAA_dfo$a11 + WAA_dfo$a12 + WAA_dfo$a13 + WAA_dfo$a14 + WAA_dfo$a15 + WAA_dfo$a16

#Select columns and tidy data into long format
WAA_dfo2 <- WAA_dfo %>% dplyr::select(Year, a0, a1, a2, a3, a4, a5, a6, a7, a8, a9, 'a10+') %>%
  pivot_longer(!c(Year), names_to = "Age", values_to = "Weight")

#Finish tidying
WAA_dfo2$Age <- sub("a", "", WAA_dfo2$Age)
WAA_dfo2$Age[which(WAA_dfo2$Age == "10+")] = 10
WAA_dfo2$Age <- as.numeric(WAA_dfo2$Age)


#Plot all ages
WAA_dfo2[is.na(WAA_dfo2)] <- 0
WAA_dfo2$Year <- as.numeric(WAA_dfo2$Year)
ggplot(WAA_dfo2, aes(Year, Weight)) +
  geom_line(colour="blue") +
  geom_point(colour = "blue") + 
  ylab("Weight (kg)")+
  theme_bw()

#Plot ages 2-7, faceted by age
WAA_dfo2[WAA_dfo2 == 0] <- NA
dfo_waa_egb <- ggplot(subset(WAA_dfo2, Age %in% c(2:7)), aes(Year, Weight)) +
  geom_line(colour="blue") +
  geom_point(colour = "blue") + 
  facet_grid(~Age) +
  ylab("Weight (kg)")+
  theme_bw() +
  ggtitle("DFO Weight at Age") +
  xlim(1970, 2023)

##NMFS Spring Survey-----------------------------

WAA_nmfsspr <- read.csv(here("data/Survey CAA/nmfsspr_survey_waa.csv"))

WAA_nmfsspr$'a10+' <- WAA_nmfsspr$a10 + WAA_nmfsspr$a11 + WAA_nmfsspr$a12 + WAA_nmfsspr$a13 + WAA_nmfsspr$a14 + WAA_nmfsspr$a15 + WAA_nmfsspr$a16

#Select columns and tidy data into long format
WAA_nmfsspr2 <- WAA_nmfsspr %>% dplyr::select(Year, a0, a1, a2, a3, a4, a5, a6, a7, a8, a9, 'a10+') %>%
  pivot_longer(!c(Year), names_to = "Age", values_to = "Weight")

#Finish tidying
WAA_nmfsspr2$Age <- sub("a", "", WAA_nmfsspr2$Age)
WAA_nmfsspr2$Age[which(WAA_nmfsspr2$Age == "10+")] = 10
WAA_nmfsspr2$Age <- as.numeric(WAA_nmfsspr2$Age)


#Plot all ages
WAA_nmfsspr2[is.na(WAA_nmfsspr2)] <- 0
WAA_nmfsspr2$Year <- as.numeric(WAA_nmfsspr2$Year)
ggplot(WAA_nmfsspr2, aes(Year, Weight)) +
  geom_line(colour="blue") +
  geom_point(colour = "blue") + 
  ylab("Weight (kg)")+
  theme_bw()

#Plot ages 2-7, faceted by age
WAA_nmfsspr2[WAA_nmfsspr2 == 0] <- NA
nmfsspring_waa_egb <- ggplot(subset(WAA_nmfsspr2, Age %in% c(2:7)), aes(Year, Weight)) +
  geom_line(colour="blue") +
  geom_point(colour = "blue") + 
  facet_grid(~Age) +
  ylab("Weight (kg)")+
  theme_bw() +
  ggtitle("NMFS Spring Weight at Age")

##NMFS Fall Survey-----------------------------

WAA_nmfsfall <- read.csv(here("data/Survey CAA/nmfsfall_survey_waa.csv"))

WAA_nmfsfall$'a10+' <- WAA_nmfsfall$a10 + WAA_nmfsfall$a11 + WAA_nmfsfall$a12 + WAA_nmfsfall$a13 + WAA_nmfsfall$a14 + WAA_nmfsfall$a15 + WAA_nmfsfall$a16

#Select columns and tidy data into long format
WAA_nmfsfall2 <- WAA_nmfsfall %>% dplyr::select(Year, a0, a1, a2, a3, a4, a5, a6, a7, a8, a9, 'a10+') %>%
  pivot_longer(!c(Year), names_to = "Age", values_to = "Weight")

#Finish tidying
WAA_nmfsfall2$Age <- sub("a", "", WAA_nmfsfall2$Age)
WAA_nmfsfall2$Age[which(WAA_nmfsfall2$Age == "10+")] = 10
WAA_nmfsfall2$Age <- as.numeric(WAA_nmfsfall2$Age)


#Plot all ages
WAA_nmfsfall2[is.na(WAA_nmfsfall2)] <- 0
WAA_nmfsfall2$Year <- as.numeric(WAA_nmfsfall2$Year)
ggplot(WAA_nmfsfall2, aes(Year, Weight)) +
  geom_line(colour="blue") +
  geom_point(colour = "blue") + 
  ylab("Weight (kg)")+
  theme_bw()

#Plot ages 2-7, faceted by age
WAA_nmfsfall2[WAA_nmfsfall2 == 0] <- NA
nmfsfall_waa_egb <- ggplot(subset(WAA_nmfsfall2, Age %in% c(2:7)), aes(Year, Weight)) +
  geom_line(colour="blue") +
  geom_point(colour = "blue") + 
  facet_grid(~Age) +
  ylab("Weight (kg)")+
  theme_bw()+
  ggtitle("NMFS Fall Weight at Age")

ggarrange(dfo_waa_egb, nmfsspring_waa_egb, nmfsspring_waa_egb, ncol=1, nrow=3)

ggsave(here("figures/Survey_WAA_EGB.png"), width=20, height=12, units="in")
