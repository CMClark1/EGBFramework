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
require(viridis)

#WAA DFO EGB vs WGB

WAA_EGB <- read.csv(here("data/WAA/WAA_EGB.csv"))
colnames(WAA_EGB)[1] <- "YEAR"
WAA_EGB <- WAA_EGB %>% select(1:18)
WAA_EGB$SURVEY <- "DFO EGB"

WAA_WGB <- read.csv(here("data/WAA/WAA_WGB.csv"))
colnames(WAA_WGB)[1] <- "YEAR"
WAA_WGB <- WAA_WGB %>% select(1:18)
WAA_WGB$SURVEY <- "DFO WGB"

WAA <- rbind(WAA_EGB, WAA_WGB)

tidy <- WAA %>%
  pivot_longer(!c(SURVEY, YEAR), names_to="Age", values_to="Weight") %>% 
  filter(!is.na(Age))

library(stringr)
tidy$Age <- str_sub(tidy$Age,2,3)

WAAplot <- ggplot(subset(tidy, Age %in% c(2:7)), aes(YEAR, Weight)) +
  geom_line(colour="blue") +
  geom_point(colour = "blue") + 
  facet_grid(SURVEY~Age) +
  ylab("Weight (kg)")+
  theme_bw() +
  ggtitle("DFO Weight at Age") +
  xlim(1970, 2023)
WAAplot



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

#Plot ages 2-7, faceted by age
WAA_nmfsspr2[WAA_nmfsspr2 == 0] <- NA
nmfsspring_waa_egb <- ggplot(subset(WAA_nmfsspr2, Age %in% c(2:7)), aes(Year, Weight)) +
  geom_line(colour="blue") +
  geom_point(colour = "blue") + 
  facet_grid(~Age) +
  ylab("Weight (kg)")+
  theme_bw() +
  ggtitle("NMFS Spring EGB Weight at Age")
nmfsspring_waa_egb

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

#Plot ages 2-7, faceted by age
WAA_nmfsfall2[WAA_nmfsfall2 == 0] <- NA
nmfsfall_waa_egb <- ggplot(subset(WAA_nmfsfall2, Age %in% c(2:7)), aes(Year, Weight)) +
  geom_line(colour="blue") +
  geom_point(colour = "blue") + 
  facet_grid(~Age) +
  ylab("Weight (kg)")+
  theme_bw()+
  ggtitle("NMFS Fall Weight at Age")
nmfsfall_waa_egb

ggarrange(WAAplot, nmfsspring_waa_egb, nmfsfall_waa_egb, ncol=1, nrow=3)
ggsave(here("figures/Survey_WAA.png"), width=15, height=12, units="in")

#Plot all together

WAA_nmfsspr2$SURVEY <- "NMFS Spring"
colnames(WAA_nmfsspr2)[1] <- "YEAR"
WAA_nmfsfall2$SURVEY <- "NMFS Fall"
colnames(WAA_nmfsfall2)[1] <- "YEAR"
tidy <- tidy %>% select(YEAR, Age, Weight, SURVEY)
allsurveys <- rbind(WAA_nmfsspr2, WAA_nmfsfall2, tidy)

allsurveys[allsurveys == 0] <- NA
ggplot(subset(allsurveys, Age %in% c(2:7)), aes(YEAR, Weight, colour=SURVEY)) +
  geom_line() +
  geom_point() + 
  scale_color_viridis_d(option="turbo", end=0.8)  +
  facet_wrap(.~Age, scales="free") +
  ylab("Weight (kg)")+
  theme_bw()+
  ggtitle("Weight at Age")
ggsave(here("figures/Survey_WAA_all.png"), width=15, height=12, units="in")


