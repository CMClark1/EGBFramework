#############################
# 5Z Cod
# Survey Length at Age
# July 1, 2021
# Author: Caira Clark
#############################

require(ggplot2)
require(dplyr)
require(tidyr)
require(here)
require(ggpubr)

##DFO Spring Survey---------------

LAA_dfo <- read.csv(here("data/Survey CAA/dfo_survey_laa.csv"))

LAA_dfo$'a10+' <- LAA_dfo$a10 + LAA_dfo$a11 + LAA_dfo$a12 + LAA_dfo$a13 + LAA_dfo$a14 + LAA_dfo$a15 + LAA_dfo$a16

#Select columns and tidy data into long format
LAA_dfo2 <- LAA_dfo %>% dplyr::select(Year, a0, a1, a2, a3, a4, a5, a6, a7, a8, a9, 'a10+') %>%
  pivot_longer(!c(Year), names_to = "Age", values_to = "Length")

#Finish tidying
LAA_dfo2$Age <- sub("a", "", LAA_dfo2$Age)
LAA_dfo2$Age[which(LAA_dfo2$Age == "10+")] = 10
LAA_dfo2$Age <- as.numeric(LAA_dfo2$Age)
LAA_dfo2[LAA_dfo2 == 0] <- NA

#Plot ages 2-7, faceted by age
dfo_laa_egb <- ggplot(subset(LAA_dfo2, Age %in% c(2:7)), aes(Year, Length)) +
  geom_line(colour="blue") +
  geom_point(colour = "blue") + 
  facet_grid(~Age) +
  ylab("Length (cm)")+
  theme_bw() +
  ggtitle("DFO Length at Age") +
  xlim(1970, 2023)

##NMFS Spring Survey-----------------------------

LAA_nmfsspr <- read.csv(here("data/Survey CAA/nmfsspr_survey_laa.csv"))

LAA_nmfsspr$'a10+' <- LAA_nmfsspr$a10 + LAA_nmfsspr$a11 + LAA_nmfsspr$a12 + LAA_nmfsspr$a13 + LAA_nmfsspr$a14 + LAA_nmfsspr$a15 + LAA_nmfsspr$a16

#Select columns and tidy data into long format
LAA_nmfsspr2 <- LAA_nmfsspr %>% dplyr::select(Year, a0, a1, a2, a3, a4, a5, a6, a7, a8, a9, 'a10+') %>%
  pivot_longer(!c(Year), names_to = "Age", values_to = "Weight")

#Finish tidying
LAA_nmfsspr2$Age <- sub("a", "", LAA_nmfsspr2$Age)
LAA_nmfsspr2$Age[which(LAA_nmfsspr2$Age == "10+")] = 10
LAA_nmfsspr2$Age <- as.numeric(LAA_nmfsspr2$Age)


#Plot all ages
LAA_nmfsspr2[is.na(LAA_nmfsspr2)] <- 0
LAA_nmfsspr2$Year <- as.numeric(LAA_nmfsspr2$Year)
ggplot(LAA_nmfsspr2, aes(Year, Weight)) +
  geom_line(colour="blue") +
  geom_point(colour = "blue") + 
  ylab("Length (cm)")+
  theme_bw()

#Plot ages 2-7, faceted by age
LAA_nmfsspr2[LAA_nmfsspr2 == 0] <- NA
LAA_nmfsspr2$Weight[LAA_nmfsspr2$Weight == 0] <- NA

nmfsspring_laa_egb <- ggplot(subset(LAA_nmfsspr2, Age %in% c(2:7)), aes(Year, Weight)) +
  geom_line(colour="blue") +
  geom_point(colour = "blue") + 
  facet_grid(~Age) +
  ylab("Length (cm)")+
  theme_bw() +
  ggtitle("NMFS Spring Length at Age")

##NMFS Fall Survey-----------------------------

LAA_nmfsfall <- read.csv(here("data/Survey CAA/nmfsfall_survey_LAA.csv"))

LAA_nmfsfall$'a10+' <- LAA_nmfsfall$a10 + LAA_nmfsfall$a11 + LAA_nmfsfall$a12 + LAA_nmfsfall$a13 + LAA_nmfsfall$a14 + LAA_nmfsfall$a15 + LAA_nmfsfall$a16

#Select columns and tidy data into long format
LAA_nmfsfall2 <- LAA_nmfsfall %>% dplyr::select(Year, a0, a1, a2, a3, a4, a5, a6, a7, a8, a9, 'a10+') %>%
  pivot_longer(!c(Year), names_to = "Age", values_to = "Weight")

#Finish tidying
LAA_nmfsfall2$Age <- sub("a", "", LAA_nmfsfall2$Age)
LAA_nmfsfall2$Age[which(LAA_nmfsfall2$Age == "10+")] = 10
LAA_nmfsfall2$Age <- as.numeric(LAA_nmfsfall2$Age)


#Plot all ages
LAA_nmfsfall2[is.na(LAA_nmfsfall2)] <- 0
LAA_nmfsfall2$Year <- as.numeric(LAA_nmfsfall2$Year)

ggplot(LAA_nmfsfall2, aes(Year, Weight)) +
  geom_line(colour="blue") +
  geom_point(colour = "blue") + 
  ylab("Length (cm)")+
  theme_bw()

#Plot ages 2-7, faceted by age
LAA_nmfsfall2[LAA_nmfsfall2 == 0] <- NA

nmfsfall_laa_egb <- ggplot(subset(LAA_nmfsfall2, Age %in% c(2:7)), aes(Year, Weight)) +
  geom_line(colour="blue") +
  geom_point(colour = "blue") + 
  facet_grid(~Age) +
  ylab("Length (cm)")+
  theme_bw() +
  ggtitle("NMFS Fall Length at Age")

ggarrange(dfo_laa_egb, nmfsspring_laa_egb, nmfsfall_laa_egb, ncol=1, nrow=3)

ggsave(here("figures/Survey_LAA_EGB.png"), width=20, height=12, units="in")
