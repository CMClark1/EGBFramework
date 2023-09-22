#############################
# 5Z Cod
# RelF and Selectivity
# September 2023
# Author: Caira Clark
#############################

require(ggplot2)
require(dplyr)
require(tidyr)
require(here)
require(ggpubr)
library(viridis)

#Load the fishery CAA file
CAA_fishery <- read.csv(here("data/Survey CAA/fishery_combined_caa.csv"))
colnames(CAA_fishery)[1] <- "Year"

#Select columns and tidy data into long format
CAA_fishery2 <- CAA_fishery %>%
  tidyr::pivot_longer(!Year, names_to = "Age", values_to = "Abundance") 

#Finish tidying
CAA_fishery2$Age <- sub("a", "", CAA_fishery2$Age)
CAA_fishery2$Age <- as.numeric(CAA_fishery2$Age)
CAA_fishery2$Abundance <- CAA_fishery2$Abundance*1000

##DFO Spring Survey EGB + GB ----------------

#Load the DFO CAA files
CAA_dfoEGB <- read.csv(here("data/Survey CAA/dfo_survey_caa.csv"))
colnames(CAA_dfoEGB)[1] <- "Year"
CAA_dfoEGB$Survey <- "DFO EGB"

CAA_dfoWGB <- read.csv(here('data/Survey CAA/dfo_survey_caa_WGB.csv'))
colnames(CAA_dfoWGB)[1] <- "Year"
CAA_dfoWGB$Survey <- "DFO WGB"

CAA_dfo <- rbind(CAA_dfoEGB, CAA_dfoWGB)

#Select columns and tidy data into long format
CAA_dfo2 <- CAA_dfo %>%
  pivot_longer(!c(Survey, Year), names_to = "Age", values_to = "Abundance") %>%
  pivot_wider(c(Year, Age), names_from="Survey", values_from = "Abundance")
CAA_dfo2[is.na(CAA_dfo2)] <- 0
CAA_dfo2$'DFO GB' <- CAA_dfo2$`DFO EGB`+CAA_dfo2$`DFO WGB`
CAA_dfo2 <- CAA_dfo2 %>% pivot_longer(!c(Year, Age), names_to="Survey", values_to="Abundance") %>% select(Survey, Year, Age, Abundance)

#Finish tidying
CAA_dfo2$Age <- sub("a", "", CAA_dfo2$Age)
CAA_dfo2$Age[which(CAA_dfo2$Age == "10+")] = 10
CAA_dfo2$Age <- as.numeric(CAA_dfo2$Age)

#NMFS EGB ------------- 

#Load the NMFS CAA files and add 10+ groups
CAA_nmfsspr <- read.csv(here("data/Survey CAA/nmfsspr_survey_caa.csv"))
CAA_nmfsspr$SURVEY <- "NMFS SPRING EGB"

CAA_nmfsfall <- read.csv(here("data/Survey CAA/nmfsfall_survey_caa.csv")) #this stranal file should have the conversion factor selected.
CAA_nmfsfall$SURVEY <- "NMFS FALL EGB"

CAA_nmfsEGB <- rbind(CAA_nmfsspr, CAA_nmfsfall)

#Load the NMFS conversion file and calculate spring conversion factors
nmfsconv <- read.csv(here("data/Survey CAA/nmfs_conversionfactors.csv"))
nmfsconv$SpringConversion <- nmfsconv$SpringDoorEffect*nmfsconv$SpringVesselEffect
nmfsconv$FallConversion <- nmfsconv$FallDoorEffect*nmfsconv$FallVesselEffect

#Pivot dataframes so that they can be joined easily, and join them
CAA_nmfsEGB <- CAA_nmfsEGB %>% pivot_longer(!c(SURVEY, Year), names_to="Age", values_to="AbundanceUnconv")
nmfsconv <- nmfsconv %>% select(Year, SpringConversion, FallConversion) %>% pivot_longer(!c(Year), names_to="Season", values_to="Conversion") %>% mutate(SURVEY=case_when(Season=="SpringConversion" ~ "NMFS SPRING EGB", Season=="FallConversion" ~ "NMFS FALL EGB"))
CAA_nmfsEGB <- left_join(CAA_nmfsEGB, nmfsconv)
CAA_nmfsEGB$Abundance <- CAA_nmfsEGB$AbundanceUnconv*CAA_nmfsEGB$Conversion

#Finish tidying
CAA_nmfsEGB[is.na(CAA_nmfsEGB)] <- 0
CAA_nmfsEGB$Age <- sub("a", "", CAA_nmfsEGB$Age)
CAA_nmfsEGB$Age <- as.numeric(CAA_nmfsEGB$Age)
colnames(CAA_nmfsEGB)[2] <- "Survey"

CAA_nmfsEGB <- CAA_nmfsEGB %>% select(Survey, Year, Age, Abundance)

#NMFS SURVEY GB--------------

gbdata <- read.csv(here("data/NMFS.GB/NumAge_Data.csv"))
gbdata <- gbdata %>% filter(INDEX_NAME%in%c("NEFSC BTS_GBK_FALL", "NEFSC BTS_GBK_SPRING"))

CAA_nmfs <- gbdata %>% 
  select(INDEX_NAME, YEAR, Age.0, Age.1, Age.2, Age.3, Age.4, Age.5, Age.6, Age.7, Age.8, Age.9.) %>% 
  mutate(Age.10=0, Age.11=0, Age.12=0, Age.13=0, Age.14=0, Age.15=0, Age.16=0) %>%
  pivot_longer(!c(INDEX_NAME, YEAR), names_to="Age", values_to="Abundance") %>% 
  mutate(INDEX_NAME=case_when(INDEX_NAME=="NEFSC BTS_GBK_FALL" ~ "NMFS FALL GB", 
                              INDEX_NAME=="NEFSC BTS_GBK_SPRING" ~ "NMFS SPRING GB"))
CAA_nmfs$Age <- as.double(substr(CAA_nmfs$Age, 5, 5))
colnames(CAA_nmfs)[1] <- "Survey"
colnames(CAA_nmfs)[2] <- "Year"

#Join all the survey data together
CAA_surveys <- rbind(CAA_dfo2, CAA_nmfsEGB, CAA_nmfs)

#Join fishery CAA to survey CAA

CAA_all <- CAA_surveys %>% 
  mutate('SurveyAbundance' = Abundance) %>% 
  select(-Abundance) %>% group_by(Year, Survey, Age) %>% 
  left_join(CAA_fishery2%>%mutate('FisheryAbundance' = Abundance) %>% 
              select(-Abundance))
CAA_all[is.na(CAA_all) | CAA_all == Inf | CAA_all == -Inf] <- NA

#Calculate Relative F
CAA_all$RelF <- CAA_all$FisheryAbundance/CAA_all$SurveyAbundance
CAA_all[is.na(CAA_all) | CAA_all == Inf | CAA_all == -Inf] <- NA

CAA_all <- CAA_all %>% mutate(DECADE=case_when(
  Year <= 1979            ~ "1970-1979",
  Year > 1979 & Year <= 1989 ~ "1980-1989",
  Year > 1989 & Year <= 1999 ~ "1990-1999",
  Year > 1999 & Year <= 2009 ~ "2000-2009",
  Year > 2009 & Year <= 2019 ~ "2010-2019",
  Year > 2019  ~ "2020+"))

ggplot(CAA_all) +
  geom_path(aes(x=Age, y=RelF, colour=Year)) +
  facet_grid(DECADE~Survey, scales="free_y")+
  theme_bw()+
  scale_colour_viridis() +
  ggtitle("Relative F")


#Calculate selectivity

maxab <- CAA_surveys %>% group_by(Survey, Year) %>% summarise(max=max(Abundance))
CAA_surveys2 <- left_join(CAA_surveys, maxab)
CAA_surveys2[is.na(CAA_surveys2) | CAA_surveys2 == Inf | CAA_surveys2 == -Inf | CAA_surveys2 == 0] <- NA

CAA_surveys2$Selectivity <- CAA_surveys2$Abundance/CAA_surveys2$max
CAA_surveys2[is.na(CAA_surveys2) | CAA_surveys2 == Inf | CAA_surveys2 == -Inf] <- NA
  
CAA_surveys2 <- CAA_surveys2 %>% mutate(DECADE=case_when(
  Year <= 1979            ~ "1970-1979",
  Year > 1979 & Year <= 1989 ~ "1980-1989",
  Year > 1989 & Year <= 1999 ~ "1990-1999",
  Year > 1999 & Year <= 2009 ~ "2000-2009",
  Year > 2009 & Year <= 2019 ~ "2010-2019",
  Year > 2019  ~ "2020+"))

ggplot(CAA_surveys2) +
  geom_line(aes(x=Age, y=Selectivity, colour=Year, group=Year)) +
  facet_grid(DECADE~Survey, scales="free_y")+
  theme_bw()+
  scale_colour_viridis() +
  ggtitle("Survey Selectivity")
