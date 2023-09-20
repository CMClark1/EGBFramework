#############################
# 5Z Cod
# Survey Catch at Age
# July 1, 2021
# Author: Caira Clark
#############################

require(ggplot2)
require(dplyr)
require(tidyr)
require(here)
require(ggpubr)

##DFO Spring Survey EGB + GB ----------------

#Load the DFO CAA files
CAA_dfoEGB <- read.csv(here("data/Survey CAA/dfo_survey_caa.csv"))
colnames(CAA_dfoEGB)[1] <- "Year"
CAA_dfoEGB$Survey <- "DFO EGB"

CAA_dfoWGB <- read.csv(here('data/Survey CAA/dfo_survey_caa_WGB.csv'))
colnames(CAA_dfoWGB)[1] <- "Year"
CAA_dfoWGB$Survey <- "DFO WGB"

CAA_dfo <- rbind(CAA_dfoEGB, CAA_dfoWGB)

#Calculate 10+ group
CAA_dfo$'a10+' <- CAA_dfo$a10 + CAA_dfo$a11 + CAA_dfo$a12 + CAA_dfo$a13 + CAA_dfo$a14 + CAA_dfo$a15 + CAA_dfo$a16

#Select columns and tidy data into long format
CAA_dfo2 <- CAA_dfo %>% dplyr::select(Year, Survey, a0, a1, a2, a3, a4, a5, a6, a7, a8, a9, 'a10+') %>%
  pivot_longer(!c(Survey, Year), names_to = "Age", values_to = "Abundance") %>%
  pivot_wider(c(Year, Age), names_from="Survey", values_from = "Abundance")
CAA_dfo2[is.na(CAA_dfo2)] <- 0
CAA_dfo2$'DFO GB' <- CAA_dfo2$`DFO EGB`+CAA_dfo2$`DFO WGB`
CAA_dfo2 <- CAA_dfo2 %>% pivot_longer(!c(Year, Age), names_to="Survey", values_to="Abundance")

#Finish tidying
CAA_dfo2$Age <- sub("a", "", CAA_dfo2$Age)
CAA_dfo2$Age[which(CAA_dfo2$Age == "10+")] = 10
CAA_dfo2$Age <- as.numeric(CAA_dfo2$Age)

#Calculate the total abundance for each year, and then use that to calculate proportion
CAA_dfo2 <- CAA_dfo2 %>% group_by(Year) %>% mutate(Year_Total = sum(Abundance))
CAA_dfo2$Proportion <- CAA_dfo2$Abundance/CAA_dfo2$Year_Total

#Take out zeros for the plots
CAA_dfo2[CAA_dfo2 == 0] <- NA

#Abundance Plot
caa_ab_dfo <- ggplot(CAA_dfo2%>%filter(Survey%in%c("DFO EGB", "DFO GB")), aes(x=Age, y=Year, size = Abundance)) +
  geom_point(aes(fill= "#00AFBB"), alpha=0.5, colour = "black", pch=21) +
  scale_y_reverse(breaks = scales::pretty_breaks (n=7)) + 
  scale_size(range = c(0.1, 16), name="Abundance (000s)")+
  scale_x_continuous (breaks = scales::pretty_breaks (n=12)) + 
  theme(legend.position = "none") +
  theme (axis.title = element_text (size = 10, colour = "black"), 
         axis.text = element_text (size = 8, colour = "black"), 
         legend.text = element_text (size = 8)) +
  theme(axis.line = element_line(color="black", size = 0.5), 
        panel.background = element_blank(), 
        legend.key=element_blank())+
  facet_wrap(Survey~., scale="free_y") +
  lims(y=c(2023, 1980)) + 
  #lims(y=c(2023, 1970)) + 
  ggtitle("DFO Survey Catch at Age - Abundance")

#Proportion Plot

caa_prop_dfo <- ggplot(CAA_dfo2%>%filter(Survey%in%c("DFO EGB", "DFO GB")), aes(x=Age, y=Year, size = Proportion)) +
  geom_point(fill= "#00AFBB", alpha=0.5, colour = "black", pch=21) +
  scale_y_reverse(breaks = scales::pretty_breaks (n=6)) + scale_size(range = c(0.1, 16), name="Abundance (000s)")+
  scale_x_continuous (breaks = scales::pretty_breaks (n=12)) + theme(legend.position = "none") +
  theme (axis.title = element_text (size = 10, colour = "black"), axis.text = element_text (size = 8, colour = "black"), legend.text = element_text (size = 8)) +
  theme(axis.line = element_line(color="black", size = 0.5), panel.background = element_blank(), legend.key=element_blank())+
  facet_wrap(Survey~., scale="free_y") +
  lims(y=c(2023, 1980)) + 
  ggtitle("DFO Survey EGB Catch at Age - Proportion")

ggarrange(caa_ab_dfo, caa_prop_dfo, ncol=1, nrow=2)

ggsave(here("figures/Survey_DFO_CAA.png"), width=10, height=12, units="in")

#NMFS EGB ------------- 

#Load the NMFS CAA files and add 10+ groups
CAA_nmfsspr <- read.csv(here("data/Survey CAA/nmfsspr_survey_caa.csv"))
CAA_nmfsspr$SURVEY <- "NMFS SPRING"
CAA_nmfsspr$'a10+' <- CAA_nmfsspr$a10 + CAA_nmfsspr$a11 + CAA_nmfsspr$a12 + CAA_nmfsspr$a13 + CAA_nmfsspr$a14 + CAA_nmfsspr$a15 + CAA_nmfsspr$a16
CAA_nmfsspr <- CAA_nmfsspr %>% select(!c(a10, a11, a12, a13, a14, a15, a16))

CAA_nmfsfall <- read.csv(here("data/Survey CAA/nmfsfall_survey_caa.csv")) #this stranal file should have the conversion factor selected.
CAA_nmfsfall$SURVEY <- "NMFS FALL"
CAA_nmfsfall$'a10+' <- CAA_nmfsfall$a10 + CAA_nmfsfall$a11 + CAA_nmfsfall$a12 + CAA_nmfsfall$a13 + CAA_nmfsfall$a14 + CAA_nmfsfall$a15 + CAA_nmfsfall$a16
CAA_nmfsfall <- CAA_nmfsfall %>% select(!c(a10, a11, a12, a13, a14, a15, a16))

CAA_nmfsEGB <- rbind(CAA_nmfsspr, CAA_nmfsfall)

#Load the NMFS conversion file and calculate spring conversion factors
nmfsconv <- read.csv(here("data/Survey CAA/nmfs_conversionfactors.csv"))
nmfsconv$SpringConversion <- nmfsconv$SpringDoorEffect*nmfsconv$SpringVesselEffect
nmfsconv$FallConversion <- nmfsconv$FallDoorEffect*nmfsconv$FallVesselEffect

#Pivot dataframes so that they can be joined easily, and join them
CAA_nmfsEGB <- CAA_nmfsEGB %>% pivot_longer(!c(SURVEY, Year), names_to="Age", values_to="AbundanceUnconv")
nmfsconv <- nmfsconv %>% select(Year, SpringConversion, FallConversion) %>% pivot_longer(!c(Year), names_to="Season", values_to="Conversion") %>% mutate(SURVEY=case_when(Season=="SpringConversion" ~ "NMFS SPRING", Season=="FallConversion" ~ "NMFS FALL"))
CAA_nmfsEGB <- left_join(CAA_nmfsEGB, nmfsconv)
CAA_nmfsEGB$Abundance <- CAA_nmfsEGB$AbundanceUnconv*CAA_nmfsEGB$Conversion

#Finish tidying
CAA_nmfsEGB[is.na(CAA_nmfsEGB)] <- 0
CAA_nmfsEGB$Age <- sub("a", "", CAA_nmfsEGB$Age)
CAA_nmfsEGB$Age[which(CAA_nmfsEGB$Age == "10+")] = 10
CAA_nmfsEGB$Age <- as.numeric(CAA_nmfsEGB$Age)

#Calculate the total abundance for each year, and then use that to calculate proportion
CAA_nmfsEGB<- CAA_nmfsEGB %>% group_by(Year) %>% mutate(Year_Total = sum(Abundance))
CAA_nmfsEGB$Proportion <- CAA_nmfsEGB$Abundance/CAA_nmfsEGB$Year_Total

#Take out zeros for the plots
CAA_nmfsEGB[CAA_nmfsEGB == 0] <- NA

#Abundance Plot
caa_nmfs_egb_ab <- ggplot(CAA_nmfsEGB, aes(x=Age, y=Year, size = Abundance)) +
  geom_point(aes(fill= "#00AFBB"), alpha=0.5, colour = "black", pch=21) +
  scale_y_reverse(breaks = scales::pretty_breaks (n=6)) + scale_size(range = c(0.1, 16), name="Abundance (000s)")+
  scale_x_continuous (breaks = scales::pretty_breaks (n=12)) + theme(legend.position = "none") +
  theme (axis.title = element_text (size = 10, colour = "black"), axis.text = element_text (size = 8, colour = "black"), legend.text = element_text (size = 8)) +
  theme(axis.line = element_line(color="black", size = 0.5), panel.background = element_blank(), legend.key=element_blank())+
  lims(y=c(2023, 1970)) +
  facet_wrap(SURVEY~., scales="free_y") +
  ggtitle("NMFS EGB Catch at Age - Abundance")

#Proportion Plot

caa_nmfs_egb_prop <- ggplot(CAA_nmfsEGB, aes(x=Age, y=Year, size = Proportion)) +
  geom_point(fill= "#00AFBB", alpha=0.5, colour = "black", pch=21) +
  scale_y_reverse(breaks = scales::pretty_breaks (n=6)) + scale_size(range = c(0.1, 16), name="Abundance (000s)")+
  scale_x_continuous (breaks = scales::pretty_breaks (n=12)) + theme(legend.position = "none") +
  theme (axis.title = element_text (size = 10, colour = "black"), axis.text = element_text (size = 8, colour = "black"), legend.text = element_text (size = 8)) +
  theme(axis.line = element_line(color="black", size = 0.5), panel.background = element_blank(), legend.key=element_blank())+
  lims(y=c(2023, 1970)) + 
  facet_wrap(SURVEY~., scales="free_y") +
  ggtitle("NMFS EGB Catch at Age - Proportion")

ggarrange(caa_nmfs_egb_ab, caa_nmfs_egb_prop, ncol=1, nrow=2)

ggsave(here("figures/Survey_NMFS_CAA_EGB.png"), width=10, height=12, units="in")

#NMFS SURVEY GB--------------

gbdata <- read.csv(here("data/NMFS.GB/NumAge_Data.csv"))
gbdata <- gbdata %>% filter(INDEX_NAME%in%c("NEFSC BTS_GBK_FALL", "NEFSC BTS_GBK_SPRING"))

CAA_nmfs <- gbdata %>% 
  select(INDEX_NAME, YEAR, Age.0, Age.1, Age.2, Age.3, Age.4, Age.5, Age.6, Age.7, Age.8, Age.9.) %>% 
  pivot_longer(!c(INDEX_NAME, YEAR), names_to="Age", values_to="Abundance") %>% 
  mutate(INDEX_NAME=case_when(INDEX_NAME=="NEFSC BTS_GBK_FALL" ~ "NMFS FALL", 
                              INDEX_NAME=="NEFSC BTS_GBK_SPRING" ~ "NMFS SPRING"))
CAA_nmfs$Age <- as.double(substr(CAA_nmfs$Age, 5, 5))

#Calculate proportion
Year_Total <- CAA_nmfs %>% group_by(INDEX_NAME, YEAR) %>% summarize(Year_Total=sum(Abundance))
CAA_nmfs <- left_join(CAA_nmfs, Year_Total)
CAA_nmfs$Proportion <- CAA_nmfs$Abundance/CAA_nmfs$Year_Total

#Abundance Plot

caa_gb_nmfs_ab <- ggplot(CAA_nmfs, aes(x=Age, y=YEAR, size = Abundance)) +
  geom_point(aes(fill= "#00AFBB"), alpha=0.5, colour = "black", pch=21) +
  scale_y_reverse(breaks = scales::pretty_breaks (n=6)) + scale_size(range = c(0.1, 16), name="Abundance (000s)")+
  scale_x_continuous (breaks = scales::pretty_breaks (n=12)) + theme(legend.position = "none") +
  theme (axis.title = element_text (size = 10, colour = "black"), axis.text = element_text (size = 8, colour = "black"), legend.text = element_text (size = 8)) +
  theme(axis.line = element_line(color="black", size = 0.5), panel.background = element_blank(), legend.key=element_blank())+
  lims(y=c(2024, 1970)) + 
  facet_wrap(INDEX_NAME~., scale="free_y") +
  ggtitle("NMFS GB Catch at Age - Abundance")

#Proportion Plot

caa_gb_nmfs_prop <- ggplot(CAA_nmfs, aes(x=Age, y=YEAR, size = Proportion)) +
    geom_point(aes(fill= "#bb0c00"), fill = "#00afbb", alpha=0.5, colour = "black", pch=21) +
  scale_y_reverse(breaks = scales::pretty_breaks (n=6)) + scale_size(range = c(0.1, 16), name="Abundance (000s)")+
  scale_x_continuous (breaks = scales::pretty_breaks (n=12)) + theme(legend.position = "none") +
  theme (axis.title = element_text (size = 10, colour = "black"), axis.text = element_text (size = 8, colour = "black"), legend.text = element_text (size = 8)) +
  theme(axis.line = element_line(color="black", size = 0.5), panel.background = element_blank(), legend.key=element_blank())+
  lims(y=c(2024, 1970)) + 
  facet_wrap(INDEX_NAME~., scale="free_y") +
  ggtitle("NMFS GB Catch at Age - Proportion")

ggarrange(caa_gb_nmfs_ab, caa_gb_nmfs_prop, ncol=1, nrow=2)

ggsave(here("figures/Survey_NMFS_CAA_GB.png"), width=10, height=12, units="in")
